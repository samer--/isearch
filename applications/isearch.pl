:- module(app_isearch, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).

:- use_module(components(label)).

:- multifile
	cliopatria:facet_exclude_property/1,		% ?Resource
	cliopatria:format_search_result/4,
	cliopatria:search_pattern/6.

:- rdf_meta
	facet_exclude_property(r),
       	cliopatria:facet_exclude_property(r).

% declare application settings
%
% Do not change these here. Instead use :- set_setting(type, value) in
% your startup file.

:- setting(search:target_class, uri, rdfs:'Resource',
	   'Default search target').

% interactive search components
:- setting(search:show_disambiguations, boolean, true,
	   'Show terms matching the query as disambiguation suggestions').
:- setting(search:show_suggestions, boolean, false,
	   'Show terms as suggestions for further queries').
:- setting(search:show_relations, boolean, true,
	   'Show relations by which search results are found').
:- setting(search:show_facets, boolean, true,
	   'Show faceted filters in the search result page').
:- setting(search:show_single_value_facet, boolean, false,
	   'Show facets with a single value').

% limits
:- setting(search:result_limit, integer, 10,
	  'Maximum number of results shown').
:- setting(search:term_limit, integer, 5,
	  'Maximum number of items shown in the term disambiguation list').
:- setting(search:relation_limit, integer, 5,
	  'Maximum number of relations shown').

% appearence
:- setting(search:logo, atom, '',
	   'Img shown as a logo on the page').

:- http_handler(root(isearch), http_interactive_search, [id(isearch)]).

%%	http_interactive_search(+Request)
%
%	HTTP handler for search requests.

http_interactive_search(Request) :-
	setting(search:target_class, TargetClass),
	setting(search:result_limit, DefaultLimit),
	http_parameters(Request,
			[ q(Keyword,
				[optional(true), description('Search query')]),
			  class(Class,
				[default(TargetClass), description('Target Class')]),
			  term(Terms,
			       [zero_or_more,
				description('Disambiguation term')]),
			  relation(Relations,
				   [zero_or_more,
				    description('Limit results by specific relation')]),
 			  filter(Filter,
				 [default([]), json,
				  description('Filters on the result set')]),
			  offset(Offset,
				[default(0), integer, description('Offset of the result list')]),
			  limit(Limit,
				[default(DefaultLimit), integer,
				 description('Limit on the number of results')])
			]),
	(   var(Keyword)
	->  html_start_page(Class)
	;   keyword_search_results(Keyword, Class, Results),
	    result_terms(Results, MatchingTerms, ResultsByTerm),
	    related_terms(Terms, Class, RelatedTerms),
	    select_results_by_key(Terms, ResultsByTerm, Results, TermResults),
	    filter_results_by_facet(TermResults, Filter, FilteredResults),
	    result_relations(FilteredResults, MatchingRelations, ResultsByRelation),
	    select_results_by_key(Relations, ResultsByRelation, FilteredResults, RelationResults),

	    result_uris(TermResults, TermResultURIs),
	    result_uris(FilteredResults, FilteredURIs),
	    facets(FilteredURIs, TermResultURIs, Filter, Facets),

	    results_by_uri(RelationResults, ResultPairs),
	    length(ResultPairs, NumberOfResults),
	    length(FilteredURIs, NumberOfRelationResults),

	    list_offset(ResultPairs, Offset, OffsetResults),
	    list_limit(OffsetResults, Limit, LimitedResults, _),

 	    html_result_page(query(Keyword, Class, Terms, Relations, Filter, Offset, Limit),
			     result(LimitedResults, NumberOfResults, NumberOfRelationResults),
			     MatchingTerms, RelatedTerms,
			     MatchingRelations, Facets)
  	).

% conversion of json parameters.

:- json_object
     prop(prop:atom, values:_),
     literal(literal:atom),
     literal(literal:_),
     type(type:atom, text:atom),
     lang(lang:atom, text:atom).

%%	http:convert_parameter(+Type, +Text, -Value) is semidet.
%
%	Convert for Type = =json= using json_to_prolog/2.

http:convert_parameter(json, Atom, Term) :-
	atom_json_term(Atom, JSON, []),
	json_to_prolog(JSON, Term).

%%	keyword_search_results(+Keyword, +Class, -Results)
%
%	Results are resources related to Keyword and of type Class.

keyword_search_results('', _, []) :- !.
keyword_search_results(Query, Class, Results) :-
	R = result(S, Lit, T, Rel, Path),
	findall(R, search_result(Query, Class, S, Lit, T, Rel, Path), Results).

search_result(Query, Class, S, _Lit, Query, P, [P,Query]) :-
	rdf(_,_,Query), !,
	rdf(S, P, Query),
	instance_of_class(Class, S).
search_result(Query, Class, S, Lit, Term, Rel, Path) :-
	rdf_find_literals(case(Query), Literals),
	member(Lit, Literals),
	search_pattern(Lit, Class, S, Rel, Term, Path).

search_pattern(Lit, Class, S, P, _Term, Path) :-
	Path = [P, literal(Lit)],
        rdf(S, P, literal(Lit)),
	instance_of_class(Class, S).
search_pattern(Lit, Class, S, Rel, Term, Path) :-
	rdf_equal(LabelP, rdfs:label),
	Path = [Rel, Term, P, literal(Lit)],
	rdf_has(Term, LabelP, literal(Lit), P),
	rdf(S, Rel, Term),
	instance_of_class(Class, S).
search_pattern(Lit, Class, S, P, Term, Path) :-
	cliopatria:search_pattern(Lit, Class, S, P, Term, Path).


%%	result_terms(+SearchResults, -Terms, -ResultsByTerms)
%
%	Terms are all resources in the search paths directly related to
%	the results. ResultsByTerms groups the results by these
%	terms.

result_terms([], [], []) :- !.
result_terms(_, [], []) :-
	setting(search:show_disambiguations, false),
	!.
result_terms(Results, Terms, ResultsByTerm) :-
	results_by_term(Results, ResultsByTerm),
	pairs_sort_by_result_count(ResultsByTerm, Terms).

results_by_term(Results, ResultsByTerm) :-
	Result = result(_, _, T, _, _),
	findall(T-Result, (member(Result, Results),
		      nonvar(T)
		     ),
		TermResults0),
	keysort(TermResults0, TermResults),
	group_pairs_by_key(TermResults, ResultsByTerm).


%%	related_terms(+ResultTerms, +Class, -RelatedTerms)
%
%	RelatedTerms are all resources related to ResultTerms and
%	used as metadata for resources of type Class.

related_terms([], _, []) :- !.
related_terms(_, _, []) :-
	setting(search:show_suggestions, false),
	!.
related_terms(Terms, Class, RelatedTerms) :-
	findall(P-RT, ( member(Term, Terms),
			related_term(Term, Class, RT, P)
		      ),
		RTs0),
	sort(RTs0, RTs),
	group_pairs_by_key(RTs, RelatedTerms).

related_term(R, Class, Term, P) :-
	related(R, Term, P),
	atom(Term),
	\+ equivalent_property(P),
	has_target(Term, Class).

has_target(Term, Class) :-
	rdf(Target, _, Term),
	instance_of_class(Class, Target).

related(S, O, P) :-
	rdf_eq(S, P0, V),
	(   O = V,
	    P = P0
	;   atom(V),
	    rdf_predicate_property(P0, rdf_object_branch_factor(BF)),
	    debug(related, '~w ~w', [P0, BF]),
	    BF < 10
	->  rdf_eq(O, P0, V),
	    O \== S,
	    P = V
	).
related(S, O, P) :-
	rdf_eq(O, P, S),
	rdf(P, owl:inverseOf, IP),
	\+ rdf_eq(S, IP, O).


%%	result_relations(+SearchResults, -Relations, -ResultsByRelation)
%
%	Relations are all predicates in the search paths direclty
%	related to the results. ResultsByRelations groups the
%	results by these relations.

result_relations([], [], []) :- !.
result_relations(_, [], []) :-
	setting(search:show_relations, false),
	!.
result_relations(Results, Relations, ResultsByRelation) :-
	results_by_relation(Results, ResultsByRelation),
	pairs_sort_by_result_count(ResultsByRelation, Relations).

results_by_relation(Results, ResultsByRelation) :-
	Result = result(_, _, _, R, _),
	findall(R-Result, member(Result, Results), RelationResults0),
	keysort(RelationResults0, RelationResults),
	group_pairs_by_key(RelationResults, ResultsByRelation).


%%	filter_results_by_facet(+Rs, +Filter, -Filtered)
%
%	Filtered contains the resources from Rs that pass Filter.

filter_results_by_facet(Rs, [], Rs) :- !.
filter_results_by_facet(Rs, Filter, FilteredResults) :-
	filter_to_goal(Filter, R, Goal),
	Result = result(R,_,_,_,_),
	findall(Result, (member(Result, Rs),
			 call(Goal)
			),
		FilteredResults).

filter_to_goal([], _, true).
filter_to_goal([prop(P, Values)|T], R, (Goal,Rest)) :-
	pred_filter(Values, P, R, Goal),
	filter_to_goal(T, R, Rest).

pred_filter([Value], P, R, Goal) :- !,
	Goal = rdf_has(R, P, Value).
pred_filter([Value|Vs], P, R, Goal) :-
	Goal =  (rdf_has(R, P, Value); Rest),
	pred_filter(Vs, P, R, Rest).


%%	select_results_by_key(+Keys, +SearchResultsByKey,
%%	+SearchResults, -SelectedResults)
%
%	SelectedResults contains the results grouped by Keys.

select_results_by_key([], _, Results, Results) :- !.
select_results_by_key(Keys, ResultsByKey, _, SelectedResults) :-
	findall(R,
		(	member(Key, Keys),
			memberchk(Key-R, ResultsByKey)
		),
		Rs),
	flatten(Rs, SelectedResults).

%%	results_by_uri(+Results, -ResultsGroupedByURI)
%
%	Group the result terms by unique URIs.

results_by_uri(Results, ResultsByURI) :-
	Result = result(S, _, _, _, _),
	findall(S-Result, member(Result, Results), URIResults0),
	keysort(URIResults0, URIResults),
	group_pairs_by_key(URIResults, ResultsByURI).

%%	result_uris(ResultObjects, -URIs)
%
%	URIs are the uris of the results in ResultObjects.

result_uris(Results, URIs) :-
	findall(S, member(result(S, _,_,_,_), Results), URIs0),
	sort(URIs0, URIs).


%%	facets(+Results, -Facets)
%
%	Collect faceted properties of Results.

facets([], _, _, []) :- !.
facets(_, _, _, []) :-
	setting(search:show_facets, false),
	!.
facets(FilteredResults, AllResults, Filter, Facets) :-
 	findall(facet(P, Values, []),
	      (	  facet_values(P, FilteredResults, Values),
 		  \+ memberchk(prop(P,_), Filter),
		  \+ facet_exclude_property(P)
	      ),
	      Facets0),
	findall(facet(P, Values, Selected),
		(   select(prop(P, Selected), Filter, FilterRest),
		    filter_to_goal(FilterRest, R, Goal),
		    facet_values(P, AllResults, Goal, R, Values)
		),
		ActiveFacets),
 	append(ActiveFacets, Facets0, Facets).

facet_values(P, Results, ResultsByValue) :-
	bagof(V-Rs,
	      setof(R,
		    ( member(R, Results),
		      facet_property(R, P, V)
  		    ),
		    Rs
		   ),
	      ResultsByValue).
facet_values(P, Results, Goal, R, ResultsByValue) :-
	bagof(V-Rs,
	      setof(R,
		    ( member(R, Results),
		      once(Goal),
		      rdf_has(R, P, V)
		    ),
		    Rs
		   ),
	      ResultsByValue).

facet_property(S, P, V) :-
	rdf(S, P0, V),
	super_property(P0, P).

super_property(P0, Super) :-
	findall(P, ( rdf_reachable(P0, rdfs:subPropertyOf, P),
		      \+ rdf(P, rdfs:subPropertyOf, _)
		    ),Ps0),
	sort(Ps0, Ps),
	member(Super, Ps).


rdf_eq(S, P, O) :-
	rdf(S, P, O).
%rdf_eq(S, P, O) :-
%	equivalent_property(R),
%	rdf(S, R, S1),
%	rdf(S1, P, O).

equivalent_property(P) :-
	rdf_equal(P, owl:sameAs).
equivalent_property(P) :-
	rdf_equal(P, skos:exactMatch).


		 /*******************************
		 *	        HTML	        *
		 *******************************/

%%	html_start_page(+Class)
%
%	Emit an html page with a search field

html_start_page(Class) :-
	reply_html_page(search,
			title('Search'),
			[  \html_requires(css('interactive_search.css')),
			   div([style('margin-top:10em')],
				[ div([style('text-align:center')], \logo),
				  div([style('text-align:center;padding:0'), id(search)],
				      \search_field('', Class))])
			]).

%%	html_result_page(+Query, +Terms, +Class, +Relations, +Filter,
%%	+Offset, +Limit)
%
%	Emit an html page with a search field,
%	a left column with query suggestions, a body with the search
%	results and a right column with faceted filters.

html_result_page(QueryObj, ResultObj, Terms, RelatedTerms, Relations, Facets) :-
	QueryObj = query(Keyword, Class, SelectedTerms, SelectedRelations, Filter, Offset, Limit),
	ResultObj = result(Results, NumberOfResults, NumberOfRelationResults),
	reply_html_page(user(isearch),
			[ title(['Search results for ', Keyword])
 			],
			[  \html_requires(css('interactive_search.css')),
			   \html_requires(js('jquery-1.4.2.min.js')),
			   \html_requires(js('json2.js')),
			   div(id(header),
			       \html_header(Keyword, Class)),
 			   div(id(main),
			       div(class('main-content'),
				   [ \html_term_list(Terms, RelatedTerms, SelectedTerms),
				     div(id(results),
					 [ div(class(header),
					       [ \html_filter_list(Filter),
						 \html_relation_list(Relations, SelectedRelations,
								     NumberOfRelationResults)
					       ]),
					   div(class(body),
					       ol(\html_result_list(Results))),
					   div(class(footer),
					       \html_paginator(NumberOfResults, Offset, Limit))
					 ]),
				     \html_facet_list(Facets)
				   ])),
			   script(type('text/javascript'),
				  [ \script_body_toggle,
 				    \script_data(Keyword, Class, SelectedTerms, SelectedRelations, Filter),
				    \script_term_select(terms),
				    \script_relation_select(relations),
				    \script_facet_select(facets),
				    \script_suggestion_select(suggestions),
				    \script_filter_select(filters)
 				  ])
			]).

html_header(Keyword, Class) -->
	html(div(class('header-content'),
		 [ div(id(logo), \logo),
		   div(id(search),
		       \search_field(Keyword, Class))
		 ])).

html_term_list([], [], _) --> !,
	html(div([id(left), class(column)],
		div(class(body), ['']))).
html_term_list(Terms, RelatedTerms, SelectedTerms) -->
	html(div([id(left), class(column)],
		 [ div(class(toggle),
		       \toggle_link(ltoggle, lbody, '>', '>', '<')),
		   div([class(body), id(lbody)],
		       [ \html_term_list(Terms, SelectedTerms),
			 \html_related_term_list(RelatedTerms)
		       ])
		 ])).

html_facet_list(Facets) -->
	{ (   setting(search:show_single_value_facet, false)
	  ->  remove_single_value_facet(Facets, Facets1)
	  ;   Facets1 = Facets
	  )
	},
	html_facet_list_(Facets1).

html_facet_list_([]) --> !.
html_facet_list_(Facets) -->
	html(div([id(right), class(column)],
		 [ div(class(toggle),
		       \toggle_link(rtoggle, rbody, '<', '<', '>')),
		   div([class(body), id(rbody)],
		       div(id(facets),
			   \html_facets(Facets, 0))
		      )
		 ])).

%%	logo
%
%	Emit a logo

logo -->
	{ setting(search:logo, Src),
	  http_location_by_id(http_interactive_search, Home)
	},
	html(a(href(Home), img([alt('logo'), src(Src)], []))).

%%	search_field(+Query, +Class)
%
%	Emit an html search field.

search_field(Query, Class) -->
	html(form([input([type(text), class(inp), name(q), value(Query)]),
		   input([type(hidden), name(class), value(Class)]),
		   input([type(submit), class(btn), value(search)])
		  ])).

%%	html_result_list(+Resources)
%
%	Emit HTML list with resources.

html_result_list([]) --> !.
html_result_list([R-SearchInfo|Rs]) -->
	html(li(class(r), \format_result(R, SearchInfo))),
	html_result_list(Rs).

format_result(R, SearchInfo, In, Out) :-
	cliopatria:format_search_result(R, SearchInfo, In, Out), !.
format_result(R, _SearchInfo) -->
	html(div(class('result-item'),
		 [ div(class(thumbnail),
		       \result_image(R)),
		   div(class(text),
		       [ div(class(title),       \rdf_link(R)),
			 div(class(subtitle),    \result_subtitle(R)),
			 div(class(description), \result_description(R))
		       ])
		 ])).

result_subtitle(R) -->
	result_creator(R),
	result_date(R).
result_description(R) -->
	{ description_property(P),
	  rdf_has(R, P, LitDesc),
	  literal_text(LitDesc, DescTxt),
	  truncate_atom(DescTxt, 200, Desc)
	},
	!,
	html(Desc).
result_description(_R) --> !.

result_creator(R) -->
	{ rdf_has(R, dc:creator, C) }, !,
	rdf_link(C).
result_creator(_) --> [].

result_date(R) -->
	{ rdf_has(R, dc:date, D), !,
	  literal_text(D, DateTxt)
	},
	html([' (', DateTxt, ')']).
result_date(_) --> [].


result_image(R) -->
	{ image_property(P),
	  rdf_has(Image, P, R),
	  (   image_suffix(Suffix)
	  ->  true
	  ;   Suffix = ''
	  )
	},
	!,
	html(img(src(Image+Suffix), [])).
result_image(_) --> !.

%%	html_paginator(+NumberOfResults, +Offset, +Limit)
%
%	Emit HTML paginator.

html_paginator(Total, _Offset, Limit) -->
	{ Total < Limit },
	!.
html_paginator(Total, Offset, Limit) -->
	{ http_current_request(Request),
	  request_url_components(Request, URLComponents),
	  Pages is ceiling(Total/Limit),
	  ActivePage is floor(Offset/Limit),
	  (   ActivePage < 9
	  ->  EndPage is min(10, Pages)
	  ;   EndPage is min(10+ActivePage, Pages)
	  ),
	  StartPage is max(0, EndPage-20),
	  (   select(search(Search0), URLComponents, Cs)
	  ->  delete(Search0, offset=_, Search)
	  ;   Search = Search0
	  ),
	  parse_url(URL, [search(Search)|Cs])
	},
	html(div(class(paginator),
		 [ \prev_page(ActivePage, Limit, URL),
		   \html_pages(StartPage, EndPage, Limit, URL, ActivePage),
		   \next_page(ActivePage, Pages, Limit, URL)
		 ])).

prev_page(0, _, _) --> !.
prev_page(Active, Limit, URL) -->
	{ Offset is (Active-1)*Limit,
	  First = 0
	},
	html([span(class(first), a(href(URL+'&offset='+First), '<<')),
	      span(class(prev), a(href(URL+'&offset='+Offset), '<'))]).

next_page(_, 0, _, _) --> !.
next_page(Active, Last, _, _) -->
	{ Active is Last-1 },
	!.
next_page(Active, Last, Limit, URL) -->
	{ Offset is (Active+1)*Limit,
	  LastOffset is (Last-1)*Limit
	},
	html([span(class(next), a(href(URL+'&offset='+Offset), '>')),
	      span(class(last), a(href(URL+'&offset='+LastOffset), '>>'))]).

html_pages(N, N, _, _, _) --> !.
html_pages(N, Pages, Limit, URL, ActivePage) -->
	{ N1 is N+1,
	  Offset is N*Limit,
 	  (   N = ActivePage
	  ->  Class = active
	  ;   Class = ''
	  )
 	},
	html(span(class(Class), a(href(URL+'&offset='+Offset), N1))),
	html_pages(N1, Pages, Limit, URL, ActivePage).

%%	html_term_list(+Terms, +Selected)
%
%	Emit a list of terms matching the query.

html_term_list([], _) --> !.
html_term_list(Terms, Selected) -->
	{ setting(search:term_limit, Limit),
	  list_limit(Terms, Limit, TopN, Rest)
   	},
	html(div(id(terms),
		[ div(class(header), 'Did you mean?'),
		  div(class(items),
		      [ \resource_list(TopN, Selected),
			\resource_rest_list(Rest, term, Selected)
 		      ])
		])).

%%	html_relation_list(+Relations, +Selected, +NumberOfResults)
%
%	Emit html with matching relations.

html_relation_list([], _, NumberOfResults) --> !,
	html(div(id(relations),
		 div(class('relations-header'),
		     [NumberOfResults, ' result found']))).
html_relation_list(Relations, Selected, NumberOfResults) -->
	{ setting(search:relation_limit, Limit),
	  list_limit(Relations, Limit, TopN, Rest)
 	},
	html(div(id(relations),
		 [ div(class('relations-header'), [NumberOfResults, ' result found by: ']),
		   div(class('relations-content'),
		       [ \resource_list(TopN, Selected),
			 \resource_rest_list(Rest, relation, Selected)
		       ])
		 ])).

%%	html_related_term_list(+Pairs)
%
%	Emit html with facet filters.

html_related_term_list(Pairs) -->
	html(div(id('suggestions'),
		 \html_related_terms(Pairs, 0))).

html_related_terms([], _) --> !.
html_related_terms([P-Terms|T], N) -->
	{ N1 is N+1,
	  rdfs_label(P, Label),
 	  list_limit(Terms, 3, TopN, Rest)
 	},
	html(div(class(suggestion),
		 [ div(class(header), Label),
		   div([title(P), class(items)],
		      [ \resource_list(TopN, []),
			\resource_rest_list(Rest, suggestions+N, [])
		      ])
		 ])),
	html_related_terms(T, N1).

%%	html_facets(+Facets, +N)
%
%	Emit html with facet filters.

html_facets([], _) --> !.
html_facets([facet(P, ResultsByValue, Selected)|Fs], N) -->
	{ N1 is N+1,
	  rdfs_label(P, Label),
	  pairs_sort_by_result_count(ResultsByValue, Values)
  	},
	html(div(class(facet),
		 [ div(class(header), Label),
		   div([title(P), class(items)],
		       \resource_list(Values, Selected))
		 ])),
	html_facets(Fs, N1).

html_filter_list([]) --> !.
html_filter_list(Filter) -->
	html(div(id(filters),
		 \html_filter(Filter))).

html_filter([]) --> !.
html_filter([prop(P, Vs)|Ps]) -->
	{ rdfs_label(P, Label) },
	html(div([title(P), class(filter)],
		 [ div(class(property), [Label, ': ']),
		   ul(class('resource-list'),
		      \property_values(Vs))
		 ])),
	html_filter(Ps).

property_values([]) --> !.
property_values([V|Vs]) -->
	{ (   V = literal(_)
	  ->  literal_text(V, Label)
	  ;   rdfs_label(V, Label)
	  ),
	  resource_attr(V, Attr),
	  http_absolute_location(icons('checkbox_selected.png'), Img, [])
	},
	html(li([title(Attr)],
		div(class('value-inner'),
		   [ img([class(checkbox), src(Img)], []),
		     \resource_label(Label)
 		   ]))),
 	property_values(Vs).

remove_single_value_facet([], []) :- !.
remove_single_value_facet([facet(_, [_], [])|Fs], Rest) :- !,
	remove_single_value_facet(Fs, Rest).
remove_single_value_facet([F|Fs], [F|Rest]) :-
	remove_single_value_facet(Fs, Rest).

%%	resource_rest_list(+Pairs:count-resource, +Id, +Selected)
%
%	Emit HTML ul with javascript control to toggle display of
%	body

resource_rest_list([], _, _) --> !.
resource_rest_list(Rest, Id, Selected) -->
	{ (   member(S, Selected),
	      memberchk(_-S, Rest)
	  ->  Display = block,
	      L1 = less, L2 = more
	  ;   Display = none,
	      L1 = more, L2 = less
	  )
	},
	html([ul([id(Id+body),
		  class('resource-list toggle-body'),
		  style('display:'+Display)
		 ],
		 \resource_items(Rest, Selected)
		),
	      div(class('toggle-button'),
		  \toggle_link(Id+toggle, Id+body, L1, L2, L1))
	     ]).

%%	resource_list(+Pairs:count-resource, +Selected)
%
%	Emit list items.

resource_list([], _) --> !.
resource_list(Rs, Selected) -->
	html(ul(class('resource-list'),
		\resource_items(Rs, Selected))).

resource_items([], _) --> !.
resource_items([V|T], Selected) -->
	{ resource_term_count(V, R, Count),
	  (   R = literal(_)
	  ->  literal_text(R, Label)
	  ;   display_label(R, Label)
 	  )
	},
	resource_item(R, Label, Count, Selected),
 	resource_items(T, Selected).

resource_term_count(Count-R, R, Count) :- !.
resource_term_count(R, R, '') :- atom(R).

resource_item(R, Label, Count, Selected) -->
	{ Selected = [],
	  resource_attr(R, A)
	},
	!,
	html(li(title(A),
		\resource_item_content(Label, Count)
	       )).
resource_item(R, Label, Count, Selected) -->
 	 { memberchk(R, Selected),
	   resource_attr(R, A),
	   !,
 	   http_absolute_location(icons('checkbox_selected.png'), Img, [])
	},
	html(li([title(A), class(selected)],
		\resource_item_content(Label, Count, Img)
	       )).
resource_item(R, Label, Count, _Selected) -->
	{ http_absolute_location(icons('checkbox_unselected.png'), Img, []),
	  resource_attr(R, A)
	},
	html(li(title(A),
		  \resource_item_content(Label, Count, Img)
	       )).

resource_attr(R, R) :- atom(R), !.
resource_attr(Lit, S) :-
	prolog_to_json(Lit, JSON),
	with_output_to(string(S),
		       json_write(current_output, JSON, [])).

resource_item_content(Label, Count) -->
	html([ div(class(count), Count),
	       div(class('value-inner'),
		   \resource_label(Label))
	     ]).
resource_item_content(Label, Count, Img) -->
	html([ div(class(count), Count),
	       div(class('value-inner'),
		   [ img([class(checkbox), src(Img)], []),
		     \resource_label(Label)
 		   ])
	     ]).

resource_label(FullLabel) -->
	{ truncate_atom(FullLabel, 75, Label) },
	html(span([title(FullLabel), class(label)], Label)).

%%	toggle_link(+ToggleId, +BodyId, +ActiveLabel, +ToggleLabel)
%
%	Emit an hyperlink that toggles the display of BodyId.

toggle_link(ToggleId, BodyId, Label, Shown, Hidden) -->
	html(a([id(ToggleId), href('javascript:void(0)'),
		onClick('javascript:bodyToggle(\'#'+ToggleId+'\',\'#'+BodyId+'\',
					       [\''+Shown+'\',\''+Hidden+'\']);')
		    ], Label)).

		 /*******************************
		 *	    javascript      	*
		 *******************************/

script_data(Query, Class, Terms, Relations, Filter) -->
	{ http_location_by_id(http_interactive_search, URL),
	  prolog_to_json(Filter, FilterJSON),
	  Params = json([url(URL),
			 q(Query),
			 class(Class),
			 terms(Terms),
			 relations(Relations),
			 filter(FilterJSON)
			]),
	  with_output_to(string(Data),
		       json_write(current_output, Params, []))
	},
 	html(\[
'var data = ',Data,';\n',

'var isEqualLiteral = function(o1,o2) {\n',
'    var l1 = o1.literal,
	 l2 = o2.literal;
   if(l1&&l2) {\n',
'      if(l1===l2) { return true; }
       else if(l1.text===l2.text) {
	 if(l1.lang===l2.lang) { return true;}
	 else if(l1.type===l2.type) { return true; }
       }
    }
}\n;',

'var updateArray = function(a, e) {\n',
'  for(var i=0; i<a.length; i++) {
     if(a[i]==e||isEqualLiteral(e, a[i])) {
       a.splice(i,1); return a;
     }
  }
  a.push(e);
  return a;\n',
'};\n',
'var updateFilter = function(a, p, v, replace) {\n',
'  for(var i=0; i<a.length; i++) {\n',
'    if(a[i].prop==p) {\n',
'       if(replace) { a[i].values = [v] }
	else {
	    var vs = updateArray(a[i].values, v);
	    if(vs.length==0) { a.splice(i,1) }
	}
      return a;
      }\n',
'  }\n',
' a.push({prop:p, values:[v]});
  return a;
};\n'
	      ]).

script_body_toggle -->
	html(\[
'function bodyToggle(toggle, container, labels) {\n',
' if($(container).css("display") === "none") {
         $(container).css("display", "block");
	 $(toggle).html(labels[0]);
     }\n',
'    else {
	  $(container).css("display", "none");
	  $(toggle).html(labels[1]);
     }',
'}\n'
	      ]).

script_term_select(Id) -->
	html(\[
'$("#',Id,'").delegate("li", "click", function(e) {\n',
'   var terms = $(e.originalTarget).hasClass("checkbox") ?
		  updateArray(data.terms, $(this).attr("title")) :
		  $(this).attr("title"),
        params = jQuery.param({q:data.q,class:data.class,term:terms}, true);
    window.location.href = data.url+"?"+params;\n',
'})\n'
	      ]).

script_suggestion_select(Id) -->
	html(\[
'$("#',Id,'").delegate("li", "click", function(e) {\n',
'   var query = $(this).find(".label").attr("title"),
        params = jQuery.param({q:query,class:data.class}, true);
    window.location.href = data.url+"?"+params;\n',
'})\n'
	      ]).

script_relation_select(Id) -->
	html(\[
'$("#',Id,'").delegate("li", "click", function(e) {\n',
'   var relations = $(e.originalTarget).hasClass("checkbox") ?
		      updateArray(data.relations, $(this).attr("title")) :
		      $(this).attr("title"),
	params = jQuery.param({q:data.q,class:data.class,term:data.terms,filter:JSON.stringify(data.filter),relation:relations}, true);\n',
'   window.location.href = data.url+"?"+params;\n',
'})\n'
	      ]).

script_facet_select(Id) -->
	html(\[
'$("#',Id,'").delegate("li", "click", function(e) {\n',
'  var value = $(this).attr("title");
   try { value = JSON.parse(value) }
   catch(e) {}\n',
'  var property = $(this).parent().parent().attr("title"),
       replace = $(e.originalTarget).hasClass("checkbox"),
       filter = updateFilter(data.filter, property, value, !replace),
       params = jQuery.param({q:data.q,class:data.class,term:data.terms,relation:data.relations,filter:JSON.stringify(filter)}, true);\n',
'  window.location.href = data.url+"?"+params;\n',
'})\n'
	      ]).

script_filter_select(Id) -->
	html(\[
'$("#',Id,'").delegate("li", "click", function(e) {\n',
'  var value = $(this).attr("title");
   try { value = JSON.parse(value) }
   catch(e) {}\n',
'  var property = $(this).parent().parent().attr("title"),
       filter = updateFilter(data.filter, property, value),
       params = jQuery.param({q:data.q,class:data.class,term:data.terms,relation:data.relations,filter:JSON.stringify(filter)}, true);\n',
'  window.location.href = data.url+"?"+params;\n',
'})\n'
	      ]).
		 /*******************************
		 *	    utilities		*
		 *******************************/

%%	request_url_components(+Request, -URLComponents)
%
%	URLComponents contains all element in Request that together
%	create the request URL.

request_url_components(Request, [ protocol(http),
				  host(Host), port(Port),
				  path(Path), search(Search)
				]) :-
	http_current_host(Request, Host, Port,
			  [ global(false)
			  ]),
 	(   option(x_redirected_path(Path), Request)
	->  true
	;   option(path(Path), Request, /)
	),
	option(search(Search), Request, []).

%%	pairs_sort_by_result_count(+Pairs:key-list,
%%	-Sorted:listcount-key)
%
%	Sorted is a list with the keys of Pairs sorted by the number of
%	elements in the value list.

pairs_sort_by_result_count(Grouped, Sorted) :-
 	pairs_result_count(Grouped, Counted),
	keysort(Counted, Sorted0),
	reverse(Sorted0, Sorted).

pairs_result_count([], []).
pairs_result_count([Key-Results|T], [Count-Key|Rest]) :-
	(   Results = [H|_],
	    atom(H)
	->  length(Results, Count)
	;   result_uris(Results, URIs)
	),
	length(URIs, Count),
	pairs_result_count(T, Rest).


%%	list_offset(+List, +N, -SmallerList)
%
%	SmallerList starts at the nth element of List.

list_offset(L, N, []) :-
	length(L, Length),
	Length < N,
	!.
list_offset(L, N, L1) :-
	list_offset_(L, N, L1).

list_offset_(L, 0, L) :- !.
list_offset_([_|T], N, Rest) :-
	N1 is N-1,
	list_offset_(T, N1, Rest).

%%	list_limit(+List, +N, -SmallerList, -Rest)
%
%	SmallerList ends at the nth element of List.

list_limit(L, N, L, []) :-
	length(L, Length),
	Length < N,
	!.
list_limit(L, N, L1, Rest) :-
	list_limit_(L, N, L1, Rest).

list_limit_(Rest, 0, [], Rest) :- !.
list_limit_([H|T], N, [H|T1], Rest) :-
	N1 is N-1,
	list_limit_(T, N1, T1, Rest).

%%	instance_of_class(+Class, +R) is semidet.
%
%	True if R is of rdf:type Class.

instance_of_class(Class, S) :-
	(   var(Class)
	->  rdf_subject(S)
	;   rdf_equal(Class, rdfs:'Resource')
	->  rdf_subject(S)
	;   rdfs_individual_of(S, Class)
	), !.

		 /*******************************
		 *    PRESENTATION PROPERTIES   *
		 *******************************/

:- multifile
	title_property/1,
	description_property/1,
	image_property/1,
	image_suffix/1.

:- rdf_meta
	title_property(r),
	description_property(r),
	image_property(r).

title_property(dc:title).
title_property(skos:prefLabel).
title_property(skos:altLabel).
title_property(rdfs:label).

description_property(dc:description).
description_property(skos:scopeNote).

image_property('http://www.vraweb.org/vracore/vracore3#relation.depicts').
image_suffix('&resize100square').

%:- multifile
%	label_property/1.		% ?Resource

:- rdf_meta
	display_label(r, -).
 	%label_property(r).

display_label(R, Label) :-
	label_property(P),
	rdf_has(R, P, Lit),
	!,
	literal_text(Lit, Label).
display_label(R, Label) :-
	rdfs_label(R, Label).
/*
label_property(skos:prefLabel).
label_property(dc:title).
label_property(skos:altLabel).
label_property(rdfs:label).
label_property(P) :-
	catch(cliopatria:label_property(P), _, fail).
*/

		 /*******************************
		 *	      FACETS		*
		 *******************************/

%facet_exclude_property(rdf:type).
facet_exclude_property(dc:title).
facet_exclude_property(dc:description).
facet_exclude_property(dc:identifier).
facet_exclude_property(P) :-
	cliopatria:facet_exclude_property(P).


		 /*******************************
		 *	      HOOKS		*
		 *******************************/

%%	cliopatria:format_search_result(+Resource, +SearchInfo, +In, -Out)
%
%	Emit HTML for the presentation of Resource as a search
%	result.
%
%       @see This hook is used by format_result//2.

%%	cliopatria:facet_exclude_property(+Property) is semidet.
%
%	True if Property must be excluded from creating a facet.

%%	cliopatria:search_pattern(+Literal, Class, S, P, Term, Path)
%
%	@tbd	Document
