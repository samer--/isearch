:- module(interactive_search, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_host)).
:- use_module(library(http/html_head)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).

:- http_handler(root(isearch), interactive_search_page, []).

:- json_object
     prop(prop:atom, values:_),
     literal(literal:atom),
     literal(literal:_),
     type(type:atom, text:atom),
     lang(lang:atom, text:atom).

interactive_search_page(Request) :-
	http_parameters(Request,
			[ q(Query,
				[description('Search query')]),
			  class(Class,
				[optional(true), description('Target Class')]),
			  term(Term,
			       [optional(true), description('Disambiguation term')]),
			  related(Related,
				  [optional(true), description('Related term')]),
			  filter(FilterAtom,
				 [default([]),
				  description('Filters on the result set')]),
			  offset(Offset,
				[default(0), integer, description('Offset of the result list')]),
			  limit(Limit,
				[default(20), integer, description('Limit on the number of results')])
			]),
	(   atom_json_term(FilterAtom, FilterJSON, [])
	->  json_to_prolog(FilterJSON, Filter)
	;   Filter = []
	),

	% collect results
	findall(Path-R, search_result(Query, Class, Filter, R, Path), PathResults),

	% collected terms
	terms_in_paths(PathResults, TermResults0),
	sort(TermResults0, TermResults),
	group_pairs_by_key(TermResults, ResultsByTerm),
	pairs_sort_by_value_count(ResultsByTerm, MatchingTerms),

	% collected related terms
	(   nonvar(Term)
	->  findall(P-T, related_term(Term, Class, T, P), PathTerms0),
	    sort(PathTerms0, PathTerms),
	    group_pairs_by_key(PathTerms, RelatedTerms)%TermsByPath)
	    %pairs_sort_by_value_count(PathsByTerm, RelatedTerms)
	;   RelatedTerms = []
	),

	% results corresponding to current query, term or related term
	(   nonvar(Related)
	->  findall(R1, term_result(Related, Class, R1, _), Rs0)
	;   nonvar(Term)
	->  memberchk(Term-Rs0, ResultsByTerm)
	;   pairs_values(PathResults, Rs0)
	),

	% take limit and offset of results
	sort(Rs0, Rs),
 	length(Rs, NumberOfResults),
	list_offset(Rs, Offset, Rs1),
	list_limit(Rs1, Limit, Results, _),

	% collect facets
	facets(Rs, Facets),

	% emit html page
	reply_html_page([ title(['Search results for ',Query]),
			  script(type('text/javascript'),
				 \js_toggle)
			],
			[  \html_requires(css('interactive_search.css')),
			   div(id(search),
			       \search_field(Query, Class)
			      ),
			   div([id(left), class(column)],
			       [ div(class(toggle),
				     a([id(ltoggle),href('#'),
					onClick('javascript:bodyToggle(\'ltoggle\',\'lbody\', [\'<\',\'>\']);')
				 ], '<')),
				 div([class(body), id(lbody)],
				     [ \html_term_list(MatchingTerms, Query, Class, Term),
				       \html_related_term_list(RelatedTerms, Query, Class, Term, Related)
				     ])
			       ]),
			   div(id(results),
			      [ div(class(header),
				  \html_result_header(Query, Term, Related, Class, NumberOfResults)
				   ),
				\html_result_list(Results)
			      ]),
			   div([id(right), class(column)],
			       [ div(class(toggle), a([id(rtoggle),href('#'),
					onClick('javascript:bodyToggle(\'rtoggle\',\'rbody\', [\'>\',\'<\']);')
				 ], '>')),
				 div([class(body), id(rbody)],
				     \html_facets(Facets, Query, Class, Term, Related, Filter))
			      ]),
			   div(class(paginator),
			      \html_paginator(NumberOfResults, Offset, Limit)
			     )
  			]).

terms_in_paths([], []).
terms_in_paths([Path-R|T], [Term-R|Rest]) :-
	Path = [_,Term|_],
	atom(Term),
	!,
	terms_in_paths(T, Rest).
terms_in_paths([_|T], Rest) :-
	terms_in_paths(T, Rest).

		 /*******************************
		 *	 HTML result page	*
		 *******************************/

%%	search_field(+Query, +Class)
%
%	Emit an html search field.

search_field(Query, Class) -->
	html(form([input([type(text), class(inp), name(q), value(Query)]),
		   input([type(hidden), name(class), value(Class)]),
		   input([type(submit), class(btn), value(search)])
		  ])).

%%	html_result_header(+Query, +Term, +Related, +NumberOfResults)
%
%	Emit HTML with info on the number of results found.

html_result_header(Query, Term, Related, Class, NumberOfResults) -->
	{ http_location_by_id(interactive_search_page, URL),
	  remove_var([q(Query), term(Term), related(Related)], Path)
	},
 	html([NumberOfResults,' results related to ',
	      \query_path(Path, URL+'?', Class)
	     ]).

remove_var([H|T], [H|Rest]) :-
	ground(H),
	!,
	remove_var(T, Rest).
remove_var(_, []).

query_path([C], _, _) -->
	{ C =.. [Type, Value]
	},
	query_path_label(Type, Value).
query_path([C|T], URL0, Class) -->
	{ C =.. [Type,Value],
 	  URL = URL0+Type+'='+Value+'&'
	},
      	html([ a(href(URL+'class='+Class),
		 \query_path_label(Type, Value)),
	       ' > ']),
	query_path(T, URL, Class).

query_path_label(q, Q) --> !,
	html(['"',Q,'"']).
query_path_label(_, R) -->
	{ rdfs_label(R, Label) },
	html(span(title(R), Label)).

%%	html_result_list(+Resources)
%
%	Emit HTML list with resources.

html_result_list([]) --> !.
html_result_list([R|Rs]) -->
	format_result(R),
	html_result_list(Rs).

format_result(R) -->
	html(div(class('result-item'),
		 [ div(class(title), a(href(R), \result_title(R))),
		   div(class(thumbnail),
		       \result_image(R)),
		   div(class(text),
		       [ div(class(subtitle), \result_subtitle(R)),
			 div(class(description), \result_description(R))
		       ])
		 ])).

result_title(R) -->
	{ (   rdf_label(R, Lit)
	  ->  literal_text(Lit, Label)
	  ;   rdfs_label(R, Label)
	  )
	},
	html(Label).
result_subtitle(R) -->
	{ (   rdf_has(R, dc:creator, C)
	  ->  rdf_label(C, Lit),
	      literal_text(Lit, Creator)
	  ;   Creator = ''
	  ),
	  (   rdf_has(R, dc:date, D)
	  ->  literal_text(D, Date)
	  ;   Date = ''
	  )
	},
	html([Creator, ' (', Date, ')']).
result_description(R) -->
	{ rdf_has(R, dc:description, LitDesc),
	  literal_text(LitDesc, DescTxt),
	  truncate_atom(DescTxt, 300, Desc)
	},
	!,
	html(Desc).
result_description(_R) --> !.


result_image(R) -->
	{ rdf(Image, 'http://www.vraweb.org/vracore/vracore3#relation.depicts', R)
	},
	!,
	html(img(src(Image), [])).
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
 	prev_page(ActivePage, Limit, URL),
	html_pages(StartPage, EndPage, Limit, URL, ActivePage),
	next_page(ActivePage, Pages, Limit, URL).

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

%%	html_term_list(+Matches, +Query, +Class, +Term)
%
%	Emit a list of terms matching the query.

html_term_list([], _, _, _) -->
	!.
html_term_list(Terms, Query, Class, Term) -->
	{ list_limit(Terms, 5, TopN, Rest),
	  http_location_by_id(interactive_search_page, URL0),
	  URL = URL0+'?q='+Query+'&class='+Class+'&term='
	},
	html(div(class(matches),
		 [
		   div(id(mitems),
		       [ ul([\all_link(Term, URL),
			     \term_list(TopN, Term, URL)
			    ]),
			 \term_list_rest(Rest, Term, URL)
		       ])
		 ])).

term_list_rest([], _, _) --> !.
term_list_rest(Rest, Term, URL) -->
	html([ul([id(tbody),style('display:none')], \term_list(Rest, Term, URL)),
	      div(a([id(ttoggle),href('#'),
		     onClick('javascript:bodyToggle(\'ttoggle\',\'tbody\', [\'less\',\'more\']);')
		    ], 'more'))
	     ]).


all_link(Term, URL) -->
	{ nonvar(Term) },
	!,
	item_link(term, URL, 'All', '').
all_link(_, _) -->
	html(li(class(term),
		div(class(label),'All'))).

item_link(CSSClass, Link, Label, Count) -->
	html(li(class(CSSClass),
		a(href(Link),
		  [ div(class(count), Count),
		    div(class(label), Label)
		  ]
		 ))).

term_list([], _, _) --> !.
term_list([H|T], Active, URL) -->
	{ term_count(H, Term, Count),
	  rdfs_label(Term, Label),
	  (   nonvar(Active),
	      Term == Active
	  ->  Class = 'term selected'
	  ;   Class = 'term'
	  )
 	},
	item_link(Class, URL+Term, Label, Count),
 	term_list(T, Active, URL).

term_count(Count-Term, Term, Count).
term_count(Term, Term, '').

%%	html_related_term_list(+Groups, +Query, +Class, +ActiveTerm,
%%	+ActiveRelated)
%
%	Emit HTML list with terms.

html_related_term_list(Groups, Query, Class, Term, Related) -->
	related_term_list(Groups, Query, Class, Term, Related, 0).

related_term_list([], _, _, _, _, _) --> !.
related_term_list([P-Terms|T], Query, Class, Term, Related, N) -->
	{ N1 is N+1,
	  list_limit(Terms, 3, TopN, Rest),
	  rdfs_label(P, PLabel),
	  http_location_by_id(interactive_search_page, URL0),
	  URL = URL0+'?q='+Query+'&class='+Class+'&term='+Term+'&related='
	},
	html(div(class(related),
		 [ div(class(header), PLabel),
		   div(class(items),
		       [ ul(\term_list(TopN, Term, URL)),
			 \related_term_list_rest(Rest, Related, URL, N)
 		       ])
		 ])),
	related_term_list(T, Query, Class, Term, Related, N1).

related_term_list_rest([], _, _, _) --> !.
related_term_list_rest(Rest, Related, URL, N) -->
	html([ul([id(rbody+N),style('display:none')], \term_list(Rest, Related, URL)),
	      div(a([id(rtoggle+N),href('#'),
		     onClick('javascript:bodyToggle(\'rtoggle'+N+'\',\'rbody'+N+'\',[\'less\',\'more\']);')
		    ], 'more'))
	     ]).


%%	html_facets(+Facets, +Query, +Class, +Term, +Related, +Filter)
%
%	Emit html with facet filters.

html_facets(Facets, Query, Class, Term, Related, Filter) -->
	{ http_location_by_id(interactive_search_page, URL0),
	  url_parameters([q=Query,class=Class,term=Term,related=Related], Params),
	  URL = URL0+'?'+Params
	},
	facets(Facets, Filter, URL).

facets([], _, _) --> !.
facets([P-Values|Fs], Filter, URL) -->
	{ rdfs_label(P, Label)
	},
	html(div(class(facet),
		 [ div(class(header), Label),
		   div(class(items),
		       ul(
			  \facet_values(Values, P, Filter, URL))
		      )
		 ])),
	facets(Fs, Filter, URL).

facet_values([], _, _, _) --> !.
facet_values([Value-Rs|T], P, Filter0, URL) -->
	{ (   Value = literal(_)
	  ->  literal_text(Value, Label0)
	  ;   rdfs_label(Value, Label0)
	  ),
	  truncate_atom(Label0, 75, Label),
	  length(Rs, Count),
	  update_filter(Filter0, P, Value, Filter),
	  prolog_to_json(Filter, FilterJSON),
	  with_output_to(string(FilterString0),
		       json_write(current_output, FilterJSON, [width(0)])),
	  www_form_encode(FilterString0, FilterString),
 	  Link = URL+'filter='+FilterString
	},
	html(li(class(value),
		a(href(Link),
		  [ div(class(count), Count),
		    div(class(label), Label)
 		  ])
	       )),
	facet_values(T, P, Filter, URL).

update_filter([], P, Value, [Filter]) :-
	Filter = prop(P, [Value]).
update_filter([F|Fs], P, Value, Filters) :-
	F = prop(P, Values),
	!,
	(   select(Value, Values, Rest)
	->  (	Rest = []
	    ->	Filters = Fs
	    ;	Filters = [prop(P, Rest)|Fs]
	    )
	;   Filters = [prop(P, [Value])|Fs]
	).
update_filter([F|Fs], P, Value, [F|Filters]) :-
	update_filter(Fs, P, Value, Filters).

url_parameters([], '').
url_parameters([Key=Value|T], Ps) :-
	(   nonvar(Value)
	->  Ps = Key+'='+Value+'&'+Rest
	;   Ps = Rest
	),
	url_parameters(T, Rest).


%%	js_toggle
%
%	Emit javascript function to toggle display of an element.

js_toggle -->
	html(\[
'function bodyToggle(toggleId, containerId, labels) {\n',
'    var elContainer = document.getElementById(containerId),
	 elToggle = document.getElementById(toggleId);\n',
'    if(elContainer.style.display === "none") {
         elContainer.style.display = "block";
	 elToggle.innerHTML = labels[0];
     }\n',
'    else {
	  elContainer.style.display = "none";
	   elToggle.innerHTML = labels[1];
     }',
'}\n'
	     ]).

		 /*******************************
	         *	 collect results        *
		 *******************************/

%%	search_result(+Query, +Class, -R, -Path)
%
%	True if R is related to Query and of type Class.

search_result(Query, Class, Filter, R, Path) :-
	filter_to_goal(Filter, R, FilterGoal),
	(   nonvar(Class)
	->  Goal = (instance_of_class(Class, R), FilterGoal)
	;   Goal = FilterGoal
	),
	result(Query, Goal, R, Path).

result(Query, Goal, R, [P,Query]) :-
	once(rdf(_,_,Query)),
	!,
	rdf(R, P, Query),
	call(Goal).
result(Query, Goal, R, Path) :-
	rdf_find_literals(case(Query), Literals),
	member(Lit, Literals),
	rdf(S, P, literal(Lit)),
	search_path(S, P, Goal, R, [P,literal(Lit)], Path).

search_path(S, _P, Goal, S, Path, Path) :-
	call(Goal),
	!.
search_path(O, P, Goal, R, Path, [Prop,O|Path]) :-
	rdfs_subproperty_of(P, rdfs:label),
	rdf(R, Prop, O),
	call(Goal).

filter_to_goal([], _, true).
filter_to_goal([prop(P, Values)|T], R, (Goal,Rest)) :-
	pred_filter(Values, P, R, Goal),
	filter_to_goal(T, R, Rest).

pred_filter([Value], P, R, Goal) :- !,
	Goal = rdf(R, P, Value).
pred_filter([Value|Vs], P, R, Goal) :-
	Goal =  (rdf(R, P, Value), Rest),
	pred_filter(Vs, P, R, Rest).

%%	term_result(+Term, +Class, -R)
%
%	True if R is related to Term and of type Class.

term_result(Term, Class, R, [P,Term]) :-
 	rdf(R, P, Term),
	instance_of_class(Class, R).

%%	related_term(+Resource, +Class, -Term)
%
%	Term is related to Resource.

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


%%	facets(+Results, -Facets)
%
%	Collect faceted properties of Results.

facets(Results, Facets) :-
	bagof(P-Values,
		bagof(V-Rs,
		      bagof(R,
			    ( member(R, Results),
			      resource_property(R, P, V)
			    ),
			    Rs
			   ),
		      Values
		     ),
		Facets).

resource_property(S, P, O) :-
	rdf(S, P, O).


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

%%	pairs_sort_by_value_count(+Pairs:key-list,
%%	-Sorted:listcount-key)
%
%	Sorted is a list with the keys of Pairs sorted by the number of
%	elements in the value list.

pairs_sort_by_value_count(Grouped, Sorted) :-
 	pairs_value_count(Grouped, Counted),
	keysort(Counted, Sorted0),
	reverse(Sorted0, Sorted).

pairs_value_count([], []).
pairs_value_count([Key-Values|T], [Count-Key|Rest]) :-
	length(Values, Count),
	pairs_value_count(T, Rest).

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

:- rdf_meta
    instance_of_class(r, r).

%%	instance_of_class(+Class, -R)
%
%	True if R is of rdf:type Class.

instance_of_class(Class, S) :-
	var(Class), !,
	rdf_subject(S).
instance_of_class(Class, S) :-
	rdf_equal(Class, rdfs:'Resource'), !,
	(   rdf(S, rdf:type, Class)
	;    \+ rdf(S, rdf:type, _)
	).
instance_of_class(Class, S) :-
	rdf(S, rdf:type, Class).
