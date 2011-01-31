/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Michiel Hildebrand
    E-mail:        M.Hildebrand@vu.nl
    WWW:           http://www.few.vu.nl/~michielh
    Copyright (C): 2010, CWI Amsterdam,
		   	 VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(app_isearch,
	  [ isearch_field//2,		% +Query, +Class
	    isearch_page/2		% Options, +Request
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
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
:- use_module(library(semweb/rdf_description)).
:- use_module(library(semweb/rdf_abstract)).
:- use_module(library(settings)).
:- use_module(library(apply)).

:- use_module(library(search/facet)).
:- use_module(components(label)).

:- multifile
	cliopatria:format_search_result/4,      % +Result, +Graph
	cliopatria:search_pattern/3.		% +Start, -Result, -Graph

:- rdf_meta
	isearch_field(+,r,?,?),
       	cliopatria:facet_exclude_property(r).

% declare application settings
%
% Do not change these here. Instead use this in your startup file:
%
%	==
%	:- set_setting_default(id, value).
%	==

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

% limits
:- setting(search:result_limit, integer, 10,
	  'Maximum number of results shown').
:- setting(search:term_limit, integer, 5,
	  'Maximum number of items shown in the term disambiguation list').
:- setting(search:relation_limit, integer, 5,
	  'Maximum number of relations shown').

% search patterns
:- setting(search:pattern_literal, boolean, true,
	   'Find results by a direct literal property').
:- setting(search:pattern_resource, boolean, true,
	   'Find results by an object property from which the resource has a matching label').

:- http_handler(root(isearch), isearch_page([]), [id(isearch)]).

%%	isearch_page(+Options, +Request)
%
%	HTTP handler for the interactive search application.  Options:
%
%	    * target_class(+URL)
%	    Class that defines targets.
%	    * query_type(+Type)
%	    One of =literal= or a type for rdf_find_literals/2.
%	    Default is =case=.
%	    * header(+Boolean)
%	    If =false=, omit the header that provides the search-field.

isearch_page(Options, Request) :-
	(   debugging(profile(isearch))
	->  profile(isearch_page2(Options, Request))
	;   isearch_page2(Options, Request)
	).

isearch_page2(Options, Request) :-
	setting(search:target_class, DefTargetClass),
	setting(search:result_limit, DefaultLimit),

	option(target_class(TargetClass), Options, DefTargetClass),

	http_parameters(Request,
			[ q(Keyword,
			    [ optional(true),
			      description('Search query')
			    ]),
			  class(Class,
				[ default(TargetClass),
				  description('Target Class')
				]),
			  term(Terms,
			       [ zero_or_more,
				 description('Disambiguation term')
			       ]),
			  relation(Relations,
				   [ zero_or_more,
				     description('Limit results by specific relation')
				   ]),
 			  filter(Filter,
				 [ default([]), json,
				   description('Filters on the result set')
				 ]),
			  offset(Offset,
				 [ default(0), integer,
				   description('Offset of the result list')
				 ]),
			  limit(Limit,
				[ default(DefaultLimit), integer,
				  description('Limit on the number of results')
				])
			]),
	(   var(Keyword)
	->  html_start_page(Class)
	;   QueryParams = query(Keyword,
				Class, Terms, Relations, Filter,
				Offset, Limit),

	    make_query(Keyword, Query, Options),

					% search
	    keyword_search_graph(Query, instance_of_class(Class),
				 AllResults, Graph),

					% limit by related terms
	    restrict_by_terms(Terms, AllResults, Graph, ResultsWithTerm),

					% limit by predicate on target
	    restrict_by_relations(Relations, ResultsWithTerm, Graph,
				  ResultsWithRelation),

					% limit by facet-value
	    filter_results_by_facet(ResultsWithRelation, Filter, Results),
	    compute_facets(Results, ResultsWithRelation, Filter, Facets),

	    length(ResultsWithRelation, NumberOfRelationResults),
	    length(Results, NumberOfResults),
	    list_offset(Results, Offset, OffsetResults),
	    list_limit(OffsetResults, Limit, LimitedResults, _),

	    graph_terms(Graph, MatchingTerms),
	    result_relations(ResultsWithTerm, Graph, MatchingRelations),
	    related_terms(Terms, Class, RelatedTerms),

 	    html_result_page(QueryParams,
			     result(LimitedResults, NumberOfResults, NumberOfRelationResults),
			     Graph,
			     MatchingTerms, RelatedTerms,
			     MatchingRelations, Facets, Options)
  	).

compute_facets(Results, AllResults, Filter, Facets) :-
	facets(Results, AllResults, Filter, ActiveFacets0, InactiveFacets0),
	maplist(cleanup_facet, ActiveFacets0, ActiveFacets1),
	maplist(cleanup_facet, InactiveFacets0, InactiveFacets1),
	length(AllResults, Total),
	map_list_to_pairs(facet_quality(Total), InactiveFacets1, Keyed),
	keysort(Keyed, Sorted),
	pairs_values(Sorted, InactiveFacets),
	append(ActiveFacets1, InactiveFacets, Facets).

cleanup_facet(Facet0, Facet) :-
	facet_merge_sameas(Facet0, Facet1),
	facet_join_single(Facet1, Facet).


%%	facet_quality(+Total, +Facet, -Quality)
%
%	Rate the facet. We use 1-Q to avoid the need to reverse the
%	search results.

facet_quality(Total, Facet, Quality) :-
	facet_balance(Facet, Balance),
	facet_object_cardinality(Facet, Card),
	facet_frequency(Facet, Total, Freq),
	facet_weight(Facet, Weight),
	Quality0 is Balance*Card*Freq*Weight,
	(   debugging(facet)
	->  Facet = facet(P, _, _),
	    rdf_display_label(P, Label),
	    debug(facet, '~p: ~w = ~w*~w*~w*~w~n',
		  [Label, Quality0, Balance, Card, Freq, Weight])
	;   true
	),
	Quality is 1-Quality0.


% conversion of json parameters.

:- json_object
	prop(prop:atom, values:_),
	literal(literal:atom),
	literal(literal:_),
	type(type:atom, text:_),
	lang(lang:atom, text:atom).

%%	http:convert_parameter(+Type, +Text, -Value) is semidet.
%
%	Convert for Type = =json= using json_to_prolog/2.

http:convert_parameter(json, Atom, Term) :-
	atom_json_term(Atom, JSON, []),
	json_to_prolog(JSON, Term).

%%	make_query(+Keyword, -Query, +Options) is det.
%
%	Create a query for rdf_find_literals/2

make_query(Keyword, Query, Options) :-
	option(query_type(QueryType), Options, case),
	make_query_type(QueryType, Keyword, Query).

make_query_type(literal, Keyword, literal(Keyword)) :- !.
make_query_type(QueryType, Keyword, Query) :-
	tokenize_atom(Keyword, Words),
	tokens_query(Words, QueryType, Query).

tokens_query([Word], QueryType, Query) :- !,
	token_query(Word, QueryType, Query).
tokens_query(['"',Word,'"'|T], QueryType, Query) :- !,
	(   T == []
	->  Query = Word
	;   Query = and(T, Q2),
	    tokens_query(T, QueryType, Q2)
	).
tokens_query([H|T], QueryType, and(Q1,Q2)) :-
	token_query(H, QueryType, Q1),
	tokens_query(T, QueryType, Q2).

token_query(Word, QueryType, Query) :-
	Query =.. [QueryType, Word].

%%	keyword_search_graph(+Query, :Filter, -Targets, -Graph) is det.
%
%	@param  Query is either a literal(Text), or an expression passed
%		to rdf_find_literals/2.
%	@param  Filter is called as call(Filter, Resource) to filter
%		the results.  The filter =true= performs no filtering.
%	@param	Targets is an ordered set of resources that match Query
%	@param	Graph is a list of rdf(S,P,O) triples that forms a
%		justification for Targets

keyword_search_graph(Query, Filter, Targets, Graph) :-
	(   Query = literal(Text)
	->  Literals = [Text]
	;   rdf_find_literals(Query, Literals)
	),
	findall(Target-G, keyword_graph(Literals, Filter, Target, G), TGPairs),
	pairs_keys_values(TGPairs, Targets0, GraphList),
	sort(Targets0, Targets1),
	append(GraphList, Graph0),
	sort(Graph0, Graph1),
	merge_sameas_graph(Graph1, Graph2, [sameas_mapped(Map)]),
	sort(Graph2, Graph),
	maplist(map_over_assoc(Map), Targets1, Targets2),
	sort(Targets2, Targets).

map_over_assoc(Assoc, In, Out) :-
	get_assoc(In, Assoc, Out), !.
map_over_assoc(_, In, In).

keyword_graph(Literals, Filter, Target, Graph) :-
	member(L, Literals),
	search_pattern(L, Target, Graph),
	(   Filter = _:true
	->  true
	;   call(Filter, Target)
	).

%%	search_pattern(+Start, -Target, -Graph) is nondet.
%
%	True when Target is a result  for   the  Start.  Graph is an RDF
%	graph represented as a list of rdf(S,P,O) triples that links the
%	Target to the Start.

search_pattern(Label, Target,
	       [ rdf(TN, PN, literal(Value))
	       | More
	       ]) :-
	setting(search:pattern_literal, true),
	rdf(TN, PN, literal(exact(Label), Value)),
	(   (rdf_is_bnode(TN)
	    ;rdf_equal(rdf:value, PN)
	    ),
	    rdf_has(Target, P, TN)
	*-> More = [ rdf(Target, P, TN) ]
 	;   TN = Target,
	    PN = P,
	    More = []
	).
search_pattern(Label, Target,
	       [ rdf(TN, PN, Term),
		 rdf(Term, LP, literal(Value))
	       | More
	       ]) :-
	setting(search:pattern_resource, true),
	rdf_has(Term, rdfs:label, literal(exact(Label), Value), LP),
	rdf(TN, PN, Term),
	(   rdf_is_bnode(TN),
	    rdf_has(Target, P, TN)
	*-> More = [ rdf(Target, P, TN) ]
	;   TN = Target,
	    PN = P,
	    More = []
	).
search_pattern(Label, Target, Graph) :-
	cliopatria:search_pattern(Label, Target, Graph).




%%	graph_terms(+Graph, -TermSet) is det.
%
%	TermSet is an ordered set  of  _terms_   in  Graph.  a _term_ is
%	defined as a resource found through a literal using its label.

graph_terms(Graph, TermSet) :-
	graph_terms_(Graph, Terms),
	sort(Terms, TermSet).

graph_terms_([], []).
graph_terms_([rdf(S,P,L)|T], Terms) :-
	(   rdf_is_literal(L),
	    rdfs_subproperty_of(P, rdfs:label),
	    \+ rdf_is_bnode(S)
	->  Terms = [S|More],
	    graph_terms_(T, More)
	;   graph_terms_(T, Terms)
	).

%%	restrict_by_terms(+Terms, +AllResults, +Graph, -Results) is det
%
%	Results is the subset of AllResults that  have at least one term
%	from Terms in their justification.

restrict_by_terms([], Results, _, Results) :- !.
restrict_by_terms(Terms, Results, Graph, TermResults) :-
	sort(Terms, TermSet),
	result_terms(Results, Graph, Result_Terms),
	matches_term(Result_Terms, TermSet, TermResults).

matches_term([], _, []).
matches_term([R-TL|T0], Terms, Results) :-
	(   ord_intersect(Terms, TL)
	->  Results = [R|More],
	    matches_term(T0, Terms, More)
	;   matches_term(T0, Terms, Results)
	).

result_terms(Results, Graph, Result_Terms) :-
	result_justifications(Results, Graph, TermJusts),
	maplist(value_graph_terms, TermJusts, Result_Terms).

value_graph_terms(R-G, R-T) :-
	graph_terms(G, T).

%%	result_relations(+Results, +Graph, -RelationSet) is det.
%
%	RelationSet is the set of all  predicates on the result-set that
%	appear in Graph.

result_relations(Results, Graph, Relations) :-
	map_list_to_pairs(=, Results, Pairs),
	list_to_assoc(Pairs, ResultAssoc),
	empty_assoc(R0),
	result_relations(Graph, ResultAssoc, R0, R),
	assoc_to_keys(R, Relations).

result_relations([], _, R, R).
result_relations([rdf(S,P,_)|T], Results, R0, R) :-
	(   get_assoc(P, R0, _)
	->  result_relations(T, Results, R0, R)
	;   get_assoc(S, Results, _)
	->  put_assoc(P, R0, true, R1),
	    result_relations(T, Results, R1, R)
	;   result_relations(T, Results, R0, R)
	).

%%	restrict_by_relations(+Relations, +AllResults, +Graph, -Result)
%
%	Restrict the result  to  results  that   are  based  on  one  of
%	Relations.
%
%	@param Relations is a list of (predicate) URIs.
%	@param AllResults is an ordered set of URIs
%	@param Graph is an ordered set of rdf(S,P,O)
%	@param Result is an ordered set of URIs

restrict_by_relations([], AllResults, _, AllResults) :- !.
restrict_by_relations(_, [], _, []) :- !.
restrict_by_relations(Relations, [R0|R], [T0|T], Results) :-
	cmp_subject(Diff, R0, T0),
	rel_restrict(Diff, R0, R, T0, T, Relations, Results).

rel_restrict(=, R0, R, T0, T, Relations, Result) :-
	(   rel_in(T0, Relations)
	->  Result = [R0|More],
	    restrict_by_relations(Relations, R, T, More)
	;   T = [T1|TT]
	->  cmp_subject(Diff, R0, T1),
	    rel_restrict(Diff, R0, R, T1, TT, Relations, Result)
	;   Result = []
	).
rel_restrict(>, R0, R, _, Graph, Relations, Result) :-
	(   Graph = [T0|T]
	->  cmp_subject(Diff, R0, T0),
	    rel_restrict(Diff, R0, R, T0, T, Relations, Result)
	;   Result = []
	).
rel_restrict(<, _, AllResults, T0, T, Relations, Result) :-
	(   AllResults = [R0|R]
	->  cmp_subject(Diff, R0, T0),
	    rel_restrict(Diff, R0, R, T0, T, Relations, Result)
	;   Result = []
	).

cmp_subject(Diff, R, rdf(S,_,_)) :-
	compare(Diff, R, S).

rel_in(rdf(_,P,_), Relations) :-
	memberchk(P, Relations).

%%	result_justifications(+Results, +Graph, -ResultGraphs)
%
%	ResultGraphs is a pair-list Result-SubGraph,  where Graph is the
%	transitive closure of Result in Graph.   ResultGraphs  is in the
%	same order as Results.
%
%	@tbd	This can be much more efficient: Results and Graph are
%		ordered by subject, so we can do the first step as an
%		efficient split.  Then we only need to take care of the
%		(smaller) number of triples that are not connected to
%		a result.

result_justifications(Results, Graph, Pairs) :-
	graph_subject_assoc(Graph, Assoc),
	maplist(result_justification(Assoc), Results, Pairs).

result_justification(SubjectAssoc, Result, Result-Graph) :-
	result_justification(Result, SubjectAssoc, [], _, Graph, []).

result_justification(Result, SubjectAssoc, S0, S, Graph, GT) :-
	(   memberchk(Result, S0)
	->  Graph = GT,
	    S = S0
	;   get_assoc(Result, SubjectAssoc, POList)
	->  po_result_just(POList, Result, SubjectAssoc,
			   [Result|S0], S, Graph, GT)
	;   Graph = GT,
	    S = S0
	).

po_result_just([], _, _, S, S, Graph, Graph).
po_result_just([P-O|T], R, SubjectAssoc, S0, S, [rdf(R,P,O)|Graph], GT) :-
	result_justification(O, SubjectAssoc, S0, S1, Graph, GT1),
	po_result_just(T, R, SubjectAssoc, S1, S, GT1, GT).

graph_subject_assoc(Graph, Assoc) :-
	rdf_s_po_pairs(Graph, Pairs),
	list_to_assoc(Pairs, Assoc).

%%	rdf_s_po_pairs(+Graph, -S_PO_Pairs) is det.
%
%	Transform Graph into a list of  pairs, where each key represents
%	a unique resource in Graph and each value is a p-o pairlist.
%
%	@param Graph is an _ordered_ set of rdf(S,P,O) triples.

rdf_s_po_pairs([], []).
rdf_s_po_pairs([rdf(S,P,O)|T], [S-[P-O|M]|Graph]) :-
	same_s(S, T, M, T1),
	rdf_s_po_pairs(T1, Graph).

same_s(S, [rdf(S,P,O)|T], [P-O|M], Rest) :- !,
	same_s(S, T, M, Rest).
same_s(_, Graph, [], Graph).


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

rdf_eq(S, P, O) :-
	rdf(S, P, O).

:- rdf_meta
	equivalent_property(r).

equivalent_property(owl:sameAs).
equivalent_property(skos:exactMatch).


%%	filter_results_by_facet(+Rs, +Filter, -Filtered)
%
%	Filtered contains the resources from Rs that pass Filter.

filter_results_by_facet(AllResults, [], AllResults) :- !.
filter_results_by_facet(AllResults, Filter, Results) :-
	facet_condition(Filter, AllResults, R, Goal),
	findall(R, (member(R, AllResults), Goal), Results).


		 /*******************************
		 *	        HTML	        *
		 *******************************/

%%	html_start_page(+Class)
%
%	Emit an html page with a search field

html_start_page(Class) :-
	reply_html_page(user(search),
			title('Search'),
			[  \html_requires(css('interactive_search.css')),
			   div([style('margin-top:10em')],
				[ div([style('text-align:center')], \logo),
				  div([style('text-align:center;padding:0'), id(search)],
				      \isearch_field('', Class))])
			]).

%%	html_result_page(+Query, +Graph, +Terms, +Relations, +Relation,
%%	+Facets, +Options)
%
%	Emit an html page with a search field,
%	a left column with query suggestions, a body with the search
%	results and a right column with faceted filters.

html_result_page(QueryObj, ResultObj, Graph, Terms, RelatedTerms, Relations, Facets, Options) :-
	QueryObj = query(Keyword,
			 Class, SelectedTerms, SelectedRelations, Filter,
			 Offset, Limit),
	ResultObj = result(Results, NumberOfResults, NumberOfRelationResults),
	reply_html_page(user(isearch),
			[ title(['Search results for ', Keyword])
 			],
			[  \html_requires(css('interactive_search.css')),
			   \html_requires(js('jquery-1.4.2.min.js')),
			   \html_requires(js('json2.js')),
			   \html_header(Keyword, Class, Options),
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
					       ol(\html_result_list(Results, Graph))),
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

html_header(_Keyword, _Class, Options) -->
	{ option(header(false), Options) }, !.
html_header(Keyword, Class, _Options) -->
	html(div(id(header),
		 div(class('header-content'),
		     [ div(id(logo), \logo),
		       div(id(search),
			   \isearch_field(Keyword, Class))
		     ]))).

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

html_facet_list([]) --> !.
html_facet_list(Facets) -->
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
	{ http_location_by_id(isearch, Home)
	},
	html(a([class(isearch_logo), href(Home)], '')).

%%	isearch_field(+Query, +Class)//
%
%	Component  that  provides  the  initial  search  field  for  the
%	interactive search application.

isearch_field(Query, Class) -->
	html(form([input([type(text), class(inp), name(q), value(Query)]),
		   input([type(hidden), name(class), value(Class)]),
		   input([type(submit), class(btn), value(search)])
		  ])).

%%	html_result_list(+Resources, +Graph)
%
%	Emit HTML list with resources.

html_result_list([], _) --> !.
html_result_list([R|Rs], Graph) -->
	html(li(class(r), \format_result(R, Graph))),
	html_result_list(Rs, Graph).

format_result(R, Graph) -->
	cliopatria:format_search_result(R, Graph).
format_result(R, _) -->
	html(div(class('result-item'),
		 [ div(class(thumbnail),
		       \result_image(R)),
		   div(class(text),
		       [ div(class(title),       \rdf_link(R, [max_length(120)])),
			 div(class(subtitle),    \result_subtitle(R)),
			 div(class(description), \result_description(R))
		       ])
		 ])).


result_subtitle(R) -->
	result_creator(R),
	result_date(R).
result_description(R) -->
	{ rdf_description(R, LitDesc),
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
		 [ div(class('relations-header'),
		       [ NumberOfResults, ' result found by: ' ]),
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
	  pairs_sort_by_result_count(ResultsByValue, AllValues),
	  top_bottom(5, 5, AllValues, Values)
  	},
	html(div(class(facet),
		 [ div(class(header), \rdf_link(P)),
		   div([title(P), class(items)],
		       \resource_list(Values, Selected))
		 ])),
	html_facets(Fs, N1).

top_bottom(MaxTop, MaxBottom, All, List) :-
	length(All, Len),
	(   Len =< MaxTop+MaxBottom
	->  List = All
	;   Skipped is Len-(MaxTop+MaxBottom),
	    top(MaxTop, All, Rest0, List, List1),
	    List1 = [Count-'__skipped'|List2],
	    skip(Skipped, 0, Count, Rest0, List2)
	).

top(0, All, All, List, List) :- !.
top(N, [H|T0], All, [H|T], List) :-
	succ(N1, N),
	top(N1, T0, All, T, List).

skip(0, Count, Count, List, List).
skip(N, C0, C, [C1-_|T], List) :-
	C2 is C0+C1,
	N2 is N-1,
	skip(N2, C2, C, T, List).


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
	{ rdf_display_label(V, Label),
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
	  (   R == '__skipped'
	  ->  Label = '<skipped>'
	  ;   rdf_display_label(R, Label)
	  )
	},
	resource_item(R, Label, Count, Selected),
 	resource_items(T, Selected).

resource_term_count(Count-R, R, Count) :- !.
resource_term_count(R, R, '').

resource_item(R, Label, Count, Selected) -->
	{ Selected = [],
	  resource_attr(R, A)
	}, !,
	html(li(title(A),
		\resource_item_content(Label, Count)
	       )).
resource_item(R, Label, Count, Selected) -->
	{ memberchk(R, Selected),
	  resource_attr(R, A), !,
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
		\resource_item_content(Label, Count, Img))).

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
		 *	    JAVASCRIPT      	*
		 *******************************/

script_data(Query, Class, Terms, Relations, Filter) -->
	{ http_current_request(Request),
	  memberchk(path(URL), Request),
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

%%	pairs_sort_by_result_count(+Pairs:key-list, -Sorted:listcount-key)
%
%	Sorted is a list with the keys of Pairs sorted by the number of
%	elements in the value list.

pairs_sort_by_result_count(Grouped, Sorted) :-
 	pairs_result_count(Grouped, Counted),
	keysort(Counted, Sorted0),
	reverse(Sorted0, Sorted).

pairs_result_count([], []).
pairs_result_count([Key-Results|T], [Count-Key|Rest]) :-
	(   integer(Results)
	->  Count = Results
	;   length(Results, Count)
	),
	pairs_result_count(T, Rest).


%%	list_offset(+List, +N, -SmallerList)
%
%	SmallerList starts at the nth element of List.

list_offset([], _, []) :- !.
list_offset(L, 0, L) :- !.
list_offset([_|T], N, Rest) :-
	N1 is N-1,
	list_offset(T, N1, Rest).

%%	list_limit(+List, +N, -SmallerList, -Rest)
%
%	SmallerList ends at the nth element of List.

list_limit([], _, [], []) :- !.
list_limit(Rest, 0, [], Rest) :- !.
list_limit([H|T], N, [H|T1], Rest) :-
	N1 is N-1,
	list_limit(T, N1, T1, Rest).

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
	image_property/1,
	image_suffix/1.

:- rdf_meta
	image_property(r).

image_property('http://www.vraweb.org/vracore/vracore3#relation.depicts').
image_suffix('&resize100square').


		 /*******************************
		 *	      HOOKS		*
		 *******************************/

%%	cliopatria:format_search_result(+Resource)//
%
%	Emit HTML for the presentation of Resource as a search result.
%
%       @see This hook is used by format_result//1.

%%	cliopatria:search_pattern(+Start, -Result, -Graph) is nondet.
%
%	True when the resource Result is   a search-result for Start and
%	Graph is a list of rdf(S,P,O) triples that justify this.
