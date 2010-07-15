:- module(basic_search, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_host)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_litindex)).

:- http_handler(root(search), basic_search_page, []).

basic_search_page(Request) :-
	http_parameters(Request,
			[ q(Query,
				[description('Search query')]),
			  class(Class,
				[optional(true), description('Target Class')]),
			  offset(Offset,
				[default(0), integer, description('Offset of the result list')]),
			  limit(Limit,
				[default(20), integer, description('Limit on the number of results')])
			]),
 	findall(R, search_result(Query, Class, R), Rs0),
	sort(Rs0, Rs),
 	length(Rs, NumberOfResults),
	list_offset(Rs, Offset, Rs1),
	list_limit(Rs1, Limit, Results, _),
 	reply_html_page(title(['Search results for ',Query]),
			[ \html_requires(css('basic_search.css')),
			  div(class(search),
			       \search_field(Query, Class)),
			  div(class(results),
			      [ div(class(header),
				  [ NumberOfResults,' results for "',Query,'"']),
				\html_result_list(Results)
			      ]),
			  div(class(paginator),
			      \html_paginator(NumberOfResults, Offset, Limit)
			     )
  			]).

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
html_result_list([R|Rs]) -->
	format_result(R),
	html_result_list(Rs).

format_result(R) -->
	{ (   rdf_label(R, Lit)
	  ->  literal_text(Lit, Label)
	  ;   rdfs_label(R, Label)
	  )
	},
	html(div(class('result-item'),
		 [div(class(thumbnail),
		      \result_image(R)),
		  a(href(R), Label)
		 ])).

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

		 /*******************************
	         *	 collect results        *
		 *******************************/

%%	search_result(+Query, +Class, -R)
%
%	True if R is related to Query and of type Class.

search_result(Query, Class, R) :-
	rdf_find_literals(case(Query), Literals),
	member(Lit, Literals),
	rdf(S, P, literal(Lit)),
	search_path(S, P, Class, R).

search_path(S, _, Class, S) :-
 	instance_of_class(Class, S),
	!.
search_path(O, P, Class, R) :-
	rdfs_subproperty_of(P, rdfs:label),
	rdf(R, _, O),
	instance_of_class(Class, R).

		 /*******************************
		 *	       utilities	*
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
