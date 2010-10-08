/*  This file is part of ClioPatria.

	Author:
	HTTP:	http://e-culture.multimedian.nl/
	GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
	GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
	GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
	Copyright:  2007, E-Culture/MultimediaN

	ClioPatria is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	ClioPatria is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(html_graph,
	  [ property_table//1		% +Pairs:p-v
	]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).


%%	property_table(+Pairs:property-resources)
%
%	Write html table with all property value Pairs.

property_table(Grouped) -->
 	html([ table(class(properties),
 		     [ \ptable_header
		     | \ptable_rows(Grouped)
		     ])
	     ]).

ptable_header -->
	html(tr([th('Predicate'), th('Object')])).

ptable_rows(Grouped) -->
	ptable_rows(Grouped, 1).

ptable_rows([], _) -->
	[].
ptable_rows([H|T], Row) -->
	{ Row1 is Row + 1 },
	ptable_row(H, Row),
	ptable_rows(T, Row1).

ptable_row(P-VL, Row) -->
	{ ( Row mod 2 =:= 0 -> Class = even ; Class = odd ),
	  rdf_global_id(Alias:Local, P)
	},
	html(tr(class(Class),
		[ td(class(predicate), \plink(P)),
		  td(class(object),    ul(\vlist(VL, Alias:Local)))
		])).

vlist([], _) --> [].
vlist([H|T], A:L) --> html(li(\vdiv(H, A:L))), vlist(T, A:L).

vdiv(literal(Literal), A:L) --> !,
	{ text_of_literal(Literal, Text) },
	html(div([property(A+':'+L), class(lvalue)], Text)).
vdiv(R, A:L) -->
	html(div(class(rvalue), \rlink(R, A:L))).

rlink(R, A:L) -->
	{ rdfs_ns_label(R, Label),
	  uri_iri(URI, R),
	  HREF = URI
	},
	html(a([rel(A+':'+L), href(HREF)], Label)).

plink(R) -->
	{ rdfs_ns_label(R, Label),
	  uri_iri(URI, R),
	  HREF = URI
	},
	html(a(href(HREF), Label)).


%%	text_of_literal(+LiteralArg, -Text)
%
%	Text is the raw text of   a possibly typed of language-qualified
%	RDF literal.

text_of_literal(literal(Lit), Text) :- !,
	text_of_literal(Lit, Text).
text_of_literal(lang(_, Text), Text) :- !.
text_of_literal(type(_, Text), Text) :- !.
text_of_literal(Text, Text).

