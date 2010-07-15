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

:- module(json_graph,
	[	properties_to_json/2,           % +Pairs:property-resource, -JSONList
		rdf_resource_to_json/2		% +Resource, -JSONTerm
	]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library('http/json')).
:- use_module(library('http/json_convert')).
:- use_module(library('http/http_json')).


%%	graph_to_json(+Pairs:property-resources, -JSON)
%
%	JSON is a prolog json term of grouped Pairs.

properties_to_json([], []) :- !.
properties_to_json([P-Vs|T], [P=JSON|Rest]) :-
	resources_to_json(Vs, JSON),
	properties_to_json(T, Rest).

%%	resources_to_json(+Rs, -JSON)
%
%	Convert all resources in Rs to a JSON term.

resources_to_json([], []) :- !.
resources_to_json([R|T], [json(JSON)|Vs]) :-
 	rdf_resource_to_json(R, JSON),
	resources_to_json(T, Vs).

%%	rdf_resource_to_json(+Resource, -JSON)
%
%	Convert an RDF Resource to a JSON term.

rdf_resource_to_json(Bool, Object) :-
	boolean_to_json(Bool, Boolean), !,
	Object = [value=Boolean, type=boolean].
rdf_resource_to_json(literal(Lit), Object) :- !,
	Object = [value=Txt, type=literal|Rest],
	literal_to_json(Lit, Txt, Rest).
rdf_resource_to_json(URI0, Object) :-
	rdf_global_id(URI0, URI),
	Object = [value=URI, type=Type],
	object_uri_type(URI, Type).

%%	literal_to_json(+Literal, -Text, -Attributes)
%
%	Extract text and Attributes from Literal resource.

literal_to_json(lang(Lang, Txt), Txt, [lang=Lang]) :- !.
literal_to_json(type(Type, Txt), Txt, [datatype=Type]) :- !.
literal_to_json(Txt, Txt, []).

%%	boolean_to_json(?Bool, ?JSONBool)
%
%	JSONBool has an extra @ in front of true or false.

boolean_to_json(false, @false).
boolean_to_json(true, @true).

%%	object_uri_type(+URI, -Type)
%
%	Type is one of bnode or uri.

object_uri_type(URI, Type) :-
	(   rdf_is_bnode(URI)
	->  Type = bnode
	;   Type = uri
	).
