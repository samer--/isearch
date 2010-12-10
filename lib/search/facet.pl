/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
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

:- module(search_facet,
	  [ facets/4,			% +Results, +AllResults, +Filter, -Facets
	    facet_merge_sameas/2,	% +FacetIn, -FacetOut
	    facet_condition/3		% +Facets, ?Resource, -Goal
	  ]).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/owl_sameas)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_description)).

/** <module> Computations for facetted search

This module computes facets  for  lists   of  resources  and  implements
various operations on facets.  A facet is represented as

    * prop(Property, Values)
    * prop(Property, Values, Selected)
*/

:- multifile
	cliopatria:facet_exclude_property/1.	% ?Resource


%%	facets(+Results, +AllResults, +Filter, -Facets)
%
%	Collect faceted properties of Results.
%
%	@param	Results is the set of results after applying the facet
%		filter.
%	@param	AllResults is the set of results before applying the
%		facet filter
%	@param	Filter is the facet filter, which is a list of terms
%		prop(P, SelectedValues).
%	@param	Facets is a list of
%			facet(P, Value_Results_Pairs, SelectedValues)

facets([], _, _, []) :- !.
facets(_, _, _, []) :-
	setting(search:show_facets, false), !.
facets(FilteredResults, AllResults, Filter, Facets) :-
	inactive_facets(FilteredResults, Filter, InactiveFacets),
	active_facets(AllResults, Filter, ActiveFacets),
 	append(ActiveFacets, InactiveFacets, Facets).

inactive_facets(Results, Filter, Facets) :-
	findall(P-(V-R), inactive_facet_property(Results, Filter, R,P,V), Pairs),
	sort(Pairs, ByP),
	group_pairs_by_key(ByP, Grouped),
	maplist(make_facet, Grouped, Facets).

make_facet(P-V_R, facet(P, V_RL, [])) :-
	group_pairs_by_key(V_R, V_RL).

inactive_facet_property(Results, Filter, R, P, V) :-
	member(R, Results),
	facet_property(R, P, V),
	\+ memberchk(prop(P,_), Filter),
	\+ facet_exclude_property(P).

active_facets(Results, Filter, Facets) :-
	findall(P-(V-R),
		active_facet_property(Results, Filter, R, P, V), Pairs),
	sort(Pairs, ByP),
	group_pairs_by_key(ByP, Grouped),
	maplist(make_active_facet(Filter), Grouped, Facets).

make_active_facet(Filter, P-V_R, facet(P, V_RL, Selected)) :-
	memberchk(prop(P, Selected), Filter),
	group_pairs_by_key(V_R, V_RL).

active_facet_property(Results, Filter, R, P, V) :-
	select(prop(P, _), Filter, FilterRest),
	facet_condition(FilterRest, R, Goal),
	member(R, Results),
	call(Goal),
	rdf_has(R, P, V).


facet_property(S, P, V) :-
	rdf(S, P0, V0),
	real_value(V0, V),
	root_property(P0, P).

real_value(V0, V) :-
	rdf_is_bnode(V0),
	rdf_has(V0, rdf:value, V), !.
real_value(V, V).

:- dynamic
	root_property_cache/3.

root_property(P0, Super) :-
	rdf_generation(Generation),
	(   root_property_cache(P0, Super, Generation)
	*-> true
	;   retractall(root_property_cache(P0, _, Generation)),
	    forall(root_property_uncached(P0, Super),
		   assert(root_property_cache(P0, Super, Generation))),
	    root_property_cache(P0, Super, Generation)
	).

root_property_uncached(P0, Super) :-		% FIXME: can be cyclic?
	findall(P, ( rdf_reachable(P0, rdfs:subPropertyOf, P),
		     \+ rdf(P, rdfs:subPropertyOf, _)
		   ),Ps0),
	sort(Ps0, Ps),
	member(Super, Ps).

%%	facet_merge_sameas(Facet0, Facet) is det.
%
%	Merge different values for  a  facet   that  are  linked through
%	owl:sameAs.
%
%	@param facet(P, Value_Result_Pairs, SelectedValues)

facet_merge_sameas(facet(P, VRPairs0, SelectedValues0),
		   facet(P, VRPairs,  SelectedValues)) :-
	pairs_keys(VRPairs0, Values),
	owl_sameas_map(default, Values, Map),
	maplist(map_key(Map), VRPairs0, VRPairs1),
	sort(VRPairs1, VRPairs2),
	group_pairs_by_key(VRPairs2, Grouped),
	maplist(union_results, Grouped, VRPairs),
	maplist(map_resource(Map), SelectedValues0, SelectedValues).

map_key(Assoc, K0-V, K-V) :-
	(   get_assoc(K0, Assoc, K)
	->  true
	;   K = K0
	).

union_results(K-RL, K-R) :-
	append(RL, R0),
	sort(R0, R).

map_resource(Map, R0, R) :-
	(   get_assoc(R0, Map, R)
	->  true
	;   R = R0
	).

%%	facet_condition(+Facets, ?Resource, -Goal) is det.
%
%	Goal is an executable representation of   the  current Facets on
%	Resource.  Goal itself is semidet.
%
%	@param Facets is a list of prop(P,Values)

facet_condition([], _, true).
facet_condition([prop(P, Values)|T], R, (Goal->Rest)) :-
	findall(V, (member(V0, Values), owl_sameas(V0, V)), AllValues),
	pred_filter(AllValues, P, R, Goal),
	facet_condition(T, R, Rest).

pred_filter([Value], P, R, Goal) :- !,
	Goal = rdf_has(R, P, Value).
pred_filter([Value|Vs], P, R, Goal) :-
	Goal = (rdf_has(R, P, Value); Rest),
	pred_filter(Vs, P, R, Rest).


:- rdf_meta
	facet_exclude_property(r).

%facet_exclude_property(rdf:type).
facet_exclude_property(P) :-
	label_property(P).
facet_exclude_property(P) :-
	description_property(P).
facet_exclude_property(dc:identifier).
facet_exclude_property(skos:notation).
facet_exclude_property(owl:sameAs).
facet_exclude_property(rdf:value).
facet_exclude_property(P) :-
	cliopatria:facet_exclude_property(P).


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

%%	cliopatria:facet_exclude_property(+Property) is semidet.
%
%	True if Property must be excluded from creating a facet.

