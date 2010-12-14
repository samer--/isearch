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
	    facet_condition/3,		% +Facets, ?Resource, -Goal
	    facet_balance/2,		% +Facet, -Balance
	    facet_object_cardinality/2,	% +Facet, -Card
	    facet_frequency/3,		% +Facet, +TotalCount, -Freq
	    facet_weight/2		% +Facet, -Weight
	  ]).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/owl_sameas)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_description)).
:- use_module(library(stat_lists)).

/** <module> Computations for facetted search

This module computes facets  for  lists   of  resources  and  implements
various operations on facets.  A facet is represented as

    * prop(Property, Values)
    * prop(Property, Values, Selected)
*/

:- multifile
	cliopatria:facet_weight/2.	% ?Resource, ?Weight


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
	maplist(make_facet(Results), Grouped, Facets).

make_facet(Results, P-V_R, facet(P, V_RL, [])) :-
	group_pairs_by_key(V_R, V_RL0),
	(   findall(R, (member(R,Results),\+rdf_has(R,P,_)), NoP),
	    NoP \== []
	->  V_RL = ['__null'-NoP|V_RL0]
	;   V_RL = V_RL0
	).

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
	maplist(make_active_facet(Results, Filter), Grouped, Facets).

make_active_facet(Results, Filter, P-V_R, facet(P, V_RL, Selected)) :-
	memberchk(prop(P, Selected), Filter),
	group_pairs_by_key(V_R, V_RL0),
	(   findall(R, (member(R,Results),\+rdf_has(R,P,_)), NoP),
	    NoP \== []
	->  V_RL = ['__null'-NoP|V_RL0]
	;   V_RL = V_RL0
	).

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
	(   Values == ['__null']
	->  Goal = (\+ rdf(R,P,_))
	;   findall(V, (member(V0, Values), owl_sameas(V0, V)), AllValues),
	    pred_filter(AllValues, P, R, Goal)
	),
	facet_condition(T, R, Rest).

pred_filter([Value], P, R, Goal) :- !,
	Goal = rdf_has(R, P, Value).
pred_filter([Value|Vs], P, R, Goal) :-
	Goal = (rdf_has(R, P, Value); Rest),
	pred_filter(Vs, P, R, Rest).


		 /*******************************
		 *	      RANKING		*
		 *******************************/

%%	facet_balance(+Facet, -Balance) is det.
%%	facet_object_cardinality(+Facet, -Card) is det.
%%	facet_frequency(+Facet, +TotalResultCount, -Freq).
%
%	Balance is a number 0..1 that   expresses how wel the result-set
%	is distributed over the different values for the facet property.
%
%	Object cardinality prefers facets with   a  reasonable number of
%	alternatives. Note that the reference   below does *not* mention
%	good values for the constants Mu and Sigma.
%
%	Facet Frequency says something about the total number of results
%	covered by the facet relative to the total (search) result.
%
%	@see	Eyal Oren, Renaud Delbru, Stefan Decker: Extending
%		Faceted Navigation for RDF Data. International Semantic
%		Web Conference 2006: 559-572

facet_balance(facet(_P, V_R, _Selected), Balance) :-
	pairs_values(V_R, RLs),
	maplist(length, RLs, Counts),
	list_variance(Counts, Var),
	Balance is 1 - (Var/(1+Var)).

facet_object_cardinality(facet(_P, V_R, _Selected), Card) :-
	Mu = 10,
	Sigma = 40,
	length(V_R, NoP),
	(   NoP =< 1
	->  Card = 0
	;   Card is exp(-(((NoP-Mu)**2)/(2*Sigma**2)))
	).

facet_frequency(facet(_P, V_R, _Selected), Total, Freq) :-
	pairs_values(V_R, RLs),
	append(RLs, AllResults),
	sort(AllResults, Unique),
	length(Unique, UniqueCount),
	Freq is UniqueCount/Total.

%%	facet_weight(?P, ?Weight)
%
%	User contributed value that assesses the usefullness of a facet.

:- rdf_meta
	facet_weight(r, -).

facet_weight(P, 0) :-
	label_property(P).
facet_weight(P, 0) :-
	description_property(P).
facet_weight(dc:identifier, 0).
facet_weight(skos:notation, 0).
facet_weight(owl:sameAs, 0).
facet_weight(rdf:value, 0).
facet_weight(P, Weight) :-
	(   cliopatria:facet_weight(P, Weight0)
	->  Weight = Weight0
	;   Weight = 0.5
	).

facet_exclude_property(P) :-
	facet_weight(P, W),
	W =:= 0.


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

%%	cliopatria:facet_weight(+Property, -Weight) is semidet.
%
%	Expresses the usefullness of Property as a facet value.
%
%	@param Weight is a float between 0 and 1.  0 excludes the
%	       facet, while 1 makes the facet `ideal'.

