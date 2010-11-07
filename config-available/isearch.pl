:- module(conf_isearch, []).
:- use_module(isearch(applications/isearch)).
:- use_module(cliopatria(hooks)).

/** <module> Interactive search

Provide interactive search based on   property-chains and multi-facetted
browsing.

@author Michiel Hildebrand
*/

cliopatria:menu_item(250=query/isearch,  'Search').



