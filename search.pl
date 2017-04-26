/** <module> Search

Generic search algorithms

This module defines core search algorithms, e.g. *breadth-first search*,
*depth-first search*, *A search*.

Since most search algorithms share a very similar form, instead of
rewriting extremely similar predicates over and over again we instead
define a search algorithm that is parameterised over the different
aspects that vary to produce different search strategies. Think of this as
a search framework, a concrete search algorithm can be made by
instantiating the search framework with a *search strategy*.

A set of search strategies are already provided to implement:

  * Breadth first search
  * Depth first search
  * A search

You can create you own search strategies and use them with search/4.


## Creating a search strategy

A search strategy is defined by a search strategy record which
encapsulates all the predicates needed by the search framework:

  * combine_agenda/3
  * cost/6

## Describing a new search problem

A search problem is defined by a search problem record encapsulating all the
problem domain specifics implementing a common interface allowing the search
framework solve the problem.



@author Will Price
@license MIT
*/
:- module(search,
        [ search/3
        , search/4
        ]).


:- use_module(library(pairs)).
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).
:- use_module(library(record)).
:- use_module(util, [merge/4]).
:- use_module(search_problem).
:- use_module(json_serialisation).

:- record search_strategy(
       combine_agenda:callable,
       cost:callable
   ).
:- record agenda_item(
       path:list,
       g_cost:integer=0,
       h_cost:integer=0
   ).

:- multifile json_serialisation:to_json/2.
json_serialisation:to_json(AgendaItem, _{
        h_cost: HCost,
        g_cost: GCost,
        path: PathJson
    }) :-
        agenda_item_path(AgendaItem, Path),
        agenda_item_g_cost(AgendaItem, GCost),
        agenda_item_h_cost(AgendaItem, HCost),
        json_serialisation:to_json(Path, PathJson).

% Search strategy predicates
% --------------------------
search_strategy(dfs, Config) :-
    make_search_strategy([
        combine_agenda(combine_agenda_dfs),
        cost(cost_nop)
    ], Config).

search_strategy(bfs, Config) :-
    make_search_strategy([
        combine_agenda(combine_agenda_bfs),
        cost(cost_nop)
    ], Config).

search_strategy(best_first, Config) :-
    make_search_strategy([
        combine_agenda(combine_agendas),
        cost(cost_h)
    ], Config).

search_strategy(a, Config) :-
    make_search_strategy([
        combine_agenda(combine_agendas),
        cost(cost_a)
    ], Config).

cost_nop(_G, _H, _, _, _, f(0, 0)).

cost_h(_G, H, _, _, To, f(0, Cost)) :-
    call(H, To, Cost).

cost_a(G, H, From, CostToCurrent, To, f(CostToNode, Cost)) :-
    CostToNode #= CostToCurrent + MoveCost,
    call(G, From, To, MoveCost),
    call(H, To, Cost).

%! combine_agenda(+OldAgenda:list(agenda_item), +ChildAgenda:list(agenda_item), -NewAgenda:list(agenda_item)) is det.
%
% Combines OldAgenda with ChildAgenda to yield NewAgenda which is used
% in the next layer of the search.

combine_agenda_dfs(OldAgenda, ChildrenAgenda, CombinedAgenda) :-
    append(ChildrenAgenda, OldAgenda, CombinedAgenda).

combine_agenda_bfs(OldAgenda, ChildrenAgenda, CombinedAgenda) :-
    append(OldAgenda, ChildrenAgenda, CombinedAgenda).

combine_agendas(SortedAgenda1, SortedAgenda2, MergedAgendas) :-
    merge(agenda_comparison, SortedAgenda1, SortedAgenda2, MergedAgendas).

search(SearchType, SearchProblem, Path) :-
    DummyCallback = \_^_^(true),
    search(SearchType, SearchProblem, DummyCallback, Path).

:- meta_predicate search(+, +, 2, -).
search(SearchType, SearchProblem, Callback, Path) :-
    search_strategy(SearchType, SearchConfig),
    search_problem_start(SearchProblem, StartPredicate),
    call(StartPredicate, StartNode),
    make_agenda_item([path([StartNode]), g_cost(0), h_cost(0)], AgendaItem),
    search(SearchConfig, SearchProblem, Callback, [AgendaItem], [], Path).

% Search framework
% ----------------

search(_SearchConfig, SearchProblem, Callback, [TopAgendaItem|_], _, Path) :-
    must_be(ground, Callback),
    agenda_item_path(TopAgendaItem, [Current|PathTailReversed]),
    search_problem_goal(SearchProblem, Goal),
    call(Goal, Current),
    !,
    reverse([Current|PathTailReversed], Path).
search(SearchConfig, SearchProblem, Callback, Agenda, Visited, Path) :-
    must_be(ground, Callback),
    select(AgendaItem, Agenda, AgendaTail),
    agenda_item_path(AgendaItem, [Current|PathToCurrentReversed]),
    agenda_item_g_cost(AgendaItem, CostToCurrent),
    UpdatedVisited = [Current|Visited],
    search_problem_children(SearchProblem, ChildrenPredicate),
    call(ChildrenPredicate, Current, ChildrenOfCurrent),
    update_agenda(SearchConfig, SearchProblem, Current, CostToCurrent, AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    debug('search', 'Calling search callback ~p', [Callback]),
    catch(call(Callback, Current, NewAgenda), Error, print_message(informational, Error)),
    debug('search', 'Called search callback ~p', [Callback]),
    search(SearchConfig, SearchProblem, Callback, NewAgenda, UpdatedVisited, Path).

print_agenda_item(AgendaItem) :-
    agenda_item_path(AgendaItem, [Head|_Rest]),
    display(Head, HeadSimplified),
    writeln(HeadSimplified).

display(move(B1, B2, X), move(B1Simplified, B2Simplified, X)) :-
    simplify(B1, B1Simplified),
    simplify(B2, B2Simplified).

simplify([], []).
simplify([Head|Rest], [HeadSimplified|SimplifiedRest]) :-
    (Head = black -> HeadSimplified = b
    ;Head = white -> HeadSimplified = w
    ;Head = empty -> HeadSimplified = e),
    simplify(Rest, SimplifiedRest).


sort_agenda(UnsortedAgenda, SortedAgenda) :-
    predsort(agenda_comparison, UnsortedAgenda, SortedAgenda).

agenda_comparison(Delta, AgendaItem1, AgendaItem2) :-
    agenda_item_g_cost(AgendaItem1, G1),
    agenda_item_h_cost(AgendaItem1, H1),
    agenda_item_g_cost(AgendaItem2, G2),
    agenda_item_h_cost(AgendaItem2, H2),
    F1 #= G1 + H1,
    F2 #= G2 + H2,
    (F1 #> F2 -> Delta = '>'
    ;F1 #=< F2 -> Delta = '<'). % We don't let Delta be '=' since this will merge the items on the agenda.

update_agenda(SearchConfig, SearchProblem, Current, CostToCurrent, Agenda, Visited, Children, Path, NewAgenda) :-
    maplist(\AgendaItem^Node^(agenda_item_path(AgendaItem, [Node|_])), Agenda, NodesOnAgenda),
    exclude(\Child^(member(Child, NodesOnAgenda); member(Child, Visited)), Children, UnseenChildren),
    search_strategy_cost(SearchConfig, Cost_4),
    search_strategy_combine_agenda(SearchConfig, CombineAgendas_3),
    search_problem_g(SearchProblem, G),
    search_problem_h(SearchProblem, H),
    maplist(\Child^AgendaItem^(
        call(Cost_4, G, H, Current, CostToCurrent, Child, f(GCost, HCost)),
        make_agenda_item([path([Child|Path]), g_cost(GCost), h_cost(HCost)], AgendaItem)
    ), UnseenChildren, UnseenAgenda),
    sort_agenda(UnseenAgenda, SortedUnseenAgenda),
    call(CombineAgendas_3, Agenda, SortedUnseenAgenda, NewAgenda).

% vim: set ft=prolog:
