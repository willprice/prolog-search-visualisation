:- module(search,
        [ search_depth_first/3
        , search_breadth_first/3
        , search_best_first/3
        , search_a/3
        , search/4
        ]).
:- use_module(grid).
:- use_module(library(pairs)).
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).

% Blind search
% ------------
% These search heuristics dont take new node viability into account
% (i.e. is the new node closer to the goal?)
%
:- meta_predicate search_depth_first(?, :, ?).
:- meta_predicate search_breadth_first(?, :, ?).
:- meta_predicate search_best_first(?, :, ?).
:- meta_predicate search_a(?, :, ?).
:- meta_predicate search(?, :, ?).

search_config_dfs(search_config{
    combine_agenda_3: combine_agenda_dfs,
    cost_4: cost_nop
}).

search_config_bfs(search_config{
    combine_agenda_3: combine_agenda_bfs,
    cost_4: cost_nop
}).

search_config_best_first(search_config{
    combine_agenda_3: combine_agendas,
    cost_4: cost_h
}).

search_config_a(search_config{
    combine_agenda_3: combine_agendas,
    cost_4: cost_a
}).


search_depth_first(Point, Goal, Path) :-
    search_config_dfs(Config),
    search(Config, Point, Goal, Path).

search_breadth_first(Point, Goal, Path) :-
    search_config_bfs(Config),
    search(Config, Point, Goal, Path).

search_best_first(Point, Goal, Path) :-
    search_config_best_first(Config),
    search(Config, Point, Goal, Path).

search_a(Point, Goal, Path) :-
    search_config_a(Config),
    search(Config, Point, Goal, Path).

% The agenda is represented by a list of pairs of agenda items
% Agenda items are represented as pairs: f(GCost, HCost)-Path
%   e.g. f(1, 3)-[p(1, 2), p(1, 1)] represents a current node of p(1,2)
%   starting from p(1, 1) with costs: g(p(1,2)) = 1 and h(p(1, 3)) = 3.

search(SearchConfig, Point, Goal, Path) :-
    Point = p(_, _),
    search(SearchConfig, [f(0, 0)-[Point]], Goal, [], Path).
search(_, [f(_, _)-[Current|PathTailReversed]|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathTailReversed], Path).
search(SearchConfig, Agenda, Goal, Visited, Path) :-
    select(f(CostToCurrent, _)-[Current|PathToCurrentReversed], Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_agenda(SearchConfig, CostToCurrent, Goal, AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search(SearchConfig, NewAgenda, Goal, UpdatedVisited, Path).

agenda_item(f(G, H), Current, PathTail, f(G, H)-[Current|PathTail]).

node_from_agenda_item(AgendaItem, Node) :-
    agenda_item(_, Node, _, AgendaItem).

cost_h(_, Goal, Node, f(0, Cost)) :-
    findall(G, call(Goal, G), Goals),
    maplist(distance(Node), Goals, Costs),
    min_list(Costs, Cost).

cost_nop(_, _, _, f(0, 0)).

cost_a(CostToCurrent, Goal, Node, f(CostToNode, Cost)) :-
    CostToNode #= CostToCurrent + 1,
    findall(G, call(Goal, G), Goals),
    maplist(distance(Node), Goals, Costs),
    min_list(Costs, Cost).

combine_agenda_dfs(OldAgenda, ChildrenAgenda, CombinedAgenda) :-
    append(ChildrenAgenda, OldAgenda, CombinedAgenda).

combine_agenda_bfs(OldAgenda, ChildrenAgenda, CombinedAgenda) :-
    append(OldAgenda, ChildrenAgenda, CombinedAgenda).

combine_agendas([], Agenda, Agenda) :- !.
combine_agendas(Agenda, [], Agenda) :- !.
combine_agendas([A1Best|A1Tail],
                [A2Best|A2Tail],
                [First,Second|PartialAgenda]) :-
    order_agenda_items(A1Best, A2Best, First, Second),
    combine_agendas(A1Tail, A2Tail, PartialAgenda).

order_agenda_items(f(G1, H1)-Path1, f(G2, H2)-Path2,
                   f(G1, H1)-Path1, f(G2, H2)-Path2) :-
   G1 + H1 #=< G2 + H2.
order_agenda_items(f(G1, H1)-Path1, f(G2, H2)-Path2,
                   f(G2, H2)-Path2, f(G1, H1)-Path1) :-
   G1 + H1 #> G2 + H2.


sort_agenda(UnsortedAgenda, SortedAgenda) :-
    predsort(agenda_comparison, UnsortedAgenda, SortedAgenda).

agenda_comparison(Delta, AgendaItem1, AgendaItem2) :-
    agenda_item(f(G1, H1), _, _, AgendaItem1),
    agenda_item(f(G2, H2), _, _, AgendaItem2),
    F1 #= G1 + H1,
    F2 #= G2 + H2,
    (F1 #> F2 -> Delta = '>'
    ;F1 #=< F2 -> Delta = '<'). % We don't let Delta be '=' since this will merge the items on the agenda.

update_agenda(SearchConfig, CostToCurrent, Goal, Agenda, Visited, Children, Path, NewAgenda) :-
    maplist(node_from_agenda_item, Agenda, NodesOnAgenda),
    exclude(\Child^(member(Child, NodesOnAgenda); member(Child, Visited)), Children, UnseenChildren),
    Cost_4 = SearchConfig.cost_4,
    CombineAgendas_3 = SearchConfig.combine_agenda_3,
    maplist(\Child^AgendaItem^(call(Cost_4, CostToCurrent, Goal, Child, Cost), agenda_item(Cost, Child, Path, AgendaItem)), UnseenChildren, UnseenAgenda),
    sort_agenda(UnseenAgenda, SortedUnseenAgenda),
    call(CombineAgendas_3, Agenda, SortedUnseenAgenda, NewAgenda).

% Debug hooks
% -----------
% prolog_trace_interception(Port, Frame, Choice, continue) :-
%     prolog_frame_attribute(Frame, goal, Goal),
%     print_debug_info(Port, Frame, Choice).
% prolog_trace_interception(Port, Frame, Choice, continue).
%
% print_debug_info(Port, Frame, Choice) :-
%     prolog_frame_attribute(Frame, goal, Goal),
%     format('port: ~p~n', [Port]),
%     format('goal: ~p~n', [Goal]),
%     format('frame ~p~n', [Frame]),
%     format('choice ~p~n', [Choice]).

% vim: set ft=prolog:
