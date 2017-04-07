:- module(search,
        [ search_depth_first/3
        , search_breadth_first/3
        , search_best_first/3
        ]).
:- use_module(library(pairs)).
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(assoc)).

:- dynamic search:grid_size/2.


grid_size(3, 3).

% Blind search
% ------------
% These search heuristics dont take new node viability into account
% (i.e. is the new node closer to the goal?)

goal(X) :- X = p(3,2).
search_depth_first(p(X, Y), Goal, Path) :-
    search_depth_first([p(X, Y)-[]], Goal, [], Path).
search_depth_first([Current-PathToGoalReversed|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathToGoalReversed], Path).
search_depth_first(Agenda, Goal, Visited, Path) :-
    select(Current-PathToCurrentReversed, Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_agenda(add_to_agenda_dfs, AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_depth_first(NewAgenda, Goal, UpdatedVisited, Path).


search_breadth_first(p(X, Y), Goal, Path) :-
    search_breadth_first([p(X, Y)-[]], Goal, [], Path).
search_breadth_first([Current-PathToGoalReversed|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathToGoalReversed], Path).
search_breadth_first(Agenda, Goal, Visited, Path) :-
    select(Current-PathToCurrentReversed, Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_agenda(add_to_agenda_bfs, AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_breadth_first(NewAgenda, Goal, UpdatedVisited, Path).

add_to_agenda_dfs(Item, Agenda, UpdatedAgenda) :-
    append([Item], Agenda, UpdatedAgenda).
add_to_agenda_bfs(Item, Agenda, UpdatedAgenda) :-
    append(Agenda, [Item], UpdatedAgenda).

update_agenda(_, Agenda, _, [], _, Agenda).
update_agenda(AgendaInsert, Agenda, Visited, [Child|ChildrenTail], Path, NewAgenda) :-
    pairs_keys(Agenda, NodesOnAgenda),
    \+ member(Child, NodesOnAgenda),
    \+ member(Child, Visited),
    update_agenda(AgendaInsert, Agenda, Visited, ChildrenTail, Path, NewPartialAgenda),
    call(AgendaInsert, Child-Path, NewPartialAgenda, NewAgenda).
update_agenda(AgendaInsert, Agenda, Visited, [Child|ChildrenTail], Path, NewAgenda) :-
    pairs_keys(Agenda, NodesOnAgenda),
    (member(Child, NodesOnAgenda);
     member(Child, Visited)),
    update_agenda(AgendaInsert, Agenda, Visited, ChildrenTail, Path, NewAgenda).

% Heuristic search
% ----------------

search_best_first(p(X, Y), Goal, Path) :-
    search_best_first([(p(X, Y)-0)-[]], Goal, [], Path).
search_best_first([(Current-_)-PathToGoalReversed|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathToGoalReversed], Path).
search_best_first(Agenda, Goal, Visited, Path) :-
    select((Current-_)-PathToCurrentReversed, Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_cost_agenda(AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_best_first(NewAgenda, Goal, UpdatedVisited, Path).

makepair(Fst, Snd, Pair) :-
    Pair = Fst-Snd.

child_to_agenda_item(Child, Cost, Path, AgendaItem) :-
    makepair(Child, Cost, ChildAgendaItem),
    makepair(ChildAgendaItem, Path, AgendaItem).

node_from_agenda_item(Node-_, Node).

cost(Node, Cost) :-
    findall(Goal, goal(Goal), Goals),
    maplist(distance(Node), Goals, Costs),
    min_list(Costs, Cost).

distance(p(X1, Y1), p(X2, Y2), Distance) :-
    Distance is abs(X1  - X2) + abs(Y1 - Y2).

combine_agendas([], Agenda, Agenda) :- !.
combine_agendas(Agenda, [], Agenda) :- !.
combine_agendas([(N1-N1Cost)-N1Path|A1Tail],
                [(N2-N2Cost)-N2Path|A2Tail],
                [(N1-N1Cost)-N1Path, (N2-N2Cost)-N2Path|PartialAgenda]) :-
    N1Cost  =< N2Cost,
    combine_agendas(A1Tail, A2Tail, PartialAgenda).
combine_agendas([(N1-N1Cost)-N1Path|A1Tail],
                [(N2-N2Cost)-N2Path|A2Tail],
                [(N2-N2Cost)-N2Path, (N1-N1Cost)-N1Path|PartialAgenda]) :-
    N1Cost > N2Cost,
    combine_agendas(A1Tail, A2Tail, PartialAgenda).

update_cost_agenda(Agenda, Visited, Children, Path, NewAgenda) :-
    pairs_keys(Agenda, AgendaItems),
    maplist(node_from_agenda_item, AgendaItems, NodesOnAgenda),
    exclude(\Child^(member(Child, NodesOnAgenda); member(Child, Visited)), Children, UnseenChildren),
    maplist(\Child^AgendaItem^(cost(Child, Cost), child_to_agenda_item(Child, Cost, Path, AgendaItem)), UnseenChildren, UnseenAgenda),
    combine_agendas(Agenda, UnseenAgenda, NewAgenda).

f(Heuristic, PathToCurrent, Goal, Current, Cost) :-
    length(PathToCurrent, CurrentPathCost),
    call(Heuristic, Goal, Current, HeuristicCost),
    Cost is CurrentPathCost + 1 + HeuristicCost.

heuristic(p(GoalX, GoalY), p(X, Y), Cost) :-
    Cost is abs(GoalX - X) + abs(GoalY - Y).

insert(_Heuristic, _Goal, [], [], _Agenda).
insert(Heuristic, Goal, [], Children, SortedChildren) :-
    sort_children(Heuristic, Goal, Children, SortedChildren).
insert(Heuristic, Goal, Agenda, Children, NewAgenda) :-
    sort_children(Heuristic, Goal, Children, SortedChildren),
    insert_sorted_children(Heuristic, Goal, Agenda, SortedChildren, NewAgenda).

insert_sorted_children(_Heuristic, _Goal, [], SortedChildren, SortedChildren).
insert_sorted_children(_Heuristic, _Goal, Agenda, [], Agenda).
insert_sorted_children(Heuristic, Goal, [Current|AgendaTail], [Child|ChildrenTail], [First, Second|NewAgendaTail]) :-
    call(Heuristic, Goal, Current, CurrentCost),
    call(Heuristic, Goal, Child, ChildCost),
    (ChildCost < CurrentCost ->
        (First = Child,
         Second = Current);
    (First = Current,
     Second = Child)),
    insert_sorted_children(Heuristic, Goal, AgendaTail, ChildrenTail, NewAgendaTail).

sort_children(Heuristic, Goal, Children, SortedChildren) :-
    BoundHeuristic = call(Heuristic, Goal),
    map_list_to_pairs(BoundHeuristic, Children, CostChildrenPairs),
    keysort(CostChildrenPairs, SortedCostChildrenPairs),
    pairs_values(SortedCostChildrenPairs, SortedChildren).






%search_beam(Agenda, Goal).
search_a_star([Goal|_Rest], Goal, ReversedPathToGoal, Path) :-
    reverse([Goal|ReversedPathToGoal], Path).
search_a_star(Agenda, Goal, ReversedPathToCurrent, Path) :-
    select(CurrentBest, Agenda, AgendaTail),
    children(CurrentBest, Children),
    insert(f(heuristic, ReversedPathToCurrent), Goal, AgendaTail, Children, NewAgenda),
    search_a_star(NewAgenda, Goal, [CurrentBest|ReversedPathToCurrent], Path).

% Grid specific
% -------------

children(p(X, Y), Children) :-
    grid_size(Width, Height),
    findall(Child, child(p(X, Y), Width, Height, Child), Children).

child(CurrentPosition, Width, Height, NextPosition) :-
    next_position(CurrentPosition, NextPosition),
    valid_pos(NextPosition, Width, Height).
next_position(p(X, Y), p(NewX, Y)) :-
    NewX is X + 1;
    NewX is X - 1.
next_position(p(X, Y), p(X, NewY)) :-
    NewY is Y + 1;
    NewY is Y - 1.

valid_pos(p(X, Y), Width, Height) :-
    X =< Width, X > 0,
    Y =< Height, Y > 0.

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
