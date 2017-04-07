:- module(search,
        [ search_depth_first/3
        , search_breadth_first/3
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
    search_depth_first([p(X, Y)-[]], Goal, [p(X, Y)], Path).
search_depth_first([Current-PathToGoalReversed|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathToGoalReversed], Path).
search_depth_first(Agenda, Goal, Visited, Path) :-
    select(Current-PathToCurrentReversed, Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_agenda(AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_depth_first(NewAgenda, Goal, UpdatedVisited, Path).


search_breadth_first(p(X, Y), Goal, Path) :-
    search_breadth_first([p(X, Y)-[]], Goal, [p(X, Y)], Path).
search_breadth_first([Current-PathToGoalReversed|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathToGoalReversed], Path).
search_breadth_first(Agenda, Goal, Visited, Path) :-
    select(Current-PathToCurrentReversed, Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_agenda_bfs(AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_breadth_first(NewAgenda, Goal, UpdatedVisited, Path).

update_agenda(Agenda, _, [], _, Agenda).
update_agenda(Agenda, Visited, [Child|ChildrenTail], Path, [Child-Path|NewAgenda]) :-
    pairs_keys(Agenda, NodesOnAgenda),
    \+ member(Child, NodesOnAgenda),
    \+ member(Child, Visited),
    update_agenda(Agenda, Visited, ChildrenTail, Path, NewAgenda).
update_agenda(Agenda, Visited, [Child|ChildrenTail], Path, NewAgenda) :-
    pairs_keys(Agenda, NodesOnAgenda),
    (member(Child, NodesOnAgenda);
     member(Child, Visited)),
    update_agenda(Agenda, Visited, ChildrenTail, Path, NewAgenda).

update_agenda_bfs(Agenda, _, [], _, Agenda).
update_agenda_bfs(Agenda, Visited, [Child|ChildrenTail], Path, NewAgenda) :-
    pairs_keys(Agenda, NodesOnAgenda),
    \+ member(Child, NodesOnAgenda),
    \+ member(Child, Visited),
    update_agenda_bfs(Agenda, Visited, ChildrenTail, Path, NewPartialAgenda),
    append(NewPartialAgenda, [Child-Path], NewAgenda).
update_agenda_bfs(Agenda, Visited, [Child|ChildrenTail], Path, NewAgenda) :-
    pairs_keys(Agenda, NodesOnAgenda),
    (member(Child, NodesOnAgenda);
     member(Child, Visited)),
    update_agenda_bfs(Agenda, Visited, ChildrenTail, Path, NewAgenda).

% Heuristic search
% ----------------
search_best_first(Agenda, Goal, Path) :-
    search_best_first(Agenda, Goal, _, Path).
search_best_first([Goal|_], Goal, ReversedPathFragment, Path) :-
    reverse(Path, [Goal|ReversedPathFragment]),
    !.
search_best_first(Agenda, Goal, ReversedPathFragment, Path) :-
    select(CurrentBest, Agenda, AgendaTail),
    children(CurrentBest, Children),
    insert(heuristic, Goal, AgendaTail, Children, NewAgenda),
    search_best_first(NewAgenda, Goal, [CurrentBest|ReversedPathFragment], Path).


f(Heuristic, PathToCurrent, Goal, Current, Cost) :-
    length(PathToCurrent, CurrentPathCost),
    call(Heuristic, Goal, Current, HeuristicCost),
    Cost is CurrentPathCost + 1 + HeuristicCost.
%search_beam(Agenda, Goal).
search_a_star([Goal|_Rest], Goal, ReversedPathToGoal, Path) :-
    reverse([Goal|ReversedPathToGoal], Path).
search_a_star(Agenda, Goal, ReversedPathToCurrent, Path) :-
    select(CurrentBest, Agenda, AgendaTail),
    children(CurrentBest, Children),
    insert(f(heuristic, ReversedPathToCurrent), Goal, AgendaTail, Children, NewAgenda),
    search_a_star(NewAgenda, Goal, [CurrentBest|ReversedPathToCurrent], Path).

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
