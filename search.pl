:- use_module(library(pairs)).


% pos(X, Y)
% grid_size(Width, Height).

% Blind search
% ------------
% These search heuristics dont take new node viability into account
% (i.e. is the new node closer to the goal?)

% WARNING: Doesn't terminate due to the way we decide what to put in the
% agenda, just gets caught going round in circles!
search_depth_first([Goal|_Rest], Goal, ReversedPathToGoal, Path) :-
    goal(Goal),
    reverse([Goal|ReversedPathToGoal], Path),
    !. % Cut because this definition is mutually exclusive with the
       % next, if we've reached the goal, lets not go away from it
search_depth_first(Agenda, Goal, ReversedPathToCurrent, Path) :-
    select(Current, Agenda, Rest),
    children(Current, Children),
    subtract(Children, ReversedPathToCurrent, ChildrenNotOnPath),
    append(ChildrenNotOnPath, Rest, NewAgenda),
    search_depth_first(NewAgenda, Goal, [Current|ReversedPathToCurrent], Path).

search_breadth_first(Agenda, Goal, Path) :-
    search_breadth_first(Agenda, Goal, [], Path).
search_breadth_first([Goal|_Rest], Goal, ReversedPathToGoal, Path) :-
    goal(Goal),
    reverse([Goal|ReversedPathToGoal], Path).
search_breadth_first(Agenda, Goal, ReversedPathToCurrent, Path) :-
    select(Current, Agenda, Rest),
    children(Current, Children),
    subtract(Children, ReversedPathToCurrent, ChildrenNotOnPath),
    append(Rest, ChildrenNotOnPath, NewAgenda),
    search_breadth_first(NewAgenda, Goal, [Current|ReversedPathToCurrent], Path).


% Heuristic search
% ----------------
search_best_first(Agenda, Goal, ReversedPathFragment, Path) :-
    goal(Goal),
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
    goal(Goal),
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
goal(p(3, 3)).
grid_size(3, 3).

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
