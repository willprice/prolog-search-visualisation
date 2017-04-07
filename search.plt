:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(search).


search_start(p(1,1)).
goal(P) :-
    P = p(3, 2).


valid_path([Point]) :-
    assertion(valid_point(Point)).
valid_path([P1, P2]) :-
    assertion(valid_point(P1)),
    assertion(valid_point(P2)).
valid_path([P1,P2|Rest]) :-
    assertion(valid_point(P1)),
    assertion(valid_point(P2)),
    assertion(valid_move(P1, P2)),
    assertion(valid_path(Rest)).

valid_move(p(X1, Y1), p(X2, Y2)) :-
    ((X1 is X2 + 1; X1 is X2 - 1),
      Y1 = Y2);
    ((Y1 is Y2 + 1; Y1 is Y2 - 1),
     X1 = X2).

valid_point(p(X, Y)) :-
    search:grid_size(Width, Height),
    X =< Width, X > 0,
    Y =< Height, Y > 0.

goal_not_visited_more_than_once(Path) :-
    include(goal, Path, GoalOccurences),
    length(GoalOccurences, GoalOccurenceCount),
    assertion(GoalOccurenceCount =:= 1).

search_depth_first(Path) :-
    search_start(Start),
    search_depth_first(Start, goal, Path).

search_breadth_first(Path) :-
    search_start(Start),
    search_breadth_first(Start, goal, Path).

search_best_first(Path) :-
    search_start(Start),
    search_best_first(Start, goal, Path).

%--------------------------------------
% Tests
%--------------------------------------
:- begin_tests(search_depth_first).

test(search_depth_first_finds_at_least_one_path) :-
    findall(Path, search_depth_first(Path), Paths),
    length(Paths, PathCount),
    assertion(PathCount >= 1).

test(search_depth_first_all_paths_valid,
     [forall(search_depth_first(Path)), nondet]) :-
    valid_path(Path).

test(search_depth_first_goal_not_visited_more_than_once,
     [forall(search_depth_first(Path)), nondet]) :-
    goal_not_visited_more_than_once(Path).

test(search_depth_first_last_element_of_path_is_goal,
     [forall(search_depth_first(Path)), nondet]) :-
    goal_not_visited_more_than_once(Path),
    last(Path, Last),
    goal(Last).

:- end_tests(search_depth_first).


:- begin_tests(search_breadth_first).

test(search_breadth_first_finds_at_least_one_path) :-
    findall(Path, search_breadth_first(Path), Paths),
    length(Paths, PathCount),
    assertion(PathCount >= 1).

test(search_breadth_first_all_paths_valid,
     [forall(search_breadth_first(Path)), nondet]) :-
    valid_path(Path).

test(search_breadth_first_goal_not_visited_more_than_once,
     [forall(search_breadth_first(Path)), nondet]) :-
    goal_not_visited_more_than_once(Path).

test(search_breadth_first_last_element_of_path_is_goal,
     [forall(search_breadth_first(Path)), nondet]) :-
    goal_not_visited_more_than_once(Path),
    last(Path, Last),
    goal(Last).

:- end_tests(search_breadth_first).


:- begin_tests(search_best_first).

test(search_best_first_finds_at_least_one_path) :-
    findall(Path, search_best_first(Path), Paths),
    length(Paths, PathCount),
    assertion(PathCount >= 1).

test(search_best_first_all_paths_valid,
     [forall(search_best_first(Path)), nondet]) :-
    valid_path(Path).

test(search_best_first_goal_not_visited_more_than_once,
     [forall(search_best_first(Path)), nondet]) :-
    goal_not_visited_more_than_once(Path).

test(search_best_first_last_element_of_path_is_goal,
     [forall(search_best_first(Path)), nondet]) :-
    goal_not_visited_more_than_once(Path),
    last(Path, Last),
    goal(Last).

:- end_tests(search_best_first).
% vim: set ft=prolog:
