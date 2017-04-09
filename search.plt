search_start(p(1,1)).
:- asserta(grid:grid_size(3,3)).


:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(clpfd)).
:- use_module(grid).
:- use_module(search).

:- asserta(grid:goal(p(3,2))).

assert_valid_path([Point]) :-
    assertion(assert_valid_point(Point)).
assert_valid_path([P1, P2]) :-
    assertion(assert_valid_point(P1)),
    assertion(assert_valid_point(P2)).
assert_valid_path([P1,P2|Rest]) :-
    assertion(assert_valid_point(P1)),
    assertion(assert_valid_point(P2)),
    assertion(assert_valid_move(P1, P2)),
    assertion(assert_valid_path(Rest)).

assert_valid_move(p(X1, Y1), p(X2, Y2)) :-
    ((X1 #= X2 + 1; X1 #= X2 - 1),
      Y1 = Y2);
    ((Y1 #= Y2 + 1; Y1 #= Y2 - 1),
     X1 = X2).

assert_valid_point(p(X, Y)) :-
    grid:grid_size(Width, Height),
    X =< Width, X > 0,
    Y =< Height, Y > 0.

assert_goal_not_visited_more_than_once(Path) :-
    include(goal, Path, GoalOccurences),
    length(GoalOccurences, GoalOccurenceCount),
    assertion(GoalOccurenceCount =:= 1).

assert_last_element_on_path_is_goal(Path) :-
    last(Path, Last),
    goal(Last).

assert_first_element_on_path_is_start([Start|_]) :-
    search_start(Start).

assert_search_find_min_number_of_paths(SearchPredicate, MinPathCount) :-
    findall(Path, call(SearchPredicate, Path), Paths),
    length(Paths, PathCount),
    assertion(PathCount >= MinPathCount).

assert_path_properties(Path) :-
    assert_valid_path(Path),
    assert_goal_not_visited_more_than_once(Path),
    assert_first_element_on_path_is_start(Path),
    assert_last_element_on_path_is_goal(Path).


search_depth_first(Path) :-
    search_start(Start),
    grid_search_problem(SearchProblem),
    search_depth_first(Start, SearchProblem, Path).

search_breadth_first(Path) :-
    search_start(Start),
    grid_search_problem(SearchProblem),
    search_breadth_first(Start, SearchProblem, Path).

search_best_first(Path) :-
    search_start(Start),
    grid_search_problem(SearchProblem),
    search_best_first(Start, SearchProblem, Path).

search_a(Path) :-
    search_start(Start),
    grid_search_problem(SearchProblem),
    search_a(Start, SearchProblem, Path).

%--------------------------------------
% Tests
%--------------------------------------
:- begin_tests(search_depth_first).

test(search_depth_first_finds_at_least_one_path) :-
    assert_search_find_min_number_of_paths(search_depth_first, 1).

test(search_depth_first_all_paths_valid,
     [forall(search_depth_first(Path)), nondet]) :-
    assert_path_properties(Path).

:- end_tests(search_depth_first).

:- begin_tests(search_breadth_first).

test(search_breadth_first_finds_at_least_one_path) :-
    assert_search_find_min_number_of_paths(search_breadth_first, 1).

test(search_breadth_first_all_paths_valid,
     [forall(search_breadth_first(Path)), nondet]) :-
    assert_path_properties(Path).

:- end_tests(search_breadth_first).


:- begin_tests(search_best_first).

test(search_best_first_finds_at_least_one_path) :-
    assert_search_find_min_number_of_paths(search_best_first, 1).

test(search_best_first_all_paths_valid,
     [forall(search_best_first(Path)), nondet]) :-
    assert_path_properties(Path).

:- end_tests(search_best_first).

:- begin_tests(search_a).

test(search_a_finds_at_least_one_path) :-
    assert_search_find_min_number_of_paths(search_a, 1).

test(search_a_all_paths_valid,
     [forall(search_a(Path)), nondet]) :-
    assert_path_properties(Path).

:- end_tests(search_a).

% vim: set ft=prolog:
