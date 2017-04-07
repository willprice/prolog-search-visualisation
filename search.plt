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

count(Element, List, Count) :-
    count(Element, List, 0, Count).

count(_, [], Count, Count).
count(Element, [Element|Tail], CurrentCount, AccCount) :-
    !,
    NewCount is CurrentCount + 1,
    count(Element, Tail, NewCount, AccCount).
count(Element, [_|Tail], CurrentCount, AccCount) :-
    count(Element, Tail, CurrentCount, AccCount).


search_depth_first(Paths) :-
    search_start(Start),
    findall(Path, search_depth_first(Start, goal, Path), Paths).

search_breadth_first(Paths) :-
    search_start(Start),
    findall(Path, search_breadth_first(Start, goal, Path), Paths).

%--------------------------------------
% Tests
%--------------------------------------
:- begin_tests(search).

test(search_depth_first_finds_at_least_one_path) :-
    search_depth_first(Paths),
    length(Paths, PathCount),
    assertion(PathCount >= 1).

test(search_depth_first_all_paths_valid) :-
    search_depth_first(Paths),
    maplist(valid_path, Paths).

test(search_depth_first_goal_not_visited_more_than_once) :-
    search_depth_first(Paths),
    maplist(goal_not_visited_more_than_once, Paths).

% test(search_breadth_first_finds_at_least_one_path) :-
%     search_breadth_first(Paths),
%     length(Paths, PathCount),
%     assertion(PathCount >= 1).
% 
% test(search_breadth_first_all_paths_valid) :-
%     search_breadth_first(Paths),
%     maplist(valid_path, Paths).
% 
% test(search_breadth_first_goal_not_visited_more_than_once) :-
%     search_breadth_first(Paths),
%     maplist(goal_not_visited_more_than_once, Paths).

:- end_tests(search).

% vim: set ft=prolog:
