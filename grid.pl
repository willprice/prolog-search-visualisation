:- module(grid,
    [ grid_size/2
    , goal/1
    , grid_search_problem/1
    ]).

:- dynamic grid_size/2.
:- dynamic goal/1.

:- use_module(library(clpfd)).
:- use_module(search_problem).

grid_search_problem(SearchProblem) :-
    make_search_problem([
        start(p(1, 1)),
        goal(grid:goal),
        children(grid:children),
        h(grid:h),
        g(grid:g)
    ], SearchProblem).

h(Node, Cost) :-
    findall(G, goal(G), Goals),
    maplist(distance(Node), Goals, Costs),
    min_list(Costs, Cost).

g(_From, _To, 1).

distance(p(X1, Y1), p(X2, Y2), Distance) :-
    Distance #= abs(X1  - X2) + abs(Y1 - Y2).

children(p(X, Y), Children) :-
    grid_size(Width, Height),
    findall(Child, child(p(X, Y), Width, Height, Child), Children).

child(CurrentPosition, Width, Height, NextPosition) :-
    next_position(CurrentPosition, NextPosition),
    valid_pos(NextPosition, Width, Height).

next_position(p(X, Y), p(NewX, Y)) :-
    NewX #= X + 1;
    NewX #= X - 1.
next_position(p(X, Y), p(X, NewY)) :-
    NewY #= Y + 1;
    NewY #= Y - 1.

valid_pos(p(X, Y), Width, Height) :-
    X =< Width, X > 0,
    Y =< Height, Y > 0.

% vim: set ft=prolog:
