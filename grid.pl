/** <module> Grid search problem

This module defines a [search problem](search_problem.pl) for use with the search predicates in search.pl

The following dynamic predicates *MUST* be declared by the calling
code:
- start/1.
- grid_size/2.
- goal/1.

@author Will Price
@license MIT
*/
:- module(grid,
    [ grid_size/2
    , goal/1
    , start/1
    , grid_search_problem/1
    ]).


%! start(?Pos:p(integer, integer)) is det.
%
%  Declare the starting position in the grid
:- dynamic start/1.   % e.g. start(p(1, 1)).

%! grid_size(?Width:integer, ?Height:integer) is det.
%
% Defines the grid size where:
% - X ranges from 1 to Width
% - Y ranges from 1 to Height
:- dynamic grid_size/2.        % e.g. grid_size(4, 5).

%! goal(?Pos:p(X:integer, Y:integer)) is nondet.
%
% Defines the target point (s) to reach.
:- dynamic goal/1.             % e.g. goal(p(3, 4)).

:- use_module(library(clpfd)).
:- use_module(search_problem).
:- use_module(json_serialisation).

:- multifile json_serialisation:to_json/2.
json_serialisation:to_json(p(X, Y), _{x: X, y: Y}).
json_serialisation:to_json(grid_size(Width, Height), _{ width: Width, height: Height}).

%! grid_search_problem(-SearchProblem) is det.
%
%  Get the search_problem description
grid_search_problem(SearchProblem) :-
    make_search_problem([
        start(grid:start),
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
