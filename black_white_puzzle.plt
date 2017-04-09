:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(clpfd)).
:- use_module(black_white_puzzle).
:- use_module(search).

:- begin_tests(black_white_puzzle_children).

test(children_of_start_position) :-
    black_white_puzzle:start(InitialMove),
    InitialMove = move(_, OldBoard, _),
    black_white_puzzle:children(InitialMove, ChildMoves),
    member(move(OldBoard, [empty, black, black, black, white, white, white], _), ChildMoves),
    !.

:- end_tests(black_white_puzzle_children).

% vim: set ft=prolog:
