/** <module> Black white puzzle

This search problem is about a logical puzzle comprising of a set of
wooden pieces in a line. There are 6 pieces and 7 positions along the
line. The puzzle starts in the following state:
=[b, b, b, e, w, w, w]= where:

  * =b= represents a black piece
  * =w= represents a white piece
  * =e= represents a space.

The goal of the puzzle is to get all the black pieces to the right of
the white pieces in as few moves as possible. The following states are
examples of valid end states (although there are many more):

  * =[w, w, w, e, b, b, b]=
  * =[w, w, w, b, e, b, b]=
  * =[w, w, e, w, b, b, b]=

@see [Simply Logical Chapter 6: Best first search](http://book.simply-logical.space/part_ii.html#best_first_search)
@author Peter Flach, Will Price
@license MIT
*/
:- module(black_white_puzzle,
    [ black_white_puzzle_search_problem/1
    ]).

:- use_module(library(clpfd)).
:- use_module(search_problem).

%! black_white_puzzle_search_problemove(-SearchProblem) is det.
%
%  Get the search_problem description
black_white_puzzle_search_problem(SearchProblem) :-
    make_search_problem([
        start(black_white_puzzle:start),
        goal(black_white_puzzle:goal),
        children(black_white_puzzle:children),
        h(black_white_puzzle:h),
        g(black_white_puzzle:g)
    ], SearchProblem).

start(move(initial_board, [black, black, black, empty, white, white, white], 0)).

goal(move(_LastBoard, Board, _Cost)) :-
    black_left_of_white(Board, Count),
    Count #= 0.

get_piece(Board, N, Piece) :-
    N #=< 7,
    nth1(N, Board, Piece).

children(OldMove, NextMoves) :-
    findall(NextMove, next_move(OldMove, NextMove), NextMoves).

g(_LastMove, move(_LastBoard, _CurrentBoard, Cost), Cost).

h(move(_LastBoard, CurrentBoard, _Cost), Cost) :-
    black_left_of_white(CurrentBoard, Cost).

next_move(move(_OldBoard, CurrentBoard, _OldCost),
      move(CurrentBoard, NewBoard, MoveCost)) :-
    get_piece(CurrentBoard, EmptyPosition, empty),
    get_piece(CurrentBoard, PiecePosition, Piece),
    \+ Piece = empty,
    D #= EmptyPosition - PiecePosition,
    ( D #< 0       -> Diff is -D
    ; otherwise -> Diff is D ),    % Diff is abs(Ne-Nbw)
    Diff #< 4,
    replace(CurrentBoard, EmptyPosition, Piece, TempBoard),
    replace(TempBoard, PiecePosition, empty, NewBoard),
    ( Diff=1    -> MoveCost=1
    ; otherwise -> MoveCost is Diff-1 ).

replace([_X|Xs] ,1 ,Y ,[Y|Xs]).
replace([X|Xs], N, Y,[X|Zs]):-
    N #> 1,N1 #= N - 1,
    replace(Xs,N1,Y,Zs).


black_left_of_white(Board, Value):-
    findall(black, (get_piece(Board, BlackPosition, black),
                    get_piece(Board, WhitePosition, white),
                    BlackPosition #< WhitePosition), BlacksLeftOfWhite),
    length(BlacksLeftOfWhite, Value).

%out_of_place(_Board, 8, N, N).
%out_of_place(Board, K, N0, N):-
%    K #< 8, K1 #= K + 1,
%    ( K<4,get_piece(Pos,K,b) -> N1 is N0-(K-4)
%    ; K>4,get_piece(Pos,K,w) -> N1 is N0+(K-4)
%    ; otherwise -> N1=N0 ),
%    out_of_place(Pos,K-1,N1,N).

% vim: set ft=prolog:
