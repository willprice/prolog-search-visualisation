:- module(util, [ merge/4 ]).

:- meta_predicate merge(3, ?, ?, ?).

merge(_, [], SortedList2, SortedList2).
merge(_, SortedList1, [], SortedList1).
merge(Comparison, [E1|SortedList1Tail], [E2|SortedList2Tail], [First, Second|SortedCombined]) :-
    call(Comparison, Delta, E1, E2),
    (Delta = '<' -> First = E1, Second = E2
    ;Delta = '>' -> First = E2, Second = E1
    ;Delta = '=' -> First = E1, Second = E2),
    merge(Comparison, SortedList1Tail, SortedList2Tail, SortedCombined).

% vim: set ft=prolog:
