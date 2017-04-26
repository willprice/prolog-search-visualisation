:- module(json_serialisation, [ to_json/2 ]).

:- multifile to_json/2.
to_json([], []).
to_json([Head|Tail], [HeadJson|TailJson]) :-
  to_json(Head, HeadJson),
  to_json(Tail, TailJson).

% vim: set ft=prolog:
