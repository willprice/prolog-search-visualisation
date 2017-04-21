prolog_trace_interception(Port, Frame, Choice, continue) :-
    prolog_frame_attribute(Frame, goal, Goal),
    print_debug_info(Port, Frame, Choice).
prolog_trace_interception(Port, Frame, Choice, continue).

print_debug_info(Port, Frame, Choice) :-
    prolog_frame_attribute(Frame, goal, Goal),
    format('port: ~p~n', [Port]),
    format('goal: ~p~n', [Goal]),
    format('frame ~p~n', [Frame]),
    format('choice ~p~n', [Choice]).

% vim: set ft=prolog:
