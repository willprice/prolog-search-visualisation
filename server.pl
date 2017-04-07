% Instructions
% =swipl server.pl=
% =:- start_server.=


:- module(search_visualisation_server,
    [ start_server/0,
      stop_server/0
    ]).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).


start_server :-
    default_port(Port),
    start_server(Port).
start_server(Port) :-
    http_server(http_dispatch, [port(Port), workers(1)]).

stop_server :-
    default_port(Port),
    stop_server(Port).
stop_server(Port) :-
    http_stop_server(Port, []).

default_port(3000).


:- http_handler(root(.),
                http_reply_from_files('public', []),
                [priority(1), prefix]).
:- http_handler(root(api),
                http_upgrade_to_websocket(api_ws_handler, []),
                [priority(2), spawn([])]).
:- http_handler(root(.),
                http_404([]),
                [priority(0), prefix]).


api_ws_handler(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    writeln('Receiving message '),
    writeln(Message),
    ( Message.opcode == close
    -> true
    ; api_handler(Message.data, Response),
      ws_send(WebSocket, json(Response)),
      api_ws_handler(WebSocket)
    ).

debug_api_topic('API').
api_handler(Payload, Response) :-
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'The payload ~p was received', [Payload]),
    api_handler(Payload.command, Payload.args, Response),
    debug(DebugTopic, 'The response ~p is ready to be sent', [Payload]).

api_handler("setup", Args, Response) :-
    debug('API-setup', 'Setup called with ~p', [Args]),
    GridSize = grid_size(Args.grid_size.width,
                         Args.grid_size.height),
    StartPosition = p(Args.start_position.x, Args.start_position.y),
    GoalPosition = p(Args.goal.x, Args.goal.y),
    setup(GridSize, StartPosition, GoalPosition),
    Response = _{ response: ok, data: null },
    debug('API-setup', 'Responding with ~p', [Response]).


setup(GridSize, StartPosition, GoalPosition) :-
    asserta(GridSize),
    asserta(start_position(StartPosition)),
    asserta(goal_position(GoalPosition)).

% vim: set ft=prolog:
