#!/usr/bin/env swipl

% Instructions
% =swipl server.pl=
% =:- start_server.=

% We leverage websockets for communications, to see a simple example of
% how this works in SWI-prolog, visit
% https://gist.github.com/willprice/0b9ec66ce59799b7632dd1a224bd485e
% and play around with the small example.

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
                [priority(0), spawn([])]).
:- http_handler(root(.),
                http_404([]),
                [priority(0), prefix]).


api_ws_handler(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'Receiving message ~p', [Message]),
    ( Message.opcode == close
    -> true
    ; api_handler(Message.data, Response),
      ws_send(WebSocket, json(Response)),
      debug(DebugTopic, 'The response ~p was sent', [Response]),
      api_ws_handler(WebSocket)
    ).

debug_api_topic('api-server').
api_handler(Payload, Response) :-
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'The payload ~p was received', [Payload]),
    api_handler(Payload.command, Payload.args, Response),
    debug(DebugTopic, 'The response ~p is ready to be sent', [Response]).

%% GRID Specific code

api_handler("grid:setup", Args, Response) :-
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'Setup grid with ~p', [Args]),
    GridSize = grid_size(Args.size.width,
                         Args.size.height),
    StartPosition = p(Args.start.x, Args.start.y),
    GoalPosition = p(Args.goal.x, Args.goal.y),
    setup(GridSize, StartPosition, GoalPosition),
    Response = _{ response: ok, data: null },
    debug(DebugTopic, 'Responding with ~p', [Response]),
    !. % Do not backtrack into the catch all error api_handler

api_handler(Command, Args, Response) :-
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'No know handler for command ~p', [Command]),
    Response = _{ response: error_unknown_command,
                  data: _{ 
                    command: Command,
                    args: Args
                  }
                }.


setup(GridSize, StartPosition, GoalPosition) :-
    asserta(GridSize),
    asserta(start_position(StartPosition)),
    asserta(goal_position(GoalPosition)).

% vim: set ft=prolog:
