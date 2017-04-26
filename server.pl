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
:- use_module(search_problem).
:- use_module(search).


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
api_handler(Payload, ResponseWithId) :-
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'The payload ~p was received', [Payload]),
    api_handler(Payload.command, Payload.args, Response),
    put_dict(id, Response, Payload.id, ResponseWithId),
    debug(DebugTopic, 'The response ~p is ready to be sent', [Response]).

%% GRID Specific code

:- use_module(grid).

to_json(p(X, Y), _{x: X, y: Y}).


search_callback(CurrentItem, Agenda) :-
    debug('api-server', 'Current: ~p', [CurrentItem]),
    debug('api-server', 'Agenda: ~p', [Agenda]).

api_handler("search", Args, Response) :-
    !, % Do not backtrack into the catch all error api_handler
    debug_api_topic(DebugTopic),
    atom_string(SearchType, Args.algorithm),
    debug(DebugTopic, 'Search type ~p', [SearchType]),
    grid:grid_search_problem(SearchProblem),
    debug(DebugTopic, 'Search problem ~p', [SearchProblem]),
    (
        debug(DebugTopic, 'Starting search', []),
        search(SearchType, SearchProblem, search_callback, Path),
        debug(DebugTopic, 'Path ~p', [Path]),
        maplist(to_json, Path, JsonPath),
        Response = _{ response: ok, data: JsonPath }
    );(
        debug(DebugTopic, 'Not path found', []),
        Response = _{ response: search_fail, data: null }
    ),
    debug(DebugTopic, 'Responding with ~p', [Response]).


api_handler("grid:setup", Args, Response) :-
    !, % Do not backtrack into the catch all error api_handler
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'Setup grid with ~p', [Args]),
    GridSize = grid_size(Args.size.width,
                         Args.size.height),
    Start = p(Args.start.x, Args.start.y),
    Goal = p(Args.goal.x, Args.goal.y),
    grid_setup(GridSize, Start, Goal),
    Response = _{ response: ok, data: null },
    debug(DebugTopic, 'Responding with ~p', [Response]).


api_handler(Command, Args, Response) :-
    debug_api_topic(DebugTopic),
    debug(DebugTopic, 'No know handler for command ~p', [Command]),
    Response = _{ response: error_unknown_command,
                  data: _{
                    command: Command,
                    args: Args
                  }
                }.

grid_setup(GridSize, Start, Goal) :-
    retractall(grid:grid_size(_, _)),
    asserta(grid:GridSize),
    retractall(grid:start(_)),
    asserta(grid:start(Start)),
    retractall(grid:goal(_)),
    asserta(grid:goal(Goal)).

% vim: set ft=prolog:
