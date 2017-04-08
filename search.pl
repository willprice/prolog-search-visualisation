:- module(search,
        [ search_depth_first/3
        , search_breadth_first/3
        , search_best_first/3
        , search_a/3
        ]).
:- use_module(grid).
:- use_module(library(pairs)).
:- use_module(library(lambda)).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).

% Blind search
% ------------
% These search heuristics dont take new node viability into account
% (i.e. is the new node closer to the goal?)
%
:- meta_predicate search_depth_first(?, :, ?).
:- meta_predicate search_breadth_first(?, :, ?).
:- meta_predicate search_best_first(?, :, ?).

search_depth_first(p(X, Y), Goal, Path) :-
    search_depth_first([p(X, Y)-[]], Goal, [], Path).
search_depth_first([Current-PathToGoalReversed|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathToGoalReversed], Path).
search_depth_first(Agenda, Goal, Visited, Path) :-
    select(Current-PathToCurrentReversed, Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_agenda(add_to_agenda_dfs, AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_depth_first(NewAgenda, Goal, UpdatedVisited, Path).


search_breadth_first(p(X, Y), Goal, Path) :-
    search_breadth_first([p(X, Y)-[]], Goal, [], Path).
search_breadth_first([Current-PathToGoalReversed|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathToGoalReversed], Path).
search_breadth_first(Agenda, Goal, Visited, Path) :-
    select(Current-PathToCurrentReversed, Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_agenda(add_to_agenda_bfs, AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_breadth_first(NewAgenda, Goal, UpdatedVisited, Path).

add_to_agenda_dfs(Item, Agenda, UpdatedAgenda) :-
    append([Item], Agenda, UpdatedAgenda).
add_to_agenda_bfs(Item, Agenda, UpdatedAgenda) :-
    append(Agenda, [Item], UpdatedAgenda).

update_agenda(_, Agenda, _, [], _, Agenda).
update_agenda(AgendaInsert, Agenda, Visited, [Child|ChildrenTail], Path, NewAgenda) :-
    pairs_keys(Agenda, NodesOnAgenda),
    \+ member(Child, NodesOnAgenda),
    \+ member(Child, Visited),
    update_agenda(AgendaInsert, Agenda, Visited, ChildrenTail, Path, NewPartialAgenda),
    call(AgendaInsert, Child-Path, NewPartialAgenda, NewAgenda).
update_agenda(AgendaInsert, Agenda, Visited, [Child|ChildrenTail], Path, NewAgenda) :-
    pairs_keys(Agenda, NodesOnAgenda),
    (member(Child, NodesOnAgenda);
     member(Child, Visited)),
    update_agenda(AgendaInsert, Agenda, Visited, ChildrenTail, Path, NewAgenda).

% Heuristic search
% ----------------

search_best_first(p(X, Y), Goal, Path) :-
    search_best_first([f(0, 0)-[p(X, Y)]], Goal, [], Path).
search_best_first([f(_, _)-[Current|PathTailReversed]|_], Goal, _, Path) :-
    call(Goal, Current),
    !,
    reverse([Current|PathTailReversed], Path).
search_best_first(Agenda, Goal, Visited, Path) :-
    select(f(_, _)-[Current|PathToCurrentReversed], Agenda, AgendaTail),
    UpdatedVisited = [Current|Visited],
    children(Current, ChildrenOfCurrent),
    update_f_agenda(Goal, AgendaTail, UpdatedVisited, ChildrenOfCurrent, [Current|PathToCurrentReversed], NewAgenda),
    search_best_first(NewAgenda, Goal, UpdatedVisited, Path).

search_a(p(X, Y), Goal, Path) :-
    search_best_first([(p(X, Y)-0)-[]], Goal, [], Path).

makepair(Fst, Snd, Fst-Snd).

agenda_item(f(G, H), Current, PathTail, f(G, H)-[Current|PathTail]).

node_from_agenda_item(AgendaItem, Node) :-
    agenda_item(_, Node, _, AgendaItem).

cost(Goal, Node, f(0, Cost)) :-
    findall(G, call(Goal, G), Goals),
    maplist(distance(Node), Goals, Costs),
    min_list(Costs, Cost).

order_agenda_items(f(G1, H1)-Path1, f(G2, H2)-Path2,
                   f(G1, H1)-Path1, f(G2, H2)-Path2) :-
   G1 + H1 #=< G2 + H2.
order_agenda_items(f(G1, H1)-Path1, f(G2, H2)-Path2,
                   f(G2, H2)-Path2, f(G1, H1)-Path1) :-
   G1 + H1 #> G2 + H2.

combine_agendas([], Agenda, Agenda) :- !.
combine_agendas(Agenda, [], Agenda) :- !.
combine_agendas([A1Best|A1Tail],
                [A2Best|A2Tail],
                [First,Second|PartialAgenda]) :-
    order_agenda_items(A1Best, A2Best, First, Second),
    combine_agendas(A1Tail, A2Tail, PartialAgenda).

sort_agenda(UnsortedAgenda, SortedAgenda) :-
    predsort(agenda_comparison, UnsortedAgenda, SortedAgenda).

agenda_comparison(Delta, AgendaItem1, AgendaItem2) :-
    agenda_item(f(G1, H1), _, _, AgendaItem1),
    agenda_item(f(G2, H2), _, _, AgendaItem2),
    F1 #= G1 + H1,
    F2 #= G2 + H2,
    (F1 #> F2 -> Delta = '>'
    ;F1 #=< F2 -> Delta = '<'). % We don't let Delta be '=' since this will merge the items on the agenda.

update_f_agenda(Goal, Agenda, Visited, Children, Path, NewAgenda) :-
    maplist(node_from_agenda_item, Agenda, NodesOnAgenda),
    exclude(\Child^(member(Child, NodesOnAgenda); member(Child, Visited)), Children, UnseenChildren),
    maplist(\Child^AgendaItem^(cost(Goal, Child, Cost), agenda_item(Cost, Child, Path, AgendaItem)), UnseenChildren, UnseenAgenda),
    sort_agenda(UnseenAgenda, SortedUnseenAgenda),
    combine_agendas(Agenda, SortedUnseenAgenda, NewAgenda).

% Debug hooks
% -----------
% prolog_trace_interception(Port, Frame, Choice, continue) :-
%     prolog_frame_attribute(Frame, goal, Goal),
%     print_debug_info(Port, Frame, Choice).
% prolog_trace_interception(Port, Frame, Choice, continue).
%
% print_debug_info(Port, Frame, Choice) :-
%     prolog_frame_attribute(Frame, goal, Goal),
%     format('port: ~p~n', [Port]),
%     format('goal: ~p~n', [Goal]),
%     format('frame ~p~n', [Frame]),
%     format('choice ~p~n', [Choice]).

% vim: set ft=prolog:
