% IMPORTANT, this module DOES declare a non empty public interface, but
% it is created dynamically after generating the predicates with
% `record/2`.
:- module(search_problem, []).
:- use_module(library(record)).

% When declaring a search_problem record e.g. with make_search_problem,
% it is imperative that you wrap your predicates with the module prefix,
% e.g. instead of =|goal(my_goal)|=, use =|goal(my_module:my_goal)|=.
:- record search_problem(
       % start/1, true if node is a goal.
       start,
       % goal/1, true if node is a goal.
       goal:callable,
       % children/2, children(Node, ChildNodes).
       children:callable,
       % h/2, h(Node1, ChildNodes).
       h:callable,
       % g/3, g(FromNode, ToNode, Cost).
       g:callable
   ).

predicates_in_file(Predicates) :-
    findall(X,
        (prolog_load_context(file, File),
         source_file(Pred, File),
         functor(Pred, Name, Arity),
         X=Name/Arity),
     Predicates).

private_predicate(private_predicate/1).
private_predicate(predicates_in_file/1).

:- predicates_in_file(Predicates),
   exclude(private_predicate, Predicates, PublicPredicates),
   maplist(search_problem:export, PublicPredicates).

