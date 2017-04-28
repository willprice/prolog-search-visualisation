# Search Visualisation in Prolog

[![Build Status](https://travis-ci.org/willprice/prolog-search-visualisation.svg?branch=master)](https://travis-ci.org/willprice/prolog-search-visualisation)

[SWI-Prolog](http://www.swi-prolog.org/) based tracer for visualisation search
algorithms such as depth first, breadth first, best first, A*, ... search

## Instructions

Install dependencies using `yarn` then run `./serve.sh` and visit
http://localhost:4000

Here's a demo of the visualisation:

![Search visualisation demo GIF](demo-2017-04-28.gif)

* The dark circle indicates the agent, it tracks the path through the search tree.
* Light blue cells indicate those that have been visited by the algorithm.
* Dark blue cells indicate those that are on the current path being explored.

* The width/height of the grid can be adjusted
* The search algorithm can be toggled through any that are implemented on the server
* The starting position of the agent can be chosen by dragging the agent to the
  desired starting cell
* The goal position can be selected by clicking on a cell.
* Searches can be aborted using the *reset* button

## Implementing new search algorithms

Search algorithms are abstractly defined by the prolog record [`search_strategy`](./search.pl#L59-L63). Search strategies define methods for ...

* Combining the current agenda with the agenda items created for the children
  reachable from the current state
* Computing the cost of an agenda item given `h` and `g` predicates
* Depth bounds

The following predicates define a search strategy:

* `combine_agenda(+OldAgenda:list(agenda_item), +ChildAgenda:list(agenda_item),
  -NewAgenda:list(agenda_item))` combines `OldAgenda`, the current agenda
  (minus the agenda item we popped off) and `ChildAgenda`, agenda items
  corresponding to the children found by `children/2` defined in the
  `search_problem`.
* `cost(+G:callable/3, +H:callable/3, +From:state, +CostToCurrent:integer,
  +To:state, -FCost:f(CostToNode:integer, HeuristicCostToGoal:integer))`
  evaluates the child state `To` reachable from `From` using `G` and `H`,
  predicates defining the cost of moving from `From` to `To` and from `To` to
  a goal. Note that `G` differs from the its traditional definition in the
  A algorithm literature, instead of computing the cost of a state from scratch
  it is much easier to compute the cost difference of a single move in the
  search true keeping a running total along a path.


## Implementing new search problems

Search problems are abstractly defined by the prolog record [`search_problem`](./search_problem.pl), there are *two* example problems:

* [grid search](./grid.pl)
* [black-white counter puzzle](./black_white_puzzle.pl) (see the [Simply
  Logical chapter](http://book.simply-logical.space/part_ii.html#informed_search) for
  an explanation)

Search problems are defined by 5 predicates:

* `start(-StartState:state)`, first argument unifies with the starting state of the search problem, e.g. the starting position of the agent in a grid search, or an initial board state in a game tree search.
* `goal(+State:state)` holds if `State` is a goal state or not.
* `children(+State:state, -ChildStates:list(state))` the reachable child states from `State`
* `h(+State, -Cost:integer)`, cost unifies with the
  [h-value](https://en.wikipedia.org/wiki/A*_search_algorithm) of `State`. The
  h-value is the heuristic estimate for how must it costs to reach the goal
  state from `State`.
* `g(+State:state, -Cost:integer)`, cost unifies with the cost of reaching
  `State` from the start state as defined by `start/1`.

The predicates are wrapped up into a `search_problem` record using `search_problem:make_search_record`.

The `state` type is user defined--the search algorithms are orthogonal to the representation, you can choose your state representation however you see fit.


## Contributing

See [DEVELOPMENT.md](./DEVELOPMENT.md) for technical details.
