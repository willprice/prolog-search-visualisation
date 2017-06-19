# To Do

## Search algorithms

* [ ] Breadth first search; verify whether the current implementation is buggy
  or correct
* [ ] Best first search
* [ ] Iterative deepening
* [ ] Beam search
* [ ] Hill climbing


## UI

The whole UI is based on a flawed premise, it needs to be rebuilt from the
ground up, perhaps instead of rolling everything myself I should use
a framework to force me into certain well-tested patterns for development.

In addition to rewriting what we currently have I'd like to add the following:

* [ ] Visualisation of the agenda (perhaps as mini boards showing each path)
* [ ] Hover over each cell to get its g and h costs
* [ ] Animate the agent moving along the path
* [ ] Control agent speed
* [ ] Allow multiple goals
* [ ] Animate backtracking (maybe, does this make sense?)


## Misc

It would be nice to allow people to provide their own predicates defining cost
for A search so the can see how this effects the behaviour of the search
algorithm.

Better tests for each search strategy are needed to verify that they are indeed
implementing the chosen strategy, currently we just verify properties of the
paths
