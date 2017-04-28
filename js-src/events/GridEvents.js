'use strict'

const GridEvents = Object.freeze({
  agent_moved: Symbol('grid-event-agent-moved'),
  agent_backtracked: Symbol('grid-event-agent-backtracked'),
  agent_path_followed: Symbol('grid-event-agent-path-followed'),
  grid_size_change: Symbol('grid-event-size-change'),
  searchComplete: Symbol('grid-world-state-search-complete')
})

export default GridEvents
