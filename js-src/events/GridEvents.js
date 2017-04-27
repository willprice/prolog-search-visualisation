'use strict'

const GridEvents = Object.freeze({
  agent_moved: Symbol('grid-event-agent-moved'),
  agent_path_followed: Symbol('grid-event-agent-path-followed'),
  grid_size_change: Symbol('grid-event-size-change')
})

export default GridEvents
