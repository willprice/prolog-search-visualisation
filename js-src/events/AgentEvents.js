'use strict'

const AgentEvents = Object.freeze({
  move: Symbol('agent-event-move'),
  backtrack: Symbol('agent-event-backtrack'),
  reset: Symbol('agent-event-reset'),
  startPositionChanged: Symbol('agent-event-start-position-changed'),
  goalPositionChanged: Symbol('agent-event-goal-position-changed')
})

export default AgentEvents
