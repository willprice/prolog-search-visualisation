'use strict'
import Path from './Path'
import AgentEvents from 'events/AgentEvents'
import PubSub from 'util/PubSub'
import { p } from './Position'

class Agent {
  constructor (position) {
    this._position = position
    this._start = position
    this._goal = p(1, 1)
    this.pubSub = new PubSub(AgentEvents)
    this.world = null
    this.previousPath = new Path([])
  }

  setWorld (world) {
    this.world = world
  }

  get position () {
    return this._position
  }

  get startPosition () {
    return this._start
  }

  set startPosition (position) {
    this._start = position
    this._position = position
    this.pubSub.notifySubscribers(AgentEvents.startPositionChanged, position)
  }

  set goal (position) {
    let oldGoal = this._goal
    this._goal = position
    this.pubSub.notifySubscribers(AgentEvents.goalPositionChanged, oldGoal, this._goal)
  }

  get goal () {
    return this._goal
  }

  reset () {
    this._position = this._start
    return this.pubSub.notifySubscribers(AgentEvents.reset, this._position)
  }

  backtrack (position) {
    this._position = position
    return this.pubSub.notifySubscribers(AgentEvents.backtrack, position)
  }

  move (position) {
    this._position = position
    return this.pubSub.notifySubscribers(AgentEvents.move)
  }

}

export default Agent
