'use strict'

class Agent {
  constructor (position) {
    this._position = position
    this.positionListeners = []
    this.world = null
  }

  setWorld (world) {
    this.world = world
  }

  get position () {
    return this._position
  }

  addListener (listener) {
    this.positionListeners.push(listener)
  }

  move (position) {
    this._position = position
    this.notifyListeners()
  }

  notifyListeners () {
    for (const listener of this.positionListeners) {
      listener.agentUpdateNotification(this)
    }
  }
}

export default Agent
