'use strict'

const CellStates = Object.freeze({
  default: Symbol('cell-state-default'),
  visited: Symbol('cell-state-visited'),
  goal: Symbol('cell-state-visited')
})

class Cell {
  constructor (position) {
    this.position = position
    this.state = CellStates.default
    this.listeners = []
  }

  visited () {
    this.state = CellStates.visited
    this.notifyListeners()
  }

  addListener (listener) {
    this.listeners.push(listener)
  }

  notifyListeners () {
    for (const listener of this.listeners) {
      listener.cellUpdateNotification(this)
    }
  }

}

export default Cell
export { Cell, CellStates }
