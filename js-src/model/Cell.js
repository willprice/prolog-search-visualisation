'use strict'
import CellEvents from 'events/CellEvents'
import PubSub from 'util/PubSub'

const CellStates = Object.freeze({
  default: Symbol('cell-state-default'),
  visited: Symbol('cell-state-visited'),
  goal: Symbol('cell-state-visited'),
  start: Symbol('cell-state-start')
})

class Cell {
  constructor (position) {
    this.position = position
    this.state = CellStates.default
    this.pubSub = new PubSub(CellEvents)
  }

  visited () {
    this.state = CellStates.visited
    this.pubSub.notifySubscribers(CellEvents.stateChange, this.state)
  }

  reset () {
    this.state = CellStates.default
    this.pubSub.notifySubscribers(CellEvents.stateChange, this.state)
  }

}

export default Cell
export { Cell, CellStates }
