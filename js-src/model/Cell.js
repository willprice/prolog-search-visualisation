'use strict'
import CellEvents from 'events/CellEvents'
import PubSub from 'util/PubSub'

const CellStates = {
  default: Symbol('cell-state-default'),
  visited: Symbol('cell-state-visited'),
  goal: Symbol('cell-state-goal'),
  start: Symbol('cell-state-start')
}

class Cell {
  constructor (position) {
    this.position = position
    this.state = CellStates.default
    this.pubSub = new PubSub(CellEvents)
  }

  visited () {
    this._updateState(CellStates.visited)
  }

  reset () {
    this._updateState(CellStates.default)
  }

  goal () {
    this._updateState(CellStates.goal)
  }

  _updateState (state) {
    this.state = state
    this.pubSub.notifySubscribers(CellEvents.stateChange, this.state)
  }

}

export default Cell
export { Cell, CellStates }
