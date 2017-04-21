'use strict'
import { Cell, CellStates } from 'model/Cell'

import Position from 'model/Position'

class GridWorld {
  constructor (config) {
    this.config = config
    this.grid = GridWorld.createGrid(config.width, config.height)
    this.agents = []
    this.setupGoalCell()
    this.setupStartCell()
  }

  setupGoalCell () {
    this.grid[this.config.goal.y - 1][this.config.goal.x - 1].state = CellStates.goal
  }

  setupStartCell () {
    this.grid[this.config.start.y - 1][this.config.start.x - 1].state = CellStates.visited
  }

  static createGrid (cols, rows) {
    let grid = []
    for (let y = 1; y <= rows; y++) {
      let row = []
      for (let x = 1; x <= cols; x++) {
        let position = new Position(x, y)
        row.push(new Cell(position))
      }
      grid.push(row)
    }
    return grid
  }

  addAgent (agent) {
    this.agents.push(agent)
    agent.addListener(this)
  }

  agentUpdateNotification (agent) {
    let cell = this.cellUnderAgent(agent)
    cell.visited()
  }

  cellUnderAgent (agent) {
    return this.grid[agent.position.y - 1][agent.position.x - 1]
  }
}

export default GridWorld

