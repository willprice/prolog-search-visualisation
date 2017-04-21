'use strict'
import Cell from 'model/Cell'
import Position from 'model/Position'

class GridWorld {
  constructor (cols, rows) {
    this.grid = this.createGrid(cols, rows)
    this.agents = []
  }

  createGrid (cols, rows) {
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
  }
}

export default GridWorld

