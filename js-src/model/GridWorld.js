'use strict'
import Cell from 'model/Cell'
import Position from 'model/Position'


function GridWorld(cols, rows) {
  this.grid = this.createGrid(cols, rows)
}

GridWorld.prototype.createGrid = function (cols, rows) {
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
export default GridWorld
