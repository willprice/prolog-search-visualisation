'use strict'

function Cell(position) {
  this.position = position
}

Cell.prototype.move = function(position) {
  this.position = position
}

export default Cell
