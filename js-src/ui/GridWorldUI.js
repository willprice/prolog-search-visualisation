'use strict'
import CellUI from 'ui/CellUI'
import Konava from 'konva'

const GridWorldUI = function (stage, world) {
  this.world = world
  this.rows = []

  this.stage = stage
  this.cellLayer = new Konava.Layer()
  this.agentLayer = new Konava.Layer()

  this.stage.add(this.cellLayer)
  this.stage.add(this.agentLayer)

  for (let y = 0; y < this.world.grid.length; y++) {
    let worldRow = this.world.grid[y]
    for (let x = 0; x < worldRow.length; x++) {
      let worldCell = worldRow[x]
      let cellUi = new CellUI(this.cellLayer, worldCell)
      this.rows.push(cellUi)
    }
  }
}

GridWorldUI.prototype.render = function () {
  this.stage.draw()
  this.cellLayer.draw()
  this.agentLayer.draw()
}

export default GridWorldUI
