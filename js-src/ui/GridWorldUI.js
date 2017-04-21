'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import CellUI from 'ui/CellUI'
import Konva from 'konva'

const GridWorldUI = function (containerId, world) {
  this.world = world
  this.rows = []

  this.stage = new Konva.Stage({
    height: this.world.grid.length * CELL_UI_CONFIG.cellSize,
    width: this.world.grid[0].length * CELL_UI_CONFIG.cellSize,
    container: containerId
  })
  this.cellLayer = new Konva.Layer()
  this.agentLayer = new Konva.Layer()

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
