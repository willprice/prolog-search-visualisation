'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import CellUI from 'ui/CellUI'
import AgentUI from 'ui/AgentUI'
import Konva from 'konva'

const GridWorldUI = function (containerId, world) {
  this.world = world
  this.rows = []
  this.agents = []

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

  for (let i = 0; i < this.world.agents.length; i++) {
    this.agents.push(new AgentUI(this.agentLayer, this.world.agents[i]))
  }
}

GridWorldUI.prototype.render = function () {
  this.stage.draw()
  this.cellLayer.draw()
  this.agentLayer.draw()
}

export default GridWorldUI
