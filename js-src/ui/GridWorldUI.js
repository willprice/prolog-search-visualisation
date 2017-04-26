'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import CellUI from 'ui/CellUI'
import AgentUI from 'ui/AgentUI'
import Konva from 'konva'
import AnimationQueue from 'ui/AnimationQueue'

class GridWorldUI {
  constructor (containerId, world) {
    this.world = world
    this.rows = []
    this.animationQueue = new AnimationQueue()

    this.stage = new Konva.Stage({
      height: this.world.grid.length * CELL_UI_CONFIG.size,
      width: this.world.grid[0].length * CELL_UI_CONFIG.size,
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
        let cellUi = new CellUI(this.cellLayer, this.animationQueue, worldCell)
        this.rows.push(cellUi)
      }
    }

    this.agentUI = new AgentUI(this.agentLayer, this.animationQueue, this.world.agent)
    this.world.addAgendaUpdateListener(this)
  }

  agendaUpdateNotification () {
    this.step()
  }

  step () {
    this.animationQueue.step()
  }

  render () {
    this.stage.draw()
  }
}

export default GridWorldUI
