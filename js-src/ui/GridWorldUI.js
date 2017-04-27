'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import CellUI from 'ui/CellUI'
import { AgentUI, AgentUIEvents } from 'ui/AgentUI'
import Konva from 'konva'
import AnimationQueue from 'ui/AnimationQueue'
import GridEvents from 'events/GridEvents'
import PubSub from 'util/PubSub'

const GridWorldUIEvents = {
  agentDragged: Symbol('grid-world-ui-event-agent-dragged')
}

class GridWorldUI {
  constructor (containerId, world) {
    this.world = world
    this.containerId = containerId
    this.gridUi = []
    this.animationQueue = new AnimationQueue()
    this.pubSub = new PubSub(GridWorldUIEvents)

    this.initialiseUI()
    this.createGridUI()

    this.world.pubSub.addSubscriber(GridEvents.agent_moved, this.runPendingAnimations.bind(this))
    this.world.pubSub.addSubscriber(GridEvents.agent_path_followed, this.runPendingAnimations.bind(this))
    this.world.pubSub.addSubscriber(GridEvents.grid_size_change, this.updateGridSize.bind(this))
  }

  initialiseUI () {
    this.stage = new Konva.Stage({
      height: this.world.grid.length * CELL_UI_CONFIG.size,
      width: this.world.grid[0].length * CELL_UI_CONFIG.size,
      container: this.containerId
    })

    this.cellLayer = new Konva.Layer()
    this.agentLayer = new Konva.Layer()

    this.stage.add(this.cellLayer)
    this.stage.add(this.agentLayer)
    this.agentUI = new AgentUI(this.agentLayer, this.animationQueue, this.world.agent)
    this.agentUI.pubSub.addSubscriber(AgentUIEvents.dragged, this.onAgentDragged.bind(this))
  }

  onAgentDragged (agentCanvasPosition) {
    let minDist = Infinity
    let closestCellUI

    function distance (p1, p2) {
      return Math.sqrt((p1.x - p2.x) ** 2 + (p1.y - p2.y) ** 2)
    }

    this.forEachCell((cellUI) => {
      let d = distance(cellUI.canvasCenter, agentCanvasPosition)
      if (d < minDist) {
        closestCellUI = cellUI
        minDist = d
      }
    })
    if (closestCellUI !== undefined) {
      this.pubSub.notifySubscribers(GridWorldUIEvents.agentDragged, closestCellUI.cell)
    }
  }

  createGridUI () {
    this.stage.destroy()
    this.initialiseUI()

    let gridUi = []
    for (let y = 0; y < this.world.grid.length; y++) {
      let worldRow = this.world.grid[y]
      let uiRow = []
      for (let x = 0; x < worldRow.length; x++) {
        let worldCell = worldRow[x]
        let cellUi = new CellUI(this.cellLayer, this.animationQueue, worldCell)
        uiRow.push(cellUi)
      }
      gridUi.push(uiRow)
    }
    this.gridUi = gridUi
    this.render()
  }

  updateGridSize (gridSize) {
    return new Promise((resolve) => {
      this.createGridUI()
      resolve()
    })
  }

  runPendingAnimations () {
    return this.animationQueue.step()
  }

  forEachCell (cb) {
    for (let row of this.gridUi) {
      for (let cellUi of row) {
        cb(cellUi)
      }
    }
  }

  render () {
    this.forEachCell((cell) => cell.render())
    this.stage.draw()
    this.cellLayer.draw()
    this.agentLayer.draw()
    this.agentUI.render()
  }
}

export { GridWorldUI, GridWorldUIEvents }
export default GridWorldUI
