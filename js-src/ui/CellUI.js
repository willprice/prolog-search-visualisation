'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import Konva from 'konva'
import CellEvents from 'events/CellEvents'
import PubSub from 'util/PubSub'
import promisify from 'util/promises'
import log from 'util/log'

const CellUIEvents = {
  cycleState: Symbol('cell-ui-event-state-change')
}
const DEBUG_TOPIC = 'CellUI'

class CellUI {
  constructor (layer, animationQueue, cell) {
    this.cell = cell
    this.animationQueue = animationQueue
    this.pubSub = new PubSub(CellUIEvents)
    this.cell.pubSub.addSubscriber(CellEvents.stateChange, promisify(this.cellUpdateNotification.bind(this)))
    this.layer = layer
    this.rect = this.render()
    this.interactable = true
    this.rendered = false
  }

  disableInteractions () {
    this.interactable = false
  }

  enableInteractions () {
    this.interactable = true
  }

  render () {
    if (this.rendered) {
      return this.rect
    }
    let width = CELL_UI_CONFIG.size
    let height = CELL_UI_CONFIG.size

    let x = (this.cell.position.x - 1) * width
    let y = (this.cell.position.y - 1) * height

    let rect = new Konva.Rect({
      x: x,
      y: y,
      width: width,
      height: height,
      fill: CELL_UI_CONFIG.color[this.cell.state],
      stroke: CELL_UI_CONFIG.strokeColor,
      strokeWidth: CELL_UI_CONFIG.strokeWidth
    })
    rect.on('click tap', () => {
      if (this.interactable) {
        log(DEBUG_TOPIC, 'Clicked cell: ', this.cell.position)
        this.pubSub.notifySubscribers(CellUIEvents.cycleState)
      }
    })
    this.layer.add(rect)
    this.rendered = true
    return rect
  }

  get canvasPosition () {
    return this.rect.position()
  }

  get canvasHeight () {
    return this.rect.height()
  }

  get canvasWidth () {
    return this.rect.width()
  }

  get canvasCenter () {
    return {
      x: this.canvasPosition.x + this.canvasWidth / 2,
      y: this.canvasPosition.y + this.canvasHeight / 2
    }
  }

  _xFromCell (cell) {
    return (cell.position.x - 1) * CELL_UI_CONFIG.size
  }

  _yFromCell (cell) {
    return (cell.position.y - 1) * CELL_UI_CONFIG.size
  }

  _fillFromCellState (cell) {
    return CELL_UI_CONFIG.color[cell.state]
  }

  update () {
    this.animationQueue.addTween({
      node: this.rect,
      x: this._xFromCell(this.cell),
      y: this._yFromCell(this.cell),
      fill: this._fillFromCellState(this.cell),
      duration: CELL_UI_CONFIG.frameTime
    })
  }

  cellUpdateNotification (state) {
    log(DEBUG_TOPIC, 'Cell UI updating ', this.cell.position, state)
    this.rect.fill(CELL_UI_CONFIG.color[state])
    this.rect.draw()
  }
}

export { CellUI, CellUIEvents }
export default CellUI
