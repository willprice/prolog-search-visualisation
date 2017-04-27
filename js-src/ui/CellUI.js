'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import Konva from 'konva'
import CellEvents from 'events/CellEvents'
import promisify from 'util/promises'

class CellUI {
  constructor (layer, animationQueue, cell) {
    this.cell = cell
    this.animationQueue = animationQueue
    this.cell.pubSub.addSubscriber(CellEvents.stateChange, promisify(this.cellUpdateNotification.bind(this)))
    this.layer = layer
    this.rect = this.render()
  }

  render () {
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
    this.layer.add(rect)
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

  cellUpdateNotification () {
    this.update()
  }
}

export default CellUI
