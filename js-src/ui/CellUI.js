'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import Konva from 'konva'

class CellUI {

  constructor (layer, cell) {
    this.cell = cell
    this.cell.addListener(this)
    this.layer = layer
    this.rect = this.draw()
  }

  draw () {
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
    this.updates = []
    this.layer.add(rect)
    return rect
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
    let tween = new Konva.Tween({
      node: this.rect,
      x: this._xFromCell(this.cell),
      y: this._yFromCell(this.cell),
      fill: this._fillFromCellState(this.cell),
      duration: CELL_UI_CONFIG.frameTime
    })
    tween.play()
  }

  cellUpdateNotification (cell) {
    this.update()
  }
}

export default CellUI
