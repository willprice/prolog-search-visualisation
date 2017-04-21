'use strict'
import { CELL_UI_CONFIG } from 'ui/gridConfig'
import Konva from 'konva'

class CellUI {

  constructor (layer, cell) {
    this.cell = cell
    this.layer = layer
    this.draw()
  }

  draw () {
    let x = this.cell.position.x
    let y = this.cell.position.y

    let width = CELL_UI_CONFIG.cellSize
    let height = CELL_UI_CONFIG.cellSize

    let rect = new Konva.Rect({
      x: (x - 1) * width,
      y: (y - 1) * height,
      width: width,
      height: height,
      fill: CELL_UI_CONFIG.cellColor,
      stroke: CELL_UI_CONFIG.cellStrokeColor,
      strokeWidth: CELL_UI_CONFIG.cellStrokeWidth
    })
    this.layer.add(rect)
  }
}

export default CellUI
