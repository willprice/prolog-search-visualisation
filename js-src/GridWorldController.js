import GridParameterControls from 'ui/controls/GridParameterControls'
import GridWorld from 'model/GridWorld'
import { GridWorldUI, GridWorldUIEvents } from 'ui/GridWorldUI'
'use strict'

class GridWorldController {
  constructor () {
    this.gridParameterControls = new GridParameterControls(
      document.querySelector('.grid-parameter-controls')
    )
    this.gridWorld = new GridWorld()
    this.gridWorldUI = new GridWorldUI('grid', this.gridWorld)
    this.gridWorldUI.render()
    this.gridWorldUI.pubSub.addSubscriber(GridWorldUIEvents.agentDragged, this.moveAgentToCell.bind(this))
    this.gridParameterControls.addStartSubscriber(this.onStart.bind(this))
    this.gridParameterControls.addStepSubscriber(this.gridWorld.step.bind(this.gridWorld))
    this.gridParameterControls.addResetSubscriber(this.onReset.bind(this))
    this.gridParameterControls.addGridSizeSubscriber(this.onGridSizeChange.bind(this))
  }

  onStart () {
    return new Promise((resolve) => {
      this.gridWorld.startSearch().then(() => {
        this.enableSearchControls()
      }).then(resolve)
    })
  }

  onReset () {
    return new Promise((resolve) => {
      this.disableSearchControls()
      this.gridWorld.reset().then(resolve)
    })
  }

  moveAgentToCell (cell) {
    this.gridWorld.setAgentStartPosition(cell)
  }

  enableSearchControls () {
    this.gridParameterControls.startButton.disable()
    this.gridParameterControls.stepButton.enable()
    this.gridParameterControls.resetButton.enable()
  }

  disableSearchControls () {
    this.gridParameterControls.startButton.enable()
    this.gridParameterControls.stepButton.disable()
    this.gridParameterControls.resetButton.disable()
  }

  onGridSizeChange () {
    return new Promise((resolve) => {
      this.gridWorld.gridSize = {
        width: this.gridParameterControls.widthSlider.value,
        height: this.gridParameterControls.heightSlider.value
      }
      resolve()
    })
  }

}

export default GridWorldController
