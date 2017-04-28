import GridParameterControls from 'ui/controls/GridParameterControls'
import GridWorld from 'model/GridWorld'
import GridEvents from 'events/GridEvents'
import { GridWorldUI, GridWorldUIEvents } from 'ui/GridWorldUI'
import { CellStates } from 'model/Cell'
import { p } from 'model/Position'
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
    this.gridWorldUI.pubSub.addSubscriber(GridWorldUIEvents.cycleCellState, this.onCycleCellState.bind(this))
    this.gridParameterControls.addStartSubscriber(this.onStart.bind(this))
    this.gridParameterControls.addStepSubscriber(this.onStep.bind(this))
    this.gridParameterControls.addResetSubscriber(this.onReset.bind(this))
    this.gridParameterControls.addGridSizeSubscriber(this.onGridSizeChange.bind(this))
    this.gridParameterControls.addAlgorithmChangeSubscriber(this.onAlgorithmChange.bind(this))
    this.gridWorld.pubSub.addSubscriber(GridEvents.searchComplete, this.onSearchComplete.bind(this))
    this.gridWorld.agent.goal = p(1, 1)
  }

  onStart () {
    return new Promise((resolve) => {
      this.gridWorld.startSearch().then(() => {
        this.disableSetupControls()
        this.enableSearchControls()
        this.gridWorldUI.disableCellInteractions()
      }).then(resolve)
    })
  }

  onStep () {
    return new Promise((resolve) => {
      this.gridWorld.step().then((response) => {
        resolve()
      })
    })
  }

  onCycleCellState (cell) {
    switch (cell.state) {
      case CellStates.default:
        this.gridWorld.agent.goal = cell.position
        break
      case CellStates.goal:
        this.gridWorld.agent.goal = p(1, 1)
        cell.reset()
        break
    }
    return new Promise((resolve) => resolve())
  }

  onReset () {
    return new Promise((resolve) => {
      this.disableSearchControls()
      this.enableSetupControls()
      this.gridWorld.reset().then(() => {
        this.gridWorldUI.animationQueue.empty()
      })
    })
  }

  onAlgorithmChange (algorithmType) {
    return new Promise((resolve) => {
      this.gridWorld.searchType = algorithmType
      resolve()
    })
  }

  onSearchComplete () {
    return new Promise((resolve) => {
      this.disableStepControl()
      resolve()
    })
  }

  moveAgentToCell (cell) {
    this.gridWorld.setAgentStartPosition(cell)
  }

  disableSetupControls () {
    this.gridWorldUI.disableCellInteractions()
    this.gridParameterControls.algorithmDropdown.disable()
    this.gridParameterControls.heightSlider.disable()
    this.gridParameterControls.widthSlider.disable()
  }

  enableSetupControls () {
    this.gridWorldUI.enableCellInteractions()
    this.gridParameterControls.algorithmDropdown.enable()
    this.gridParameterControls.heightSlider.enable()
    this.gridParameterControls.widthSlider.enable()
  }

  enableSearchControls () {
    this.gridParameterControls.startButton.disable()
    this.gridParameterControls.stepButton.enable()
    this.gridParameterControls.resetButton.enable()
  }

  disableSearchControls () {
    this.gridParameterControls.startButton.enable()
    this.gridParameterControls.resetButton.disable()
    this.disableStepControl()
  }

  disableStepControl () {
    this.gridParameterControls.stepButton.disable()
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
