'use strict'
import { Button, ButtonEvents } from 'ui/controls/Button'
import { Slider, SliderEvents } from 'ui/controls/Slider'
import { Dropdown, DropdownEvents } from './Dropdown'

class GridParameterControls {
  constructor (element) {
    this.element = element
    this.startButton = new Button(element.querySelector('.start-button'), true)
    this.stepButton = new Button(element.querySelector('.step-button'), false)
    this.resetButton = new Button(element.querySelector('.reset-button'), false)
    this.heightSlider = new Slider(element.querySelector('.height-slider'), 1, 20, 4)
    this.widthSlider = new Slider(element.querySelector('.width-slider'), 1, 20, 4)
    this.algorithmDropdown = new Dropdown(element.querySelector('select'), true)
  }

  addStepSubscriber (cb) {
    this.stepButton.pubSub.addSubscriber(ButtonEvents.press, cb)
  }

  addResetSubscriber (cb) {
    this.resetButton.pubSub.addSubscriber(ButtonEvents.press, cb)
  }

  addAlgorithmChangeSubscriber (cb) {
    this.algorithmDropdown.pubSub.addSubscriber(DropdownEvents.selectionChanged, cb)
  }

  addGridSizeSubscriber (cb) {
    this.heightSlider.pubSub.addSubscriber(SliderEvents.change, cb)
    this.widthSlider.pubSub.addSubscriber(SliderEvents.change, cb)
  }

  addStartSubscriber (cb) {
    this.startButton.pubSub.addSubscriber(ButtonEvents.press, cb)
  }
}

export default GridParameterControls
