import PubSub from 'util/PubSub'
'use strict'

const SliderEvents = {
  change: Symbol('slider-event-change')
}

class Slider {
  constructor (element, min, max, defaultVal) {
    this.element = element
    this.element.min = min
    this.element.max = max
    if (defaultVal !== undefined && Number.isInteger(defaultVal)) {
      this.element.value = defaultVal
    } else {
      this.element.value = min
    }
    this.pubSub = new PubSub(SliderEvents)
    this.element.oninput = this._sliderChangeCallback.bind(this)
  }

  _sliderChangeCallback () {
    this.pubSub.notifySubscribers(SliderEvents.change, this.value)
  }

  enable () {
    this.element.disabled = false
  }

  disable () {
    this.element.disabled = true
  }

  get value () {
    return parseInt(this.element.value)
  }

}

export { Slider, SliderEvents }
