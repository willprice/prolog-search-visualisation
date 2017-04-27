'use strict'
import PubSub from 'util/PubSub'

const ButtonEvents = {
  press: Symbol('button-event-press')
}

class Button {
  constructor (element, enabled) {
    this.pubSub = new PubSub(ButtonEvents)
    this.element = element
    if (enabled !== undefined && !enabled) {
      this.element.disabled = true
    }
    this.element.addEventListener('click', this._clickCallback.bind(this))
  }

  enable () {
    this.element.disabled = false
  }

  disable () {
    this.element.disabled = true
  }

  _clickCallback () {
    this.pubSub.notifySubscribers(ButtonEvents.press)
  }
}

export { Button, ButtonEvents }
