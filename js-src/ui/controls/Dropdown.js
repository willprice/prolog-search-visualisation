import PubSub from '../../util/PubSub'
'use strict'

const DropdownEvents = {
  selectionChanged: Symbol('dropdown-event-selection-changed')
}

class Dropdown {
  constructor (element, enabled) {
    this.element = element
    if (enabled !== undefined && !enabled) {
      this.element.disabled = true
    }
    this.element.onchange = this.onChange.bind(this)
    this.pubSub = new PubSub(DropdownEvents)
  }

  onChange () {
    this.pubSub.notifySubscribers(DropdownEvents.selectionChanged, this.element.value)
  }

  enable () {
    this.element.disabled = false
  }

  disable () {
    this.element.disabled = true
  }
}

export { Dropdown, DropdownEvents }
export default Dropdown
