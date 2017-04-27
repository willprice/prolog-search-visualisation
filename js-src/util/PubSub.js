'use strict'

class PubSub {
  constructor (events) {
    this.subscribers = PubSub.initialiseSubscriberMap(events)
  }

  static initialiseSubscriberMap (events) {
    let subscribers = new Map()
    for (let key of Object.keys(events)) {
      let event = events[key]
      subscribers[event] = []
    }
    return subscribers
  }

  addSubscriber (event, callback) {
    this.subscribers[event].push(callback)
  }

  // The pile of hell fire that is the promisification of the PubSub class is thanks to Konva's lake of synchronous
  // animations.
  notifySubscribers (event, data) {
    let promises = []
    for (let subscriber of this.subscribers[event]) {
      promises.push(subscriber(data))
    }
    return Promise.all(promises)
  }
}

export default PubSub
