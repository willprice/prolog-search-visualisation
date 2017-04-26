'use strict'
/* global WebSocket */

import log from 'util/log'

const LOG_TOPIC = 'SearchApi'

class SearchAPI {
  constructor (rootUrl, onsetup) {
    this.rootUrl = rootUrl
    this.connection = this.setupConnection(onsetup)
    this._nextMessageId = 0
    this.awaitingResponse = new Map()
  }

  setupConnection (onsetup) {
    log(LOG_TOPIC, 'Setting up search API WebSocket connection')
    const connection = new WebSocket(this.rootUrl)
    connection.onerror = (error) => {
      log(LOG_TOPIC, `Failed to initialise connection to websocket server at URL: ${this.rootUrl}`)
      log(LOG_TOPIC, error)
    }
    connection.onmessage = this.handleMessage.bind(this)
    connection.onopen = () => {
      log(LOG_TOPIC, `WebSocket at address ${this.rootUrl} opened`)
      onsetup()
    }
    connection.onclose = () => {
      log(LOG_TOPIC, `WebSocket at address ${this.rootUrl} closed`)
    }
    return connection
  }

  handleMessage (event) {
    try {
      const payload = JSON.parse(event.data)
      log(LOG_TOPIC, `Received payload ${event.data}`)
      let cb = this.awaitingResponse[payload.id]
      if (cb !== undefined) {
        log(LOG_TOPIC, `Running callback for message ${payload.id}`)
        cb(payload)
      }
    } catch (error) {
      log(LOG_TOPIC, `Failed to parse JSON from '${event.data}'`)
      log(LOG_TOPIC, error)
    }
  }

  sendCommand (command, args, cb) {
    let messageId = this.nextMessageId()
    let payload = {
      command: command,
      args: args,
      id: messageId
    }
    this.awaitingResponse[messageId] = cb
    this.connection.send(JSON.stringify(payload))
    log(LOG_TOPIC, `Sent ${JSON.stringify(payload)}`)
  }

  nextMessageId () {
    return this._nextMessageId++
  }
}

export default SearchAPI
