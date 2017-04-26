'use strict'
/* global WebSocket */

import log from 'util/log'

const LOG_TOPIC = 'SearchApi'

class SearchAPI {
  constructor (rootUrl) {
    this.rootUrl = rootUrl
    this.connection = null
    this._nextMessageId = 0
    this.awaitingResponse = new Map()
  }

  setupConnection () {
    log(LOG_TOPIC, 'Setting up search API WebSocket connection')
    return new Promise((resolve, reject) => {
      const connection = new WebSocket(this.rootUrl)
      connection.onerror = (error) => {
        const errorMessage = `Failed to initialise connection to websocket server at URL: ${this.rootUrl}`
        log(LOG_TOPIC, errorMessage)
        log(LOG_TOPIC, error)
        reject(errorMessage)
      }
      connection.onmessage = this.handleMessage.bind(this)
      connection.onopen = () => {
        log(LOG_TOPIC, `WebSocket at address ${this.rootUrl} opened`)
        resolve()
      }
      connection.onclose = () => {
        log(LOG_TOPIC, `WebSocket at address ${this.rootUrl} closed`)
      }
      this.connection = connection
    })
  }

  handleMessage (event) {
    log(LOG_TOPIC, `Received payload ${event.data}`)
    try {
      const payload = JSON.parse(event.data)
      let callback = this.awaitingResponse[payload.id]
      if (callback !== undefined) {
        log(LOG_TOPIC, `Running callback for message ${payload.id}`)
        callback(payload)
      }
    } catch (error) {
      log(LOG_TOPIC, `Failed to parse JSON from '${event.data}'`)
      log(LOG_TOPIC, error)
    }
  }

  sendCommand (command, args, cb) {
    let messageId = this.newMessageId()
    let payload = {
      command: command,
      args: args,
      id: messageId
    }
    this.awaitingResponse[messageId] = cb
    this.connection.send(JSON.stringify(payload))
    log(LOG_TOPIC, `Sent ${JSON.stringify(payload)}`)
  }

  search (algorithm, cb) {
    this.sendCommand('search', {
      algorithm: algorithm
    }, cb)
  }

  step () {
    this.sendCommand('step', null)
  }

  reset () {
    this.sendCommand('reset', null)
  }

  newMessageId () {
    return this._nextMessageId++
  }
}

export default SearchAPI
