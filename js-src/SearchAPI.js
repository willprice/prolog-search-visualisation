'use strict'
/* global WebSocket */

import log from 'util/log'

const LOG_TOPIC = 'SearchApi'

function SearchApi (rootUrl, onsetup) {
  this.rootUrl = rootUrl
  this.connection = this.setupConnection(onsetup)
}

SearchApi.prototype.setupConnection = function (onsetup) {
  log(LOG_TOPIC, 'Setting up search API WebSocket connection')
  const connection = new WebSocket(this.rootUrl)
  connection.onerror = (error) => {
    log(LOG_TOPIC, `Failed to initialise connection to server at URL: ${this.rootUrl}`)
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

SearchApi.prototype.handleMessage = function (event) {
  try {
    const payload = JSON.parse(event.data)
    console.log(payload)
    log(LOG_TOPIC, `Received ${event.data}`)
  } catch (error) {
    log(LOG_TOPIC, `Failed to parse JSON from '${event.data}'`)
    log(LOG_TOPIC, error)
  }
}

SearchApi.prototype.sendCommand = function (command, args) {
  let payload = {
    command: command,
    args: args
  }
  this.connection.send(JSON.stringify(payload))
  log(LOG_TOPIC, `Sent ${JSON.stringify(payload)}`)
}

SearchApi.prototype.setup = function () {

}

