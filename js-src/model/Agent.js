'use strict'

function Agent (position) {
  this.position = position
  this.world = null
}

Agent.prototype.move = function (position) {
  this.position = position
}

Agent.prototype.setWorld = function (world) {
  this.world = world
}

export default Agent
