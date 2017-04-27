'use strict'
import Konva from 'konva'
import { CELL_UI_CONFIG, AGENT_UI_CONFIG } from 'ui/gridConfig'

class AgentUI {
  constructor (layer, animationQueue, agent) {
    this.layer = layer
    this.animationQueue = animationQueue
    this.agent = agent
    this.agent.addListener(this)
    this.circle = this.draw()
    this.animatationInProgress = false
  }

  agentUpdateNotification (agent) {
    this.update()
  }

  draw () {
    let circle = new Konva.Circle({
      x: this._xFromAgent(),
      y: this._yFromAgent(),
      radius: (CELL_UI_CONFIG.size / 2) - AGENT_UI_CONFIG.strokeWidth - AGENT_UI_CONFIG.padding,
      fill: AGENT_UI_CONFIG.color,
      stroke: AGENT_UI_CONFIG.strokeColor,
      strokeWidth: AGENT_UI_CONFIG.strokeWidth
    })
    this.layer.add(circle)
    return circle
  }

  _xFromAgent () {
    return (this.agent.position.x - 1 + 0.5) * CELL_UI_CONFIG.size
  }

  _yFromAgent () {
    return (this.agent.position.y - 1 + 0.5) * CELL_UI_CONFIG.size
  }

  update () {
    this.animationQueue.addTween({
      node: this.circle,
      x: this._xFromAgent(),
      y: this._yFromAgent()
    })
  }
}

export default AgentUI
