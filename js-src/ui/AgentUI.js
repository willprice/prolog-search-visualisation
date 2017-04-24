'use strict'
import Konva from 'konva'
import { CELL_UI_CONFIG, AGENT_UI_CONFIG } from 'ui/gridConfig'
import Animatable from './Animatable'

class AgentUI extends Animatable {
  constructor (layer, agent) {
    super()
    this.layer = layer
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
    // TODO: Figure out how to stage this transformation
    this._addPendingTween({
      node: this.circle,
      x: this._xFromAgent(),
      y: this._yFromAgent()
    })
    this.animate()
  }
}

export default AgentUI
