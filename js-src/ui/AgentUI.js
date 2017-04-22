'use strict'
import Konva from 'konva'
import { CELL_UI_CONFIG, AGENT_UI_CONFIG } from 'ui/gridConfig'

class AgentUI {
  constructor (layer, agent) {
    this.layer = layer
    this.agent = agent
    this.agent.addListener(this)
    this.circle = this.draw()
    this.tweenConfigs = []
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
    this.addTween({
      node: this.circle,
      x: this._xFromAgent(),
      y: this._yFromAgent()
    })
    this.animate()
  }

  addTween (tweenConfig) {
    this.tweenConfigs.push(tweenConfig)
  }

  animate () {
    if (!this.animatationInProgress && this.tweenConfigs.length > 0) {
      this.animatationInProgress = true
      let tweenConfig = this.tweenConfigs.shift()
      tweenConfig.onFinish = () => {
        this.animatationInProgress = false
        this.animate()
      }
      console.log(tweenConfig)
      let tween = new Konva.Tween(tweenConfig)
      tween.play()
    }
  }
}

export default AgentUI
