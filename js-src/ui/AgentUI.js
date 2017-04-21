'use strict'
import Konva from 'konva'
import { CELL_UI_CONFIG, AGENT_UI_CONFIG } from 'ui/gridConfig'

class AgentUI {
  constructor (layer, agent) {
    this.layer = layer
    this.agent = agent
    this.draw()
  }

  draw () {
    let circle = new Konva.Circle({
      x: (this.agent.position.x - 1 + 0.5) * CELL_UI_CONFIG.size,
      y: (this.agent.position.y - 1 + 0.5) * CELL_UI_CONFIG.size,
      radius: (CELL_UI_CONFIG.size / 2) - AGENT_UI_CONFIG.strokeWidth - AGENT_UI_CONFIG.padding,
      fill: AGENT_UI_CONFIG.color,
      stroke: AGENT_UI_CONFIG.strokeColor,
      strokeWidth: AGENT_UI_CONFIG.strokeWidth
    })
    this.layer.add(circle)
  }
}

export default AgentUI
