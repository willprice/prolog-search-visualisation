'use strict'
import Konva from 'konva'
import { CELL_UI_CONFIG, AGENT_UI_CONFIG } from 'ui/gridConfig'
import AgentEvents from 'events/AgentEvents'
import promisify from 'util/promises'
import PubSub from 'util/PubSub'
import log from 'util/log'

const AgentUIEvents = Object.freeze({
  dragged: Symbol('agent-ui-event-dragged')
})
const DEBUG_TOPIC = 'AgentUI'

class AgentUI {
  constructor (layer, animationQueue, agent) {
    this.layer = layer
    this.rendered = false
    this.animationQueue = animationQueue
    this.agent = agent
    this.pubSub = new PubSub(AgentUIEvents)
    this.agent.pubSub.addSubscriber(AgentEvents.move, promisify(this.agentUpdateNotification.bind(this)))
    this.agent.pubSub.addSubscriber(AgentEvents.startPositionChanged, this.immediatelyUpdatePosition.bind(this))
    this.agent.pubSub.addSubscriber(AgentEvents.reset, this.immediatelyUpdatePosition.bind(this))
    this.agent.pubSub.addSubscriber(AgentEvents.backtrack, promisify(this.agentUpdateNotification.bind(this)))
    this.circle = this.render()
  }

  agentUpdateNotification () {
    this.update()
  }

  immediatelyUpdatePosition () {
    log(DEBUG_TOPIC, 'Updating agent position')
    this.circle.x(this._xFromAgent())
    this.circle.y(this._yFromAgent())
    this.layer.draw()
  }

  render () {
    if (this.rendered) {
      return this.circle
    }
    let circle = new Konva.Circle({
      x: this._xFromAgent(),
      y: this._yFromAgent(),
      radius: (CELL_UI_CONFIG.size / 2) - AGENT_UI_CONFIG.strokeWidth - AGENT_UI_CONFIG.padding,
      fill: AGENT_UI_CONFIG.color,
      stroke: AGENT_UI_CONFIG.strokeColor,
      strokeWidth: AGENT_UI_CONFIG.strokeWidth,
      draggable: true
    })
    circle.on('mouseover', () => {
      document.body.style.cursor = 'pointer'
    })
    circle.on('mouseout', () => {
      document.body.style.cursor = 'default'
    })
    circle.on('dragend', () => {
      this.pubSub.notifySubscribers(AgentUIEvents.dragged, circle.position())
    })
    this.layer.add(circle)
    this.rendered = true
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

export { AgentUI, AgentUIEvents }
export default AgentUI
