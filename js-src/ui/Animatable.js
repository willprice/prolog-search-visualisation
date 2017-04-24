'use strict'
import Konva from 'konva'

class Animatable {
  constructor () {
    this.tweenConfigQueue = []
  }

  _addPendingTween (config) {
    this.tweenConfigQueue.push(config)
  }

  animate () {
    if (!this.animatationInProgress && this.tweenConfigQueue.length > 0) {
      this.animatationInProgress = true
      let tweenConfig = this.tweenConfigQueue.shift()
      tweenConfig.onFinish = () => {
        this.animatationInProgress = false
        this.animate()
      }
      let tween = new Konva.Tween(tweenConfig)
      tween.play()
    }
  }
}

export default Animatable
