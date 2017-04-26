'use strict'
import Konva from 'konva'

class AnimationQueue {
  constructor () {
    this.pendingTweens = []
    this.animatationInProgress = false
  }

  addTween (tweenConfig) {
    this.pendingTweens.push(tweenConfig)
  }

  step () {
    return new Promise((resolve) => {
      this._step(resolve)
    })
  }

  _step (cb) {
    if (!this.animatationInProgress && this.pendingTweens.length > 0) {
      this.animatationInProgress = true
      let tweenConfig = this.pendingTweens.shift()
      tweenConfig.onFinish = () => {
        this.animatationInProgress = false
        this._step(cb)
      }
      let tween = new Konva.Tween(tweenConfig)
      tween.play()
    } else {
      cb()
    }
  }
}

export default AnimationQueue
