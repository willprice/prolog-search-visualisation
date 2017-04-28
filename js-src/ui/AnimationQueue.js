'use strict'
import Konva from 'konva'

class AnimationQueue {
  constructor () {
    this.pendingTweens = []
  }

  addTween (tweenConfig) {
    this.pendingTweens.push(tweenConfig)
  }

  step () {
    let executedTweenPromises = []
    while (this.pendingTweens.length > 0) {
      let executedTweenPromise = new Promise((resolve) => {
        let tweenConfig = this.pendingTweens.shift()
        tweenConfig.onFinish = resolve
        tweenConfig.duration = 0.1
        let tween = new Konva.Tween(tweenConfig)
        tween.play()
      })
      executedTweenPromises.push(executedTweenPromise)
    }
    return Promise.all(executedTweenPromises)
  }

  empty () {
    this.pendingTweens = []
  }
}

export default AnimationQueue
