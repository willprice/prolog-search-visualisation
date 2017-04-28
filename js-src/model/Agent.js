'use strict'
import Path from './Path'
import AgentEvents from 'events/AgentEvents'
import PubSub from 'util/PubSub'
import { p } from './Position'

class Agent {
  constructor (position) {
    this._position = position
    this._start = position
    this._goal = p(1, 1)
    this.pubSub = new PubSub(AgentEvents)
    this.world = null
    this.previousPath = new Path([])
  }

  setWorld (world) {
    this.world = world
  }

  get position () {
    return this._position
  }

  get startPosition () {
    return this._start
  }

  set startPosition (position) {
    this._start = position
    this._position = position
    this.pubSub.notifySubscribers(AgentEvents.startPositionChanged, position)
    this.pubSub.notifySubscribers(AgentEvents.reset, position)
  }

  set goal (position) {
    let oldGoal = this._goal
    this._goal = position
    this.pubSub.notifySubscribers(AgentEvents.goalPositionChanged, oldGoal, this._goal)
  }

  get goal () {
    return this._goal
  }

  /**
   * We visit a new path by backtracking to the start node and following the new path,
   * If the new path and previous path share a common subsequence from the beginning
   * of the paths, then we only back track to the latest common ancestor
   * @param path
   */
  followPath (path) {
    let longestCommonSubsequence = path.longestCommonSubsequence(this.previousPath)
    this._backtrackPath(longestCommonSubsequence.restOfOther)
    if (longestCommonSubsequence.common.length > 0) {
      this.move(longestCommonSubsequence.common.last)
    }
    this._followPath(longestCommonSubsequence.restOfThis)
    this.previousPath = path
  }

  /**
   * Follow path blindly, do not perform any backtracking, we simply execute move for
   * each position in path.
   * @param path
   * @private
   */
  _followPath (path) {
    path.forEach((position) => {
      this.move(position)
    })
  }

  /**
   * Backtrack over path iterating from the end of the path to the beginning
   * @param path
   * @private
   */
  _backtrackPath (path) {
    path.reverse().forEach((position) => {
      this.backtrack(position)
    })
  }

  reset () {
    this._position = this.startPosition
    this.pubSub.notifySubscribers(AgentEvents.reset, this._position)
  }

  backtrack (position) {
    this._position = position
    this.pubSub.notifySubscribers(AgentEvents.backtrack, position)
  }

  move (position) {
    this._position = position
    this.pubSub.notifySubscribers(AgentEvents.move)
  }

}

export default Agent
