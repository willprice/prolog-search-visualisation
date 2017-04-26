'use strict'
import Path from './Path'

class Agent {
  constructor (position) {
    this._position = position
    this.positionListeners = []
    this.world = null
    this.previousPath = new Path([])
  }

  setWorld (world) {
    this.world = world
  }

  get position () {
    return this._position
  }

  addListener (listener) {
    this.positionListeners.push(listener)
  }

  /**
   * We visit a new path by backtracking to the start node and following the new path,
   * If the new path and previous path share a common subsequence from the beginning
   * of the paths, then we only back track to the latest common ancestor
   * @param path
   */
  followPath (path) {
    let longestCommonSubsequence = path.longestCommonSubsequence(this.previousPath)
    console.log(longestCommonSubsequence)
    this._backtrack(longestCommonSubsequence.restOfOther)
    if (longestCommonSubsequence.common.length > 0) {
      this.move(longestCommonSubsequence.common.last)
    }
    this._followPath(longestCommonSubsequence.restOfThis)
    this.previousPath = path
  }

  /**
   * Backtrack over path iterating from the end of the path to the beginning
   * @param path
   * @private
   */
  _backtrack (path) {
    this._followPath(path.reverse())
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

  move (position) {
    this._position = position
    this.notifyListeners()
  }

  notifyListeners () {
    for (const listener of this.positionListeners) {
      listener.agentUpdateNotification(this)
    }
  }
}

export default Agent
