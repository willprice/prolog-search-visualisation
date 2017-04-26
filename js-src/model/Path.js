'use strict'

class Path {
  constructor (path) {
    this.path = path
  }

  get length () {
    return this.path.length
  }

  get last () {
    return this.path[this.length - 1]
  }

  at (index) {
    return this.path[index]
  }

  longestCommonSubsequence (other) {
    let longestCommonSubsequence = []
    let minLength = Math.min(other.length, this.length)
    let i = 0
    while (i < minLength && this._pointsEqual(this.at(i), other.at(i))) {
      longestCommonSubsequence.push(this.at(i))
      i++
    }
    return longestCommonSubsequence
  }

  forEach (cb) {
    console.log(this.path)
    for (let i = 0; i < this.length; i++) {
      cb(this.at(i))
    }
  }

  _pointsEqual (p1, p2) {
    return p1.x === p2.x && p1.y === p2.y
  }
}

export default Path
