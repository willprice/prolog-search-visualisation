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
    return {
      common: new Path(longestCommonSubsequence),
      restOfThis: new Path(this.path.slice(i, this.length)),
      restOfOther: new Path(other.path.slice(i, other.length))
    }
  }

  reverse () {
    return new Path(this.path.reverse())
  }

  forEach (cb) {
    for (let i = 0; i < this.length; i++) {
      cb(this.at(i))
    }
  }

  _pointsEqual (p1, p2) {
    return p1.x === p2.x && p1.y === p2.y
  }
}

export default Path
