'use strict'

class Path {
  constructor (path) {
    this.path = path
  }

  last () {
    return this.path[this.path.length - 1]
  }

  first () {
    return this.path[0]
  }

  get length () {
    return this.path.length
  }

  at (index) {
    return this.path[index]
  }

  longestCommonSubPath (otherPath) {
    const minLength = Math.min(otherPath.length, this.length)
    let longestCommonSubPath = []
    for (let i = 0; i < minLength; i++) {
      if (this.at(i) === otherPath.at(i)) {
        longestCommonSubPath.push(this.at(i))
      }
    }
    return new Path(longestCommonSubPath)
  }
}

export default Path
