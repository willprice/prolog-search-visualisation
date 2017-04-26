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
}

export default Path
