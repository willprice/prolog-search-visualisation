function Position (x, y) {
  this.x = x
  this.y = y
}

function p (x, y) {
  'use strict'
  return new Position(x, y)
}

export default Position
export { Position, p }
