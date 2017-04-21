'use strict'
import { p } from 'model/Position'

const GRID_CONFIG = {
  width: 4,
  height: 4,
  start: p(1, 1),
  goal: p(3, 4)
}

const AGENDA_HISTORY = [
  {
    current: p(1, 1),
    children: [
      p(2, 1),
      p(1, 2)
    ]
  },
  {
    current: p(2, 1),
    children: [
      p(3, 1),
      p(2, 2)
    ]
  },
  {
    current: p(3, 1),
    children: [
      p(3, 2),
      p(4, 1)
    ]
  },
  {
    current: p(3, 2),
    children: [
      p(3, 4),
      p(4, 2)
    ]
  },
  {
    current: p(3, 3),
    children: [
      p(3, 4),
      p(4, 3),
      p(2, 3)
    ]
  }
]

export { AGENDA_HISTORY, GRID_CONFIG }

