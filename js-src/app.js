'use strict'
import GridWorld from 'model/GridWorld'
import { p } from 'model/Position'
import GridWorldUI from 'ui/GridWorldUI'
import log from 'util/log'

log('App', 'Successfully loaded :)')

const gridConfig = {
  size: {
    width: 4,
    height: 5
  },
  start: p(1, 1),
  goal: p(3, 4)
}

const gridWorld = new GridWorld(gridConfig)
const gridWorldUI = new GridWorldUI('grid', gridWorld)
gridWorldUI.render()
