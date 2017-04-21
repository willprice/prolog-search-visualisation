'use strict'
import GridWorld from 'model/GridWorld'
import Agent from 'model/Agent'
import Position from 'model/Position'
import GridWorldUI from 'ui/GridWorldUI'
import log from 'util/log'
import Konava from 'konva'

const gridWorld = new GridWorld(4, 4)
const agent = new Agent(new Position(1, 1))
agent.setWorld(gridWorld)

const gridStage = new Konava.Stage({
  container: 'grid',
  width: 500,
  height: 500
})

const gridWorldUI = new GridWorldUI(gridStage, gridWorld)
gridWorldUI.render()

log('App', 'Successfully loaded :)')
