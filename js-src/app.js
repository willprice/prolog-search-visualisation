'use strict'
import GridWorld from 'model/GridWorld'
import Agent from 'model/Agent'
import Position from 'model/Position'
import GridWorldUI from 'ui/GridWorldUI'
import log from 'util/log'

const gridWorld = new GridWorld(4, 4)
const agent = new Agent(new Position(1, 1))
gridWorld.addAgent(agent)

const gridWorldUI = new GridWorldUI('grid', gridWorld)
gridWorldUI.render()

log('App', 'Successfully loaded :)')
