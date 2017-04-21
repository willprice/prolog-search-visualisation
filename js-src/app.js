'use strict'
import GridWorld from 'model/GridWorld'
import Agent from 'model/Agent'
import { p } from 'model/Position'
import GridWorldUI from 'ui/GridWorldUI'
import log from 'util/log'
import { GRID_CONFIG } from 'dummyAgendaHistory'

log('App', 'Successfully loaded :)')

const gridWorld = new GridWorld(GRID_CONFIG)
const agent = new Agent(p(1, 1))
gridWorld.addAgent(agent)

const gridWorldUI = new GridWorldUI('grid', gridWorld)
gridWorldUI.render()
agent.move(p(1, 2))
agent.move(p(1, 3))
agent.move(p(1, 4))
agent.move(p(2, 4))

