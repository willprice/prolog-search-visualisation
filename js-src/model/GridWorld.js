'use strict'
import { Cell, CellStates } from 'model/Cell'
import Position from 'model/Position'
import Agent from 'model/Agent'
import GridSearchAPI from 'GridSearchAPI'
import log from 'util/log'
import Path from 'model/Path'

const DEBUG_TOPIC = 'GridWorld'

class GridWorld {
  constructor (config) {
    this.config = config
    this.grid = GridWorld.createGrid(config.size.width, config.size.height)
    this.agent = new Agent(config.start)
    this.searchApi = new GridSearchAPI('ws://localhost:4000/api')
    this.agendaUpdateListeners = []
    this.previousAgenda = null
    this.setupGoalCell()
    this.setupStartCell()
    this.setupSearch()
  }

  setupSearch () {
    this.searchApi.setupConnection().then(() => {
      return this.searchApi.setupGrid(this.config)
    }).then(() => {
      return this.searchApi.search('bfs', this.agendaUpdateNotification.bind(this))
    }).then((path) => {
      log('Search', path)
    })
  }

  agendaUpdateNotification (response) {
    let responseCode = response.response
    if (responseCode === 'ok') {
      let agenda = response.data
      this.updateGrid(agenda)
      for (let listener of this.agendaUpdateListeners) {
        listener.agendaUpdateNotification(this)
      }
      setTimeout(this.searchApi.step.bind(this.searchApi), 1000)
      this.previousAgenda = agenda
    } else {
      log(DEBUG_TOPIC, 'Invalid response from server ' + JSON.stringify(response))
    }
  }

  addAgendaUpdateListener (listener) {
    this.agendaUpdateListeners.push(listener)
  }

  updateGrid (agenda) {
    let bestAgendaItem = agenda[0]
    let path = new Path(bestAgendaItem.path.reverse())
    this.agent.followPath(path)
  }

  setupGoalCell () {
    this.grid[this.config.goal.y - 1][this.config.goal.x - 1].state = CellStates.goal
  }

  setupStartCell () {
    this.grid[this.config.start.y - 1][this.config.start.x - 1].state = CellStates.visited
  }

  static createGrid (cols, rows) {
    let grid = []
    for (let y = 1; y <= rows; y++) {
      let row = []
      for (let x = 1; x <= cols; x++) {
        let position = new Position(x, y)
        row.push(new Cell(position))
      }
      grid.push(row)
    }
    return grid
  }

  agentUpdateNotification (agent) {
    let cell = this.cellUnderAgent(agent)
    cell.visited()
  }

  cellUnderAgent (agent) {
    return this.grid[agent.position.y - 1][agent.position.x - 1]
  }
}

export default GridWorld

