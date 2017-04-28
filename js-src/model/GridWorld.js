'use strict'
import Cell from 'model/Cell'
import Position, { p } from 'model/Position'
import Agent from 'model/Agent'
import { SearchEvents } from 'SearchAPI'
import GridSearchAPI from 'GridSearchAPI'
import log from 'util/log'
import Path from 'model/Path'
import GridEvents from 'events/GridEvents'
import PubSub from 'util/PubSub'
import AgentEvents from 'events/AgentEvents'

const DEBUG_TOPIC = 'GridWorld'

const GridWorldState = {
  setup: Symbol('grid-world-state-setup'),
  searching: Symbol('grid-world-state-searching'),
  searchComplete: Symbol('grid-world-search-complete')

}

class GridWorld {
  constructor () {
    this._gridSize = {
      width: 4,
      height: 4
    }
    this.searchType = 'bfs'
    this.state = GridWorldState.setup
    this.pubSub = new PubSub(GridEvents)
    this.agent = new Agent(p(1, 1))
    this.gridSize = this._gridSize
    this.agent.pubSub.addSubscriber(AgentEvents.goalPositionChanged, this.onSetGoalPosition.bind(this))
    this.searchApi = new GridSearchAPI('ws://localhost:4000/api')
    this.searchApi.pubSub.addSubscriber(SearchEvents.searchComplete, this.onSearchComplete.bind(this))
  }

  onSearchComplete (response) {
    return new Promise((resolve) => {
      log(DEBUG_TOPIC, 'Search complete')
      log(DEBUG_TOPIC, response)
      this.state = GridWorldState.searchComplete
      this.pubSub.notifySubscribers(GridEvents.searchComplete).then(resolve)
    })
  }

  onSetGoalPosition (oldGoalPosition, newGoalPosition) {
    if (oldGoalPosition.x <= this._gridSize.width && oldGoalPosition.y <= this._gridSize.height) {
      this.cell(oldGoalPosition).reset()
    }
    this.cell(newGoalPosition).goal()
    return new Promise((resolve) => resolve())
  }

  set gridSize (gridSize) {
    this._gridSize = gridSize
    this.grid = GridWorld.createGrid(gridSize.width, gridSize.height)
    this.pubSub.notifySubscribers(GridEvents.grid_size_change, gridSize)
    this.agent.goal = p(gridSize.width, gridSize.height)
    this.agent.startPosition = p(1, 1)
  }

  setAgentStartPosition (cell) {
    this.agent.startPosition = cell.position
  }

  startSearch () {
    this.state = GridWorldState.searching
    return this.searchApi.setupConnection().then(() => {
      return this.searchApi.setupGrid({
        size: this._gridSize,
        start: this.agent.startPosition,
        goal: this.agent.goal
      })
    }).then(() => {
      log(DEBUG_TOPIC, 'Initialising search')
      return this.searchApi.search(this.searchType)
    }).catch((error) => {
      log(DEBUG_TOPIC, 'Error: ', error)
    })
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

  step () {
    return this.searchApi.step().then((response) => {
      this.searchResponseCallback(response)
    })
  }

  reset () {
    this._resetState()
    if (this.state === GridWorldState.searching) {
      return this.searchApi.reset().then(() => {
      })
    } else {
      return new Promise((resolve) => resolve())
    }
  }

  _resetState () {
    this.agent.reset()
    for (let y = 0; y < this.grid.length; y++) {
      let row = this.grid[y]
      for (let x = 0; x < row.length; x++) {
        row[x].reset()
      }
    }
  }

  cell (position) {
    return this.grid[position.y - 1][position.x - 1]
  }

  searchResponseCallback (response) {
    let responseCode = response.response
    if (responseCode === 'ok') {
      let agenda = response.data.agenda
      return this.updateGrid(agenda)
    } else {
      log(DEBUG_TOPIC, 'Invalid response from server ' + JSON.stringify(response))
    }
  }

  updateGrid (agenda) {
    let bestAgendaItem = agenda[0]
    let path = new Path(bestAgendaItem.path.reverse())
    let currentMovePromise = new Promise((resolve) => resolve())
    path.forEach((position) => {
      this.agent.move(position)
      this.updateCellUnderAgent()
      currentMovePromise.then(() => {
        return this.pubSub.notifySubscribers(GridEvents.agent_moved, {
          agent: this.agent,
          position: position
        })
      })
    })
    return currentMovePromise.then(() => {
      this.pubSub.notifySubscribers(GridEvents.agent_path_followed, {
        agent: this.agent,
        path: path
      })
    })
  }

  cellUnderAgent (agent) {
    return this.grid[agent.position.y - 1][agent.position.x - 1]
  }

  updateCellUnderAgent () {
    let cell = this.cellUnderAgent(this.agent)
    cell.visited()
  }
}

export default GridWorld

