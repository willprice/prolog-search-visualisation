import { CellStates } from 'model/Cell'

const CELL_UI_CONFIG = {
  size: 50,
  color: {},
  strokeColor: '#aaa',
  strokeWidth: 1,
  frameTime: 0.2
}
CELL_UI_CONFIG.color[CellStates.default] = '#eee'
CELL_UI_CONFIG.color[CellStates.visited] = '#0ab'
CELL_UI_CONFIG.color[CellStates.path] = '#449'
CELL_UI_CONFIG.color[CellStates.goal] = '#f44'

const AGENT_UI_CONFIG = {
  color: '#555',
  strokeColor: '#555',
  strokeWidth: 1,
  padding: 5
}

export { AGENT_UI_CONFIG, CELL_UI_CONFIG }
