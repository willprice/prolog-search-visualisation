'use strict'

import SearchAPI from './SearchAPI'
import log from 'util/log'

const DEBUG_TOPIC = 'GridSearchAPI'

class GridSearchAPI extends SearchAPI {
  setupGrid (config) {
    this.sendCommand('grid:setup', config, (resp) => {
      if (resp.response === 'ok') {
        log(DEBUG_TOPIC, 'Grid setup successfully')
      } else {
        log(DEBUG_TOPIC, 'Grid setup failed')
      }
    })
  }

}

export default GridSearchAPI
