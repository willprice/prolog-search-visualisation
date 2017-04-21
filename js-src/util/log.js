'use strict'

function log (topic, body) {
  const time = new Date()
  console.log(`[${time.toLocaleString()}|${topic}]: ${body}`)
}

export default log
