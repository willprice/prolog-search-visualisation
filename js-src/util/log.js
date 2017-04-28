'use strict'

function log (topic, ...args) {
  const time = new Date()
  let body = ''
  for (let arg of args) {
    if (typeof arg === 'string') {
      body += arg
    } else if (typeof arg === 'symbol') {
      body += arg.toString()
    } else {
      body += JSON.stringify(arg)
    }
    body += ' '
  }
  console.log(`[${time.toLocaleString()}|${topic}]: ${body}`)
}

export default log
