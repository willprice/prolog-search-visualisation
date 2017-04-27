'use strict'

function promisify (syncFunction) {
  return function (...args) {
    return new Promise((resolve) => {
      resolve(syncFunction(...args))
    })
  }
}

export default promisify
