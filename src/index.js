import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import path from 'ramda/es/path'
import './index.css'
import { Elm } from './Main.elm'
import { Fire } from './fire'

import { mapObjIndexed, identity } from 'ramda'

const app = Elm.Main.init({
  flags: {
    modelCache: JSON.parse(localStorage.getItem('modelCache') || 'null'),
  },
})
const fire = Fire()

const pubs = initPubs({
  onAuthStateChanged: identity,
})

fire.onAuthStateChanged(user => {
  pubs.onAuthStateChanged(user)
  if (user) {
    console.debug(user)
  } else {
  }
})

initSubs({
  cacheKeyValue: ([k, v]) => {
    console.log('cacheKeyValue', k, v)
    localStorage.setItem(k, JSON.stringify(v))
  },
  signIn: () => fire.signIn(),
  signOut: () => fire.signOut(),
})

function initSubs(subs) {
  forEachObjIndexed((listener, portName) => {
    const subscribe = path(['ports', portName, 'subscribe'])(app)
    if (!subscribe) {
      console.warn('Subscribe: Port Not Found:', portName)
      return
    }
    console.log('Subscription Port Attached', portName)
    subscribe(listener)
  })(subs)
}

function initPubs(pubs) {
  return mapObjIndexed((fn, portName) => {
    return arg => {
      const send = path(['ports', portName, 'send'])(app)
      if (!send) {
        console.warn('Send: Port Not Found:', portName, arg)
        return
      }
      if (send) {
        send(arg)
      }
    }
  })(pubs)
}
