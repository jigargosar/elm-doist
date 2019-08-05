import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import path from 'ramda/es/path'
import './index.css'
import { Elm } from './Main.elm'
import { Fire } from './fire'

const app = Elm.Main.init({
  flags: {
    modelCache: JSON.parse(localStorage.getItem('modelCache') || 'null'),
  },
})
const fire = Fire()

const pub = {
  onAuthStateChanged: (arg) => {
    const portName = 'onAuthStateChanged'
    const send = path(['ports', portName, 'send'])(app)
    if (!send) {
      console.warn('Send: Port Not Found:', portName, arg)
      return
    }
    if(send){
      send(arg)
    }
  }
}

fire.onAuthStateChanged(user =>{
  if(user){
    pub.onAuthStateChanged(user.uid)
  }else{
    pub.onAuthStateChanged(null)
  }
})






const subs = {
  cacheKeyValue: ([k, v]) => {
    console.log('cacheKeyValue', k, v)
    localStorage.setItem(k, JSON.stringify(v))
  },
}

forEachObjIndexed((listener, portName) => {
  const subscribe = path(['ports', portName, 'subscribe'])(app)
  if (!subscribe) {
    console.warn('Subscribe: Port Not Found:', portName)
    return
  }
  console.log('Subscription Port Attached', portName)
  subscribe(listener)
})(subs)
