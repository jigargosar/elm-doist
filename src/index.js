import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import path from 'ramda/es/path'
import './index.css'
import { Elm } from './Main.elm'
import firebase from "firebase/app"
import "firebase/auth"
import "firebase/firestore"

const app = Elm.Main.init({
  flags: {
    modelCache: JSON.parse(localStorage.getItem('modelCache') || 'null'),
  },
})


firebase.initializeApp({})

firebase.auth().onAuthStateChanged(user =>{
  if(user){
    app.ports.onAuthStateChanged.send(user.uid)
  }else{
    app.ports.onAuthStateChanged.send(null)
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
    console.warn('Subscription Port Not Found:', portName, app)
    return
  }
  console.log('Subscription Port Attached', portName)
  subscribe(listener)
})(subs)
