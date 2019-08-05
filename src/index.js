import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import path from 'ramda/es/path'
import './index.css'
import { Elm } from './Main.elm'
import firebase from "firebase/app"
import "firebase/auth"
// import "firebase/firestore"

const app = Elm.Main.init({
  flags: {
    modelCache: JSON.parse(localStorage.getItem('modelCache') || 'null'),
  },
})

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


const firebaseConfig = {
  apiKey: "AIzaSyBVS1Tx23pScQz9w4ZDTGh307mqkCRy2Bw",
  authDomain: "not-now-142808.firebaseapp.com",
  databaseURL: "https://not-now-142808.firebaseio.com",
  projectId: "not-now-142808",
  storageBucket: "not-now-142808.appspot.com",
  messagingSenderId: "476064436883",
  appId: "1:476064436883:web:bcd2d5b958a90fa6"
};

firebase.initializeApp(firebaseConfig)

firebase.auth().onAuthStateChanged(user =>{
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
