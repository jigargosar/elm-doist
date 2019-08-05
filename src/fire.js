import firebase from "firebase/app"
import "firebase/auth"
// import "firebase/firestore"

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


export function Fire() {
  const auth = firebase.auth()
  return {
    onAuthStateChanged(cb){
      auth.onAuthStateChanged(cb)
    },
    signIn(){
      const provider = new firebase.auth.GoogleAuthProvider()
      provider.setCustomParameters({prompt:'select_account'})
      return auth.signInWithPopup(provider)
    },
    signOut() {
      return auth.signOut()
    }

  }
}




