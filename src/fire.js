import { invariant } from './invariant'

const firebaseConfig = {
  apiKey: 'AIzaSyBVS1Tx23pScQz9w4ZDTGh307mqkCRy2Bw',
  authDomain: 'not-now-142808.firebaseapp.com',
  databaseURL: 'https://not-now-142808.firebaseio.com',
  projectId: 'not-now-142808',
  storageBucket: 'not-now-142808.appspot.com',
  messagingSenderId: '476064436883',
  appId: '1:476064436883:web:bcd2d5b958a90fa6',
}

function Disposables() {
  const disposables = []
  return {
    add(fn) {
      disposables.push(fn)
    },
    dispose() {
      disposables.forEach(fn => fn())
      disposables.splice(0, disposables.length)
    },
  }
}

export async function Fire() {
  const firebase = await import(
    /* webpackChunkName: "firebase" */
    'firebase/app'
  )
  firebase.initializeApp(firebaseConfig)


  await import(
    /* webpackChunkName: "firebase" */
    'firebase/auth'
  )
  const auth = firebase.auth()
  const authChangeDisposables = Disposables()
  const namedDisposables = {}

  auth.onAuthStateChanged(user => {
    authChangeDisposables.dispose()
  })


  const dbPromise = import(
    /* webpackPrefetch: true
    , webpackChunkName: "firestore" */
    'firebase/firestore'
    ).then(() => firebase.firestore())

  return {
    onAuthStateChanged(cb) {
      return auth.onAuthStateChanged(cb)
    },
    signIn() {
      const provider = new firebase.auth.GoogleAuthProvider()
      provider.setCustomParameters({ prompt: 'select_account' })
      return auth.signInWithPopup(provider)
    },
    signOut() {
      return auth.signOut()
    },
    disposeOnAuthChange(fn) {
      authChangeDisposables.add(fn)
    },
    async userCRef(name) {
      const uid = auth.currentUser.uid
      invariant(name.trim().length > 0)
      invariant(uid.trim().length > 0)
      const db = await dbPromise
      return db.collection(`users/${uid}/${name}`)
    },
    async userDocRef(userDocPath) {
      const uid = auth.currentUser.uid
      invariant(userDocPath.trim().length > 0)
      invariant(uid.trim().length > 0)
      const db = await dbPromise
      return db.doc(`users/${uid}/${userDocPath}`)
    },
    addDisposerWithId(id, disposer) {
      const prevDisposer = namedDisposables[id]
      if (prevDisposer) {
        prevDisposer()
      }
      namedDisposables[id] = disposer
    },
    disposeNamed(id) {
      const prevDisposer = namedDisposables[id]
      if (prevDisposer) {
        prevDisposer()
        delete namedDisposables[id]
      }
    },
  }
}
