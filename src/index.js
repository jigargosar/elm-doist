import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import path from 'ramda/es/path'
import './index.css'
import { Elm } from './Main.elm'
import { Fire } from './fire'

import faker from 'faker'

import { mapObjIndexed, identity } from 'ramda'

const app = Elm.Main.init({
  flags: {
    todoList: JSON.parse(localStorage.getItem('todoList') || 'null'),
    cachedAuthState: JSON.parse(localStorage.getItem('cachedAuthState') || 'null')
  },
})
const fire = Fire()

const pubs = initPubs({
  onAuthStateChanged: identity,
  onTodoListChanged: identity,
})

fire.onAuthStateChanged(user => {
  pubs.onAuthStateChanged(user)
  if (user) {
    console.debug(user)

    const todoCRef = fire.userCRef('todos')
    fire.disposeOnAuthChange(
      todoCRef.onSnapshot(qs => {
        const todoDataList = qs.docs.map(ds => ds.data())
        console.log(todoDataList)
        pubs.onTodoListChanged(todoDataList)
      }),
    )
  } else {
  }
})

initSubs({
  localStorageSetJsonItem: ([k, v]) => {
    console.log('localStorageSetJsonItem', k, v)
    localStorage.setItem(k, JSON.stringify(v))
  },
  signIn: () => fire.signIn(),
  signOut: () => fire.signOut(),
  changeTodoTitle: async todoId => {
    const todoCRef = fire.userCRef('todos')
    await todoCRef
      .doc(todoId)
      .update({ title: faker.hacker.phrase(), modifiedAt: Date.now() })
  },
  persistTodoList: async todoList => {
    const todoCRef = fire.userCRef('todos')
    const ps = todoList.map(todo => {
      return todoCRef.doc(todo.id).set(todo, { merge: false })
    })
    await Promise.all(ps)
  },
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
