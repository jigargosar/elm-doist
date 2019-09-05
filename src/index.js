import 'tachyons'
import './index.css'
import { Elm } from './Main.elm'
// import { Elm } from './elm.min'
import { Fire } from './fire'

import {
  forEachObjIndexed,
  identity,
  isNil,
  mapObjIndexed,
  path,
  propOr,
} from 'ramda'

// Custom Elements
customElements.define(
  'auto-resize-textarea',
  class extends HTMLElement {
    connectedCallback() {
      const ta = this.firstChild
      resizeTextArea(ta)
      ta.addEventListener('input', resizeTextAreaOnInputListener)
    }
  },
)

customElements.define(
  'track-focus-outside',
  class extends HTMLElement {
    connectedCallback() {
      this.addEventListener('focusout', focusOutListener)
    }
  },
)

function focusOutListener() {
  setTimeout(() => {
    if (!this.contains(document.activeElement)) {
      console.debug('focusOutside', this)
      this.dispatchEvent(new CustomEvent('focusOutside'))
    }
  }, 0)
}

// Cache
function localStorageSetJsonItem([k, v]) {
  localStorage.setItem(k, JSON.stringify(v))
}

function getCached(key) {
  return JSON.parse(localStorage.getItem(key) || 'null')
}

const cachedProjectList = getCached('cachedProjectList')
const cachedTodoList = getCached('cachedTodoList')
const cachedAuthState = getCached('cachedAuthState')


const app = Elm.Main.init({
  flags: {
    cachedTodoList,
    cachedProjectList,
    cachedAuthState,

    browserSize: { width: window.innerWidth, height: window.innerHeight },
    now: Date.now(),
  },
})
const firePromise = Fire()

const pubs = initPubs({
  onAuthStateChanged: identity,
  onFirestoreQueryResponse: identity,
})

firePromise.then(fire => fire.onAuthStateChanged(pubs.onAuthStateChanged))

function resizeTextArea(el) {
  el.style.height = 'auto'
  el.style.height = `${el.scrollHeight}px`
}

function resizeTextAreaOnInputListener(ev) {
  resizeTextArea(ev.target)
}

function dynamicImportPrefetchFaker() {
  return import(
    /* webpackPrefetch: true, webpackChunkName: "faker"  */ 'faker'
  )
}

initSubs({
  localStorageSetJsonItem,
  signIn: async () => (await firePromise).signIn(),
  signOut: async () => (await firePromise).signOut(),
  queryFirestore: async options => {
    const fire = await firePromise
    const cRef = await fire.userCRef(options.userCollectionName)
    const query = options.whereClause.reduce(
      (query, [fieldPath, op, value]) => {
        return query.where(fieldPath, op, value)
      },
      cRef,
    )
    fire.addDisposerWithId(
      options.id,
      query.onSnapshot(qs => {
        const docDataList = qs.docs.map(ds => ds.data())
        const response = { id: options.id, docDataList }
        pubs.onFirestoreQueryResponse(response)
      }),
    )
  },
  disposeFirestoreQuery: async id => {
    const fire = await firePromise
    fire.disposeNamed(id)
  },
  updateFirestoreDoc: async options => {
    const fire = await firePromise
    const doc = await fire.userDocRef(options.userDocPath)
    return doc.update(options.data)
  },
  deleteFirestoreDoc: async options => {
    const fire = await firePromise
    const doc = await fire.userDocRef(options.userDocPath)
    return doc.delete()
  },
  addFirestoreDoc: async options => {
    const faker = await dynamicImportPrefetchFaker()
    const fire = await firePromise
    const cRef = await fire.userCRef(options.userCollectionName)

    const docRef = cRef.doc()

    const data = Object.assign(
      {},
      options.data,
      { id: docRef.id },
      options.data.title === '' && options.userCollectionName === 'todos'
        ? { title: faker.hacker.phrase() }
        : {},

      options.data.title === '' &&
        options.userCollectionName === 'projects'
        ? { title: `${faker.hacker.ingverb()} ${faker.hacker.noun()}` }
        : {},
    )
    return docRef.set(data)
  },
})

function initSubs(subs) {
  forEachObjIndexed((listener, portName) => {
    const subscribe = path(['ports', portName, 'subscribe'])(app)
    if (!subscribe) {
      console.warn('Subscribe: Port Not Found:', portName)
      return
    }
    console.debug('Subscribe: Port Handler Attached', portName)
    subscribe(listener)
  })(subs)
  const ports = propOr({}, 'ports')(app)
  forEachObjIndexed((port, portName) => {
    if (port.subscribe && !subs[portName]) {
      console.warn('Subscribe: Port Handler Missing', portName)
    }
  })(ports)
}

function initPubs(pubs) {
  const ports = propOr({}, 'ports')(app)
  forEachObjIndexed((port, portName) => {
    if (port.send && !pubs[portName]) {
      console.warn('Send: Port publisher Missing', portName)
    }
  })(ports)
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
