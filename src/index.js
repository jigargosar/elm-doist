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
    const focusWithinSelf =
      document.querySelector(`#${this.id}:focus-within`) === this
    if (
      !this.contains(document.activeElement) &&
      !focusWithinSelf
    ) {
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
const cachedDialog = getCached('cachedDialog')
const cachedInlineEditTodo = getCached('cachedInlineEditTodo')

const app = Elm.Main.init({
  flags: {
    cachedTodoList,
    cachedProjectList,
    cachedAuthState,
    cachedDialog,
    cachedInlineEditTodo,
    browserSize: { width: window.innerWidth, height: window.innerHeight },
    now: Date.now(),
  },
})
const fire = Fire()

const pubs = initPubs({
  onAuthStateChanged: identity,
  onFirestoreQueryResponse: identity,
  onBrowserFocusChanged: identity,
})

pubs.onBrowserFocusChanged(document.hasFocus())

window.addEventListener('focus', () => pubs.onBrowserFocusChanged(true))
window.addEventListener('blur', () => pubs.onBrowserFocusChanged(false))

fire.onAuthStateChanged(pubs.onAuthStateChanged)

function resizeTextArea(el) {
  el.style.height = 'auto'
  el.style.height = `${el.scrollHeight}px`
}

function resizeTextAreaOnInputListener(ev) {
  resizeTextArea(ev.target)
}

initSubs({
  localStorageSetJsonItem,
  signIn: () => fire.signIn(),
  signOut: () => fire.signOut(),
  queryFirestore: async options => {
    const cRef = fire.userCRef(options.userCollectionName)
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
  disposeFirestoreQuery: id => {
    fire.disposeNamed(id)
  },
  updateFirestoreDoc: options => {
    const doc = fire.userDocRef(options.userDocPath)
    return doc.update(options.data)
  },
  deleteFirestoreDoc: options => {
    const doc = fire.userDocRef(options.userDocPath)
    return doc.delete()
  },
  addFirestoreDoc: async options => {
    const faker = await import('faker')
    const cRef = fire.userCRef(options.userCollectionName)

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
