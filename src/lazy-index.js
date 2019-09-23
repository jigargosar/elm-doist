import { Fire } from './fire'
import debounce from 'debounce'
import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import identity from 'ramda/es/identity'
import mapObjIndexed from 'ramda/es/mapObjIndexed'
import path from 'ramda/es/path'
import propOr from 'ramda/es/propOr'

// Custom Elements
customElements.define(
  'auto-resize-textarea',
  class extends HTMLElement {
    constructor() {
      super()
      this._textAreaValue = ''
    }
    get textAreaValue() {
      // noinspection JSUnresolvedVariable
      return this._textAreaValue || ''
    }
    set textAreaValue(value) {
      this._textAreaValue = value
      const textAreaEl = this.textAreaEl
      if (textAreaEl) {
        textAreaEl.value = value
        resizeTextArea(textAreaEl)
      }
    }
    get textAreaEl() {
      return this.querySelector('textarea')
    }

    connectedCallback() {
      const textAreaEl = this.textAreaEl
      if (textAreaEl) {
        textAreaEl.value = this.textAreaValue
        resizeTextArea(textAreaEl)
      }
    }
  },
)

customElements.define(
  'track-focus-outside',
  class extends HTMLElement {
    constructor(){
      super()
      this.handleFocusIn = this.handleFocusIn.bind(this)
    }
    handleFocusIn() {
      if (!this.isConnected) return
      const ownerDocument = this.ownerDocument
      const activeElement = ownerDocument.activeElement
      // noinspection JSCheckFunctionSignatures
      if (
        activeElement !== ownerDocument.body &&
        !this.contains(activeElement)
      ) {
        this.dispatchEvent(new CustomEvent('focusOutside'))
      }
    }
    connectedCallback() {
      // this.addEventListener('focusout', focusOutListener)
      this.ownerDocument.addEventListener('focusin', this.handleFocusIn)
    }
    disconnectedCallback() {
      // this.removeEventListener('focusout', focusOutListener)
      this.ownerDocument.removeEventListener('focusin', this.handleFocusIn)
    }
  },
)

// utils

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

function focusSelector(selector) {
  console.log('focusSelector', selector)
  const el = document.querySelector(selector)
  if (el) {
    el.focus()
  } else {
    console.error('focusSelector failed:', selector)
  }
}

const debouncedFocusSelector = debounce(focusSelector, 0)

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

async function bootElmApp() {
  const Elm = (await import('./Main.elm')).Elm
  const app = Elm.Main.init({
    flags: {
      cachedTodoList,
      cachedProjectList,
      cachedAuthState,

      browserSize: {
        width: window.innerWidth,
        height: window.innerHeight,
      },
      now: Date.now(),
    },
  })
  const firePromise = Fire()

  const pubs = initPubs(app, {
    onAuthStateChanged: identity,
    onFirestoreQueryResponse: identity,
  })

  firePromise.then(fire =>
    fire.onAuthStateChanged(pubs.onAuthStateChanged),
  )

  initSubs(app, {
    focusSelector: selector => {
      console.log('Queuing focusSelector', selector)
      requestAnimationFrame(() => {
        focusSelector(selector)
        // debouncedFocusSelector(selector)
      })
    },
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
          if (qs.docChanges().length === 0) {
            return
          }
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
}

bootElmApp()

function initSubs(app, subs) {
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

function initPubs(app, pubs) {
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
