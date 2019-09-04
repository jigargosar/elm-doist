import React, {
  createContext,
  useCallback,
  useContext,
  useState,
} from 'react'
import { render } from 'react-dom'
import 'tachyons'
import './index.css'
import nanoid from 'nanoid'
import faker from 'faker'
import times from 'ramda/es/times'
import produce from 'immer'

type Todo = {
  id: string
  title: string
  isDone: boolean
}

type Model = {
  todoPopup: { tag: 'Closed' } | { tag: 'Open'; todoId: string }
  todoList: ReadonlyArray<Todo>
}

function createFakeTodo(): Todo {
  return { id: nanoid(), title: faker.hacker.phrase(), isDone: false }
}

const initialTodos: Todo[] = times(createFakeTodo, 10)

const initialModel: Model = {
  todoPopup: { tag: 'Closed' },
  todoList: initialTodos,
}

function exhaustiveCheck(never: never) {
  return never
}

type Msg =
  | { tag: 'OpenTodoPopup'; todoId: string }
  | { tag: 'SetDone'; todoId: string; isChecked: boolean }

function update(msg: Msg, model: Model): Model {
  if (msg.tag === 'OpenTodoPopup') {
    model.todoPopup = { tag: 'Open', todoId: msg.todoId }
    return model
  } else if (msg.tag === 'SetDone') {
    const maybeTodo = model.todoList.find(todo => todo.id === msg.todoId)
    if (maybeTodo) {
      maybeTodo.isDone = msg.isChecked
    }
    return model
  }
  return exhaustiveCheck(msg)
}

const DispatcherContext = createContext((_: Msg) => {})
const ModelContext = createContext(initialModel)

function App() {
  const [model, setModel] = useState(initialModel)
  const dispatch = useCallback(
    (msg: Msg) => {
      setModel(model => {
        return produce(model, draft => update(msg, draft))
      })
    },
    [setModel],
  )
  return (
    <DispatcherContext.Provider value={dispatch}>
      <ModelContext.Provider value={model}>
        <div className="lh-copy" style={{ maxWidth: 500 }}>
          <div className="f4 pv1">TodoList</div>
          {model.todoList.map(todo => (
            <TodoItem key={todo.id} todo={todo} />
          ))}
        </div>
      </ModelContext.Provider>
    </DispatcherContext.Provider>
  )
}

function TodoItem({ todo }: { todo: Todo }) {
  const dispatch = useContext(DispatcherContext)
  return (
    <div className="flex">
      <div className="ph1 pv2">
        <input
          type="checkbox"
          className=""
          checked={todo.isDone}
          style={{ width: 24, height: 24 }}
          onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
            dispatch({
              tag: 'SetDone',
              todoId: todo.id,
              isChecked: e.target.checked,
            })
          }}
        />
      </div>
      <div className="ph1 pv1 flex-grow-1 lh-title ">{todo.title}</div>
      <div className="relative">
        <div className="ph1 b pointer">...</div>
        <TodoMenu todoId={todo.id} />
      </div>
    </div>
  )
}

function TodoMenu({ todoId }: { todoId: string }) {
  const model = useContext(ModelContext)
  if(model.todoPopup.tag === 'Open' && model.todoPopup.todoId === todoId){
    return <div>TODO MENU</div>
  }
  return null
}

render(<App />, document.getElementById('root'))
