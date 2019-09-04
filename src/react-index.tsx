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

function mergePartial<T>(partial: Partial<T>, full: T): T {
  return { ...full, ...partial }
}


type Todo = {
  id: string
  title: string
  isDone: boolean
}

type Model = {
  todoPopup: { tag: 'Closed' } | { tag: 'Open'; todoId: string }
  todoList: Todo[]
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
    return mergePartial(
      { todoPopup: { tag: 'Open', todoId: msg.todoId } },
      model,
    )
  } else if (msg.tag === 'SetDone') {
    const todoList = model.todoList.map(todo => {
      return todo.id === msg.todoId
        ? mergePartial({ isDone: msg.isChecked }, todo)
        : todo
    })
    return mergePartial({ todoList }, model)
  }
  return exhaustiveCheck(msg)
}

const DispatcherContext = createContext((_: Msg) => {})

function App() {
  const [state, setState] = useState(initialModel)
  const dispatch = useCallback(
    (msg: Msg) => {
      setState(model => {
        return update(msg, model)
      })
    },
    [setState],
  )

  return (
    <DispatcherContext.Provider value={dispatch}>
      <div className="lh-copy" style={{ maxWidth: 500 }}>
        <div className="f4 pv1">TodoList</div>
        {state.todoList.map(todo => (
          <TodoItem key={todo.id} todo={todo} />
        ))}
      </div>
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
      <div className="ph1 b pointer">...</div>
    </div>
  )
}

render(<App />, document.getElementById('root'))
