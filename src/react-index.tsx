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
import produce from "immer"

type Todo = {
  id: string
  title: string
  isDone: boolean
}

type Model = {
  readonly todoPopup: { tag: 'Closed' } | { tag: 'Open'; todoId: string }
  readonly todoList: Todo[]
}

type WritableModel = {-readonly [K in keyof  Model]: Model[K]}

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


function update(msg: Msg, model: WritableModel): Model {
  if (msg.tag === 'OpenTodoPopup') {
    model.todoPopup = { tag: 'Open', todoId: msg.todoId }
    return model
  } else if (msg.tag === 'SetDone') {
    const maybeTodo = model.todoList.find(todo=>todo.id===msg.todoId)
    if(maybeTodo){
      maybeTodo.isDone = msg.isChecked
    }
    return model
  }
  return exhaustiveCheck(msg)
}

const DispatcherContext = createContext((_: Msg) => {})

function App() {
  const [state, setState] = useState(initialModel)
  const dispatch = useCallback(
    (msg: Msg) => {
      setState(model => {
        return produce(model, draft => update(msg, draft))
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
