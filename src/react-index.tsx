import React, { useState } from 'react'
import { render } from 'react-dom'
import 'tachyons'
import './index.css'
import nanoid from 'nanoid'
import faker from 'faker'
import times from 'ramda/es/times'

type Todo = {
  id: string
  title: string
}

type Model = {
  todoPopup: { tag: 'Closed' } | { tag: 'Open'; todoId: string }
  todoList: Todo[]
}

function createFakeTodo() {
  return { id: nanoid(), title: faker.hacker.phrase() }
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
  | { tag: 'OpenTodoPopup'; payload: [] }
  | { tag: 'Check'; payload: [string, boolean] }

function update(msg: Msg, model: Model) {
  if (msg.tag === 'OpenTodoPopup') {
    return model
  } else if (msg.tag === 'Check') {
    const [todoId, isChecked] = msg.payload
    return model
  }
  return exhaustiveCheck(msg)
}

function App() {
  const [state] = useState(initialModel)

  return (
    <div className="lh-copy" style={{ maxWidth: 500 }}>
      <div className="f4 pv1">TodoList</div>
      {state.todoList.map(todo => (
        <TodoItem key={todo.id} todo={todo} />
      ))}
    </div>
  )
}

function TodoItem({ todo }: { todo: Todo }) {
  return (
    <div className="flex">
      <div className="ph1 pv2">
        <input
          type="checkbox"
          className=""
          style={{ width: 24, height: 24 }}
        />
      </div>
      <div className="ph1 pv1 flex-grow-1 lh-title ">{todo.title}</div>
      <div className="ph1 b pointer">...</div>
    </div>
  )
}

render(<App />, document.getElementById('root'))
