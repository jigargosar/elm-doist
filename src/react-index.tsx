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

type State = {
  todoList: Todo[]
}

function createFakeTodo() {
  return { id: nanoid(), title: faker.hacker.phrase() }
}

const initialTodos: Todo[] = times(createFakeTodo, 10)

const initialState: State = { todoList: initialTodos }

function App() {
  const [state] = useState(initialState)

  return (
    <div className="lh-copy" style={{maxWidth: 500}}>
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
        <input type="checkbox" className="" style={{width:24, height:24}}/>
      </div>
      <div className="ph1 pv1 flex-grow-1 lh-title ">{todo.title}</div>
      <div className="pa1 pointer">...</div>
    </div>
  )
}

render(<App />, document.getElementById('root'))
