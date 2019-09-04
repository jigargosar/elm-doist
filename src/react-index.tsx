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
  const [state, setState] = useState(initialState)

  return (
    <div>
      <div>TodoList</div>
      {state.todoList.map(todo => (
        <TodoItem key={todo.id} todo={todo} />
      ))}
    </div>
  )
}

function TodoItem({ todo }: { todo: Todo }) {
  return <div className="flex">{todo.title}</div>
}

render(<App />, document.getElementById('root'))
