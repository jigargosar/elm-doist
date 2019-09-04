import React, { useCallback, useState } from 'react'
import { render } from 'react-dom'
import 'tachyons'
import './index.css'
import nanoid from 'nanoid'
import faker from 'faker'
import times from 'ramda/es/times'
import assoc from 'ramda/es/assoc'

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
  return { id: nanoid(), title: faker.hacker.phrase() , isDone:false}
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
  | { tag: 'Check'; todoId: string; isChecked: boolean }

function setDone(isDone:boolean, todo:Todo): Todo {
  return assoc("isDone", isDone, todo)
}

function update(msg: Msg, model: Model): Model {
  if (msg.tag === 'OpenTodoPopup') {
    return {
      ...model,
      ...{ todoPopup: { tag: 'Open', todoId: msg.todoId } },
    }
  } else if (msg.tag === 'Check') {
    const todoList: Todo[] = model.todoList.map(todo => {
      return todo.id === msg.todoId ? setDone(msg.isChecked, todo) : todo
    })
    return {
      ...model,
      ...{
        todoList,
      },
    }
  }
  return exhaustiveCheck(msg)
}

function App() {
  const [state, setState] = useState(initialModel)
  const dispatch = useCallback((msg:Msg)=>{
    setState(model => {
      return update(msg, model)
    })
  },[setState])
  
  return (
    <div className="lh-copy" style={{ maxWidth: 500 }}>
      <div className="f4 pv1">TodoList</div>
      {state.todoList.map(todo => (
        <TodoItem key={todo.id} todo={todo} dispatch={dispatch}/>
      ))}
    </div>
  )
}

function TodoItem({ todo , dispatch}: { todo: Todo, dispatch:(msg :Msg)=>void }) {
  return (
    <div className="flex">
      <div className="ph1 pv2">
        <input
          type="checkbox"
          className=""
          checked={todo.isDone}
          onChange={(e: React.ChangeEvent<HTMLInputElement>)=> {
            dispatch({tag:"Check", todoId:todo.id, isChecked: e.target.checked})
          }}
          style={{ width: 24, height: 24 }}
        />
      </div>
      <div className="ph1 pv1 flex-grow-1 lh-title ">{todo.title}</div>
      <div className="ph1 b pointer">...</div>
    </div>
  )
}

render(<App />, document.getElementById('root'))
