module TodoMenu exposing (..)

import TodoId exposing (TodoId)


type alias Model =
    { todoId : TodoId }


forTodoId todoId =
    { todoId = todoId }


isOpenForTodoId todoId model =
    model.todoId == todoId
