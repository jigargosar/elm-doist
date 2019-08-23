module TodoMenu exposing (..)

import TodoId exposing (TodoId)


type Model
    = Model TodoId


forTodoId todoId_ =
    Model todoId_


isOpenForTodoId : TodoId -> Model -> Bool
isOpenForTodoId todoId_ (Model todoId) =
    todoId_ == todoId
