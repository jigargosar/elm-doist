module TodoMenu exposing (..)

import TodoId exposing (TodoId)


type Model
    = Open TodoId
    | Closed


init =
    Closed


openFor : TodoId -> Model
openFor todoId_ =
    Open todoId_


isOpenFor : TodoId -> Model -> Bool
isOpenFor todoId_ model =
    case model of
        Open tid ->
            todoId_ == tid

        Closed ->
            False
