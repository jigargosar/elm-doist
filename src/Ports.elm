port module Ports exposing
    ( changeTodoTitle
    , localStorageSetJsonItem
    , localStorageSetStringItem
    , onAuthStateChanged
    , onTodoListChanged
    , persistTodoList
    , signIn
    , signOut
    )

import Json.Encode exposing (Value)


port localStorageSetStringItem : ( String, String ) -> Cmd msg


port localStorageSetJsonItem : ( String, Value ) -> Cmd msg


port onAuthStateChanged : (Value -> msg) -> Sub msg


port onTodoListChanged : (Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port persistTodoList : Value -> Cmd msg


port changeTodoTitle : String -> Cmd msg
