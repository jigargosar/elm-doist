port module Ports exposing
    ( FirestoreQueryResponse
    , addFirestoreDoc
    , changeTodoTitle
    , deleteFirestoreDoc
    , localStorageSetJsonItem
    , localStorageSetStringItem
    , onAuthStateChanged
    , onFirestoreQueryResponse
    , onTodoListChanged
    , persistTodoList
    , queryFirestore
    , signIn
    , signOut
    , updateFirestoreDoc
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


port queryFirestore :
    { id : String
    , userCollectionName : String
    , limit : Int
    }
    -> Cmd msg


port updateFirestoreDoc : { userDocPath : String, data : Value } -> Cmd msg


port deleteFirestoreDoc : { userDocPath : String } -> Cmd msg


port addFirestoreDoc : { userCollectionName : String, data : Value } -> Cmd msg


type alias FirestoreQueryResponse =
    { id : String, docDataList : List Value }


port onFirestoreQueryResponse :
    (FirestoreQueryResponse -> msg)
    -> Sub msg
