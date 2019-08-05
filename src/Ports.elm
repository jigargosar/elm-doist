port module Ports exposing
    ( localStorageSetJsonItem
    , localStorageSetStringItem
    , onAuthStateChanged
    , signIn
    , signOut
    )

import Json.Encode exposing (Value)


port localStorageSetStringItem : ( String, String ) -> Cmd msg


port localStorageSetJsonItem : ( String, Value ) -> Cmd msg


port onAuthStateChanged : (Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg
