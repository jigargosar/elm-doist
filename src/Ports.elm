module Ports exposing
    ( localStorageSetJsonItem
    , localStorageSetStringItem
    , onAuthStateChanged
    )

import Json.Encode exposing (Value)


port localStorageSetStringItem : ( String, String ) -> Cmd msg


port localStorageSetJsonItem : ( String, Value ) -> Cmd msg


port onAuthStateChanged : (Value -> msg) -> Sub msg
