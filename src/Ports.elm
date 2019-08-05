module Ports exposing (localStorageSetJsonItem, localStorageSetStringItem)

import Json.Encode exposing (Value)


port localStorageSetStringItem : ( String, String ) -> Cmd msg


port localStorageSetJsonItem : ( String, Value ) -> Cmd msg
