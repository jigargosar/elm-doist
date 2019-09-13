port module Log exposing (..)

import Json.Encode exposing (Value)


port logError : Value -> Cmd msg
