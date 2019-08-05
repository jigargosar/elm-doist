module Errors exposing (Error, Errors, initial, prependError)


type alias Error =
    String


type alias Errors =
    { errors : List Error }


initial =
    { errors = [] }


prependError : String -> Errors -> Errors
prependError error model =
    { model | errors = error :: model.errors }
