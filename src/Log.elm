port module Log exposing (Model, subscriptions, update)

import BasicsExtra exposing (cons)


port errorStringCmd : String -> Cmd msg


port errorStringSub : (String -> msg) -> Sub msg


type Model
    = Model Internal


init : Model
init =
    Model []


type alias Internal =
    List String


map : (Internal -> Internal) -> Model -> Model
map fn (Model internal) =
    Model (fn internal)


addStringError : String -> Model -> Model
addStringError errorString =
    map (cons errorString)


type Msg
    = GotStringError String
    | Clear


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.batch
        [ errorStringSub GotStringError
        ]


update : Msg -> Model -> Model
update message model =
    case message of
        GotStringError errorString ->
            addStringError errorString model

        Clear ->
            init
