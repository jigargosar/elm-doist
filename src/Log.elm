port module Log exposing (Model, Msg, decodeError, init, subscriptions, update)

import BasicsExtra exposing (..)
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (class, tabindex)
import Json.Decode as JD


port errorStringCmd : String -> Cmd msg


decodeError : JD.Error -> Cmd msg
decodeError error =
    JD.errorToString error |> errorStringCmd


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ errorStringSub GotStringError
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        GotStringError errorString ->
            ( addStringError errorString model, Cmd.none )

        Clear ->
            ( init, Cmd.none )


view : Html msg
view =
    viewHelp [ div [] [ text "errors" ] ]


viewHelp content =
    div
        [ class "z-1 fixed absolute--fill flex items-center justify-center"
        , tabindex -1
        ]
        [ div
            [ class "absolute absolute--fill bg-black-50"
            ]
            []
        , div [ class "absolute" ] content
        , H.node "style" [] [ text "body { overflow: hidden; }" ]
        ]
