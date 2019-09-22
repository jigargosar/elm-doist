module TodoContextMenu exposing (Config, Model, Msg)

import Focus
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes exposing (class, tabindex)
import Html.Styled.Events as E
import HtmlExtra as HX
import Json.Decode as JD
import TodoId exposing (TodoId)
import UI.Key as Key


type Model
    = Opened TodoId
    | Closed


init : Model
init =
    Closed


open : TodoId -> Msg
open todoId =
    Open todoId


type Msg
    = Open TodoId
    | Close


type alias Config msg =
    { toMsg : Msg -> msg }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message _ =
    case message of
        Open todoId ->
            ( Opened todoId, focusFirstCmd config )

        Close ->
            ( Closed, Cmd.none )


rootDomId =
    "todo-context-menu"


focusFirstCmd _ =
    Focus.autoFocusWithinId rootDomId


view : { toMsg : Msg -> msg } -> Model -> Html msg
view config model =
    let
        isOpen =
            case model of
                Opened _ ->
                    True

                Closed ->
                    False
    in
    if isOpen then
        viewOpen config
            |> H.map config.toMsg

    else
        HX.none


viewOpen : Config msg -> Html Msg
viewOpen config =
    H.styled (H.node "track-focus-outside")
        []
        [ class "absolute top-1 left--1 shadow-1 bg-white"
        , E.on "focusOutside" (JD.succeed Close)
        , Key.onEscape Close
        , tabindex -1
        ]
        []
