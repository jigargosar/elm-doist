module TodoContextMenu exposing (Config, Model, Msg, init, open, triggerId, update)

import Focus
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes exposing (class, tabindex)
import Html.Styled.Events as E
import HtmlExtra as HX
import Json.Decode as JD
import Task
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = Opened Todo
    | Closed


init : Model
init =
    Closed


open : Todo -> Msg
open =
    Open


triggerId : TodoId -> String
triggerId todoId =
    "todo-context-menu" ++ TodoId.toString todoId


type Msg
    = Open Todo
    | Close
    | ItemMsg ItemMsg


type ItemMsg
    = Edit


type alias Config msg =
    { toMsg : Msg -> msg
    , edit : Todo -> msg
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        Open todoId ->
            ( Opened todoId, focusFirstCmd config )

        Close ->
            ( Closed, Cmd.none )

        ItemMsg msg ->
            case model of
                Opened todo ->
                    case msg of
                        Edit ->
                            ( Closed, perform (config.edit todo) )

                Closed ->
                    ( Closed, Cmd.none )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


rootDomId =
    "todo-context-menu"


focusFirstCmd _ =
    Focus.autoFocusWithinId rootDomId


view : Config msg -> Model -> Html msg
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
viewOpen _ =
    H.styled (H.node "track-focus-outside")
        []
        [ class "absolute top-1 left--1 shadow-1 bg-white"
        , E.on "focusOutside" (JD.succeed Close)
        , Key.onEscape Close
        , tabindex -1
        ]
        (viewMenuItems |> List.map (H.map ItemMsg))


viewMenuItems : List (Html ItemMsg)
viewMenuItems =
    [ TextButton.view [ Focus.dataAutoFocus True ] Edit "Edit"
    ]
