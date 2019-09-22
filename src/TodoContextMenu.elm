module TodoContextMenu exposing (Config, Model, Msg, init, open, triggerId, update)

import Browser.Dom exposing (Element)
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
    = Opening Todo
    | Opened Todo Element
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
    | GotAnchorElement Element
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
        Open todo ->
            ( Opening todo, focusFirstCmd config )

        GotAnchorElement el ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening todo ->
                    ( Opened todo el, focusFirstCmd config )

                Opened todo _ ->
                    ( Opened todo el, Cmd.none )

        Close ->
            ( Closed, Cmd.none )

        ItemMsg msg ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening _ ->
                    ( model, Cmd.none )

                Opened todo _ ->
                    case msg of
                        Edit ->
                            ( Closed, perform (config.edit todo) )


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
        anchorEl =
            case model of
                Closed ->
                    Nothing

                Opening _ ->
                    Nothing

                Opened _ el ->
                    Just el
    in
    case anchorEl of
        Just el ->
            viewOpen config
                |> H.map config.toMsg

        Nothing ->
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
