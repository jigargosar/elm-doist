module TodoContextMenu exposing (Config, Model, Msg, init, open, triggerId, update, view)

import Browser.Dom as Dom exposing (Element)
import Css exposing (absolute, left, position, px, top, width)
import Focus
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as A exposing (class, tabindex)
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
    | GotAnchorElement (Result Dom.Error Element)
    | Close
    | ItemMsg ItemMsg
    | Focused Focus.FocusResult


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
            ( Opening todo
            , Dom.getElement (triggerId todo.id)
                |> Task.attempt (GotAnchorElement >> config.toMsg)
            )

        GotAnchorElement (Err error) ->
            let
                _ =
                    Debug.log "GotAnchorElement Error" error
            in
            ( model, Cmd.none )

        GotAnchorElement (Ok el) ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening todo ->
                    ( Opened todo el, focusFirstCmd config )

                Opened todo _ ->
                    ( Opened todo el, Cmd.none )

        Close ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening _ ->
                    ( model, Cmd.none )

                Opened todo _ ->
                    ( Closed, restoreFocusCmd config todo.id )

        ItemMsg msg ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening _ ->
                    ( model, Cmd.none )

                Opened todo _ ->
                    let
                        restoreFocus =
                            restoreFocusCmd config todo.id
                    in
                    case msg of
                        Edit ->
                            ( Closed
                            , Cmd.batch
                                [ restoreFocus
                                , perform (config.edit todo)
                                ]
                            )

        Focused _ ->
            ( model, Cmd.none )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


rootDomId =
    "todo-context-menu"


focusFirstCmd _ =
    Focus.autoFocusWithinId rootDomId


restoreFocusCmd config todoId =
    triggerId todoId |> Focus.attempt (Focused >> config.toMsg)


view : Config msg -> Model -> Html msg
view config model =
    let
        maybeAnchorEl =
            case model of
                Closed ->
                    Nothing

                Opening _ ->
                    Nothing

                Opened _ el ->
                    Just el
    in
    case maybeAnchorEl of
        Just anchorEl ->
            viewOpen config (rootStyles anchorEl)
                |> H.map config.toMsg

        Nothing ->
            HX.none


rootStyles : Element -> Css.Style
rootStyles anchorEl =
    let
        popupWidth =
            256
    in
    Css.batch
        [ position absolute
        , top <| px <| anchorEl.element.y + 20
        , left <| px <| anchorEl.element.x + anchorEl.element.width - popupWidth
        , width <| px popupWidth
        ]


viewOpen : Config msg -> Css.Style -> Html Msg
viewOpen _ rootStyle =
    H.styled (H.node "track-focus-outside")
        [ rootStyle ]
        [ A.id rootDomId
        , class "shadow-1 bg-white"
        , E.on "focusOutside" (JD.succeed Close)
        , Key.onEscape Close
        , tabindex -1
        ]
        (viewMenuItems |> List.map (H.map ItemMsg))


viewMenuItems : List (Html ItemMsg)
viewMenuItems =
    [ TextButton.view
        [ Focus.dataAutoFocus True
        , class "pv1 ph2"
        ]
        Edit
        "Edit"
    ]
