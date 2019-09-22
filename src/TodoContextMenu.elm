module TodoContextMenu exposing (Config, Model, Msg, init, open, subscriptions, triggerId, update, view)

import Browser.Dom as Dom exposing (Element)
import Browser.Events
import Css exposing (absolute, left, position, px, top, width)
import Focus
import Html.Styled as H exposing (Html, div, text)
import Html.Styled.Attributes as A exposing (class, tabindex)
import HtmlExtra as HX
import Json.Decode as JD
import Task
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = Opening Todo
    | Opened Internal
    | Closed


type SubMenu
    = MoveToSubMenu
    | NoSubMenu


type alias Internal =
    { todo : Todo, anchor : Element, subMenu : SubMenu }


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
    | LostFocus
    | ItemMsg ItemMsg
    | Focused Focus.FocusResult


type ItemMsg
    = Edit
    | MoveTo


type alias Config msg =
    { toMsg : Msg -> msg
    , edit : Todo -> msg
    }


subscriptions : Config msg -> Model -> Sub msg
subscriptions config model =
    let
        targetOutsideRootDecoder =
            JD.field "target"
                (Focus.outsideElIdDecoder rootDomId (config.toMsg LostFocus))

        subWhenOpen =
            Sub.batch
                [ Browser.Events.onKeyDown targetOutsideRootDecoder
                , Browser.Events.onMouseDown targetOutsideRootDecoder
                ]
    in
    case model of
        Opening _ ->
            subWhenOpen

        Opened _ ->
            subWhenOpen

        Closed ->
            Sub.none


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        Open todo ->
            ( Opening todo
            , Dom.getElement (triggerId todo.id)
                |> Task.attempt (GotAnchorElement >> config.toMsg)
            )

        GotAnchorElement (Err _) ->
            ( model, Cmd.none )

        GotAnchorElement (Ok el) ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening todo ->
                    ( Opened { todo = todo, anchor = el, subMenu = NoSubMenu }, focusFirstCmd config )

                Opened state ->
                    ( Opened { state | anchor = el }, Cmd.none )

        LostFocus ->
            ( Closed, Cmd.none )

        Close ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening _ ->
                    ( model, Cmd.none )

                Opened state ->
                    ( Closed, restoreFocusCmd config state.todo.id )

        ItemMsg msg ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening _ ->
                    ( model, Cmd.none )

                Opened state ->
                    let
                        restoreFocus =
                            restoreFocusCmd config state.todo.id
                    in
                    case msg of
                        Edit ->
                            ( Closed
                            , Cmd.batch
                                [ restoreFocus
                                , perform (config.edit state.todo)
                                ]
                            )

                        MoveTo ->
                            ( Opened { state | subMenu = MoveToSubMenu }, Cmd.none )

        Focused _ ->
            ( model, Cmd.none )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


rootDomId =
    "todo-context-menu"


focusFirstCmd _ =
    Focus.autoFocusWithinId rootDomId


restoreFocusCmd : Config msg -> TodoId -> Cmd msg
restoreFocusCmd config todoId =
    triggerId todoId |> Focus.attempt (Focused >> config.toMsg)


view : Config msg -> Model -> Html msg
view config model =
    case model of
        Closed ->
            HX.none

        Opening _ ->
            HX.none

        Opened { anchor, subMenu } ->
            viewContainer anchor
                (viewItems subMenu |> List.map (H.map ItemMsg))
                |> H.map config.toMsg


viewContainer : Element -> List (Html Msg) -> Html Msg
viewContainer anchor =
    H.styled div
        [ rootStyles anchor ]
        [ A.id rootDomId
        , class "shadow-1 bg-white"
        , Key.onEscape Close
        , tabindex -1
        ]


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


viewItems : SubMenu -> List (Html ItemMsg)
viewItems subMenu =
    [ TextButton.view
        [ Focus.dataAutoFocus True
        , class "pv1 ph2"
        ]
        Edit
        "Edit"
    , div [ class "relative" ]
        [ TextButton.view
            [ class "pv1 ph2"
            ]
            MoveTo
            "Move To"
        , case subMenu of
            MoveToSubMenu ->
                div [] [ text "Select Project" ]

            _ ->
                HX.none
        ]
    ]
