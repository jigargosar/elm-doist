module TodoContextMenu exposing (Config, Model, Msg, init, open, subscriptions, triggerId, update, view)

import Browser.Dom as Dom exposing (Element)
import Css exposing (absolute, left, position, px, top, width)
import Focus
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as A exposing (class, tabindex)
import HtmlExtra as HX
import Task
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = Opening Todo
    | Opened OpenedState
    | Closed


type SubMenu
    = SelectProjectSubMenu


subMenuDomId : SubMenu -> String
subMenuDomId subMenu =
    case subMenu of
        SelectProjectSubMenu ->
            "select-project-sub-menu"


subMenuTriggerDomId : SubMenu -> String
subMenuTriggerDomId subMenu =
    subMenuDomId subMenu ++ "__trigger"


type alias OpenedState =
    { todo : Todo
    , anchor : Element
    , subMenu : Maybe SubMenu
    }


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
    | OpenedMsg OpenedMsg
    | Focused Focus.FocusResult


type OpenedMsg
    = Edit
    | OpenSelectProjectSubMenu
    | CloseSubMenu
    | SubMenuFocusLost
    | FocusLost
    | Close


type alias Config msg =
    { toMsg : Msg -> msg
    , edit : Todo -> msg
    }


subscriptions : Config msg -> Model -> Sub msg
subscriptions _ _ =
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
                    ( Opened { todo = todo, anchor = el, subMenu = Nothing }, focusFirstCmd config )

                Opened state ->
                    ( Opened { state | anchor = el }, Cmd.none )

        OpenedMsg msg ->
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
                        Close ->
                            ( Closed, restoreFocusCmd config state.todo.id )

                        Edit ->
                            ( Closed
                            , Cmd.batch
                                [ restoreFocus
                                , perform (config.edit state.todo)
                                ]
                            )

                        OpenSelectProjectSubMenu ->
                            ( Opened { state | subMenu = Just SelectProjectSubMenu }
                            , focusSubMenu SelectProjectSubMenu
                            )

                        CloseSubMenu ->
                            case state.subMenu of
                                Just menu ->
                                    ( Opened { state | subMenu = Nothing }, restoreFocusOnSubMenuClose menu )

                                Nothing ->
                                    ( model, Cmd.none )

                        SubMenuFocusLost ->
                            case state.subMenu of
                                Just _ ->
                                    ( Opened { state | subMenu = Nothing }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        FocusLost ->
                            ( Closed, Cmd.none )

        Focused _ ->
            ( model, Cmd.none )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


rootDomId =
    "todo-context-menu"


focusFirstCmd _ =
    Focus.autoFocusWithinId rootDomId


focusSubMenu : SubMenu -> Cmd msg
focusSubMenu subMenu =
    Focus.autoFocusWithinId (subMenuDomId subMenu)


restoreFocusCmd : Config msg -> TodoId -> Cmd msg
restoreFocusCmd config todoId =
    triggerId todoId |> Focus.attempt (Focused >> config.toMsg)


restoreFocusOnSubMenuClose : SubMenu -> Cmd msg
restoreFocusOnSubMenuClose menu =
    subMenuTriggerDomId menu |> Focus.focusId


view : Config msg -> Model -> Html msg
view config model =
    case model of
        Closed ->
            HX.none

        Opening _ ->
            HX.none

        Opened openedState ->
            viewOpened openedState
                |> H.map (OpenedMsg >> config.toMsg)


viewOpened : OpenedState -> Html OpenedMsg
viewOpened { anchor, subMenu } =
    viewContainer anchor
        (viewItems subMenu)


viewContainer : Element -> List (Html OpenedMsg) -> Html OpenedMsg
viewContainer anchor =
    Focus.styledFocusTracker
        [ rootStyles anchor ]
        [ A.id rootDomId
        , class "shadow-1 bg-white"
        , Key.onEscape Close
        , Focus.onFocusLost FocusLost
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


viewItems : Maybe SubMenu -> List (Html OpenedMsg)
viewItems subMenu =
    [ TextButton.view
        [ Focus.dataAutoFocus True
        , class "pv1 ph2"
        ]
        Edit
        "Edit"
    , div [ class "relative" ]
        [ TextButton.view
            [ A.id (subMenuTriggerDomId SelectProjectSubMenu)
            , class "pv1 ph2"
            ]
            OpenSelectProjectSubMenu
            "Move To"
        , case subMenu of
            Just SelectProjectSubMenu ->
                div
                    [ A.id (subMenuDomId SelectProjectSubMenu)
                    , class "absolute top-1 left--1 shadow-1 bg-white"
                    , Key.onEscape CloseSubMenu
                    , tabindex -1
                    ]
                    [ TextButton.view
                        [ Focus.dataAutoFocus True
                        , class "ph2 pv1"
                        ]
                        CloseSubMenu
                        "Select Project"
                    ]

            _ ->
                HX.none
        ]
    ]
