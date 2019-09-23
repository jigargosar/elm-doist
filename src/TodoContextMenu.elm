module TodoContextMenu exposing (Config, Model, Msg, init, open, subscriptions, triggerId, update, view)

import BasicsExtra exposing (eq_)
import Browser.Dom as Dom exposing (Element)
import BrowserSize exposing (BrowserSize)
import Css exposing (absolute, left, position, px, top, width)
import Focus
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import HtmlExtra as HX
import List.Extra as LX
import MovePopup
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
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


subMenuTriggerTitle : SubMenu -> String
subMenuTriggerTitle subMenu =
    case subMenu of
        SelectProjectSubMenu ->
            "Move to..."


type alias OpenedState =
    { todo : Todo
    , anchor : Element
    , maybeSubMenuState : Maybe SubMenu
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
    | BrowserReSized BrowserSize
    | OpenedMsg OpenedMsg
    | Focused Focus.FocusResult


type OpenedMsg
    = Edit
    | OpenSubMenu SubMenu
    | SubMenuMsg SubMenuMsg
    | FocusLost
    | Close


type SubMenuMsg
    = CloseSubMenu
    | SubMenuLostFocus
    | ProjectIdSelected ProjectId


type alias Config msg =
    { toMsg : Msg -> msg
    , edit : Todo -> msg
    , moved : ProjectId -> Todo -> msg
    }


subscriptions : Config msg -> Model -> Sub msg
subscriptions config _ =
    Sub.batch
        [ BrowserSize.onBrowserResize BrowserReSized
        ]
        |> Sub.map config.toMsg


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        Open todo ->
            ( Opening todo
            , Dom.getElement (triggerId todo.id)
                |> Task.attempt (GotAnchorElement >> config.toMsg)
            )

        BrowserReSized _ ->
            case model of
                Opening todo ->
                    ( model
                    , Dom.getElement (triggerId todo.id)
                        |> Task.attempt (GotAnchorElement >> config.toMsg)
                    )

                Opened { todo } ->
                    ( model
                    , Dom.getElement (triggerId todo.id)
                        |> Task.attempt (GotAnchorElement >> config.toMsg)
                    )

                Closed ->
                    ( model, Cmd.none )

        GotAnchorElement (Err _) ->
            ( model, Cmd.none )

        GotAnchorElement (Ok el) ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Opening todo ->
                    ( Opened
                        { todo = todo
                        , anchor = el
                        , maybeSubMenuState = Nothing
                        }
                    , focusFirstCmd config
                    )

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
                            ( Closed, restoreFocus )

                        Edit ->
                            ( Closed
                            , Cmd.batch
                                [ restoreFocus
                                , perform (config.edit state.todo)
                                ]
                            )

                        OpenSubMenu subMenu ->
                            ( Opened { state | maybeSubMenuState = Just subMenu }
                            , focusSubMenu subMenu
                            )

                        SubMenuMsg subMenuMsg ->
                            case state.maybeSubMenuState of
                                Just subMenuState ->
                                    case subMenuMsg of
                                        CloseSubMenu ->
                                            ( Opened { state | maybeSubMenuState = Nothing }
                                            , restoreFocusOnSubMenuClose subMenuState
                                            )

                                        SubMenuLostFocus ->
                                            ( Opened { state | maybeSubMenuState = Nothing }, Cmd.none )

                                        ProjectIdSelected projectId ->
                                            ( Closed
                                            , Cmd.batch
                                                [ restoreFocus
                                                , if projectId == state.todo.projectId then
                                                    Cmd.none

                                                  else
                                                    perform (config.moved projectId state.todo)
                                                ]
                                            )

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


view : Config msg -> ProjectList -> Model -> Html msg
view config projectList model =
    case model of
        Closed ->
            HX.none

        Opening _ ->
            HX.none

        Opened openedState ->
            let
                renderSubMenu which =
                    H.map SubMenuMsg <|
                        case ( openedState.maybeSubMenuState, which ) of
                            ( Nothing, _ ) ->
                                HX.none

                            ( Just SelectProjectSubMenu, SelectProjectSubMenu ) ->
                                MovePopup.view movePopupConfig openedState.todo.projectId projectList
            in
            viewOpened renderSubMenu openedState
                |> H.map (OpenedMsg >> config.toMsg)


viewOpened renderSubMenu { anchor, maybeSubMenuState } =
    viewContainer anchor
        (viewRootMenuItems renderSubMenu)


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


viewRootMenuItems renderSubMenu =
    [ viewItem [ Focus.dataAutoFocus True ] Edit "Edit"
    , viewSubmenuTriggerItem [] SelectProjectSubMenu renderSubMenu
    ]


viewSubmenuTriggerItem attrs subMenu renderSubMenu =
    div [ class "relative" ]
        [ viewItem
            (A.id (subMenuTriggerDomId subMenu) :: attrs)
            (OpenSubMenu subMenu)
            (subMenuTriggerTitle subMenu)
        , renderSubMenu subMenu
        ]


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject { id, title } =
    { id = id, title = title }


inboxDisplayProject : DisplayProject
inboxDisplayProject =
    { id = ProjectId.default, title = "Inbox" }


movePopupConfig : MovePopup.Config SubMenuMsg
movePopupConfig =
    { rootId = subMenuDomId SelectProjectSubMenu
    , closed =
        \reason ->
            case reason of
                MovePopup.LostFocus ->
                    SubMenuLostFocus

                MovePopup.Canceled ->
                    CloseSubMenu

                MovePopup.Selected projectId ->
                    ProjectIdSelected projectId
    }


viewSelectProjectSubMenu : ProjectId -> ProjectList -> Html SubMenuMsg
viewSelectProjectSubMenu selectedProjectId projectList =
    let
        items =
            inboxDisplayProject
                :: List.map toDisplayProject projectList

        selectedItem =
            LX.find (.id >> eq_ selectedProjectId) items
                |> Maybe.withDefault inboxDisplayProject

        firstItem =
            List.head items |> Maybe.withDefault selectedItem

        selectedItemStyle item =
            if item == selectedItem then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []

        attrsForItem item =
            [ Focus.dataAutoFocus (item == firstItem)
            , css [ selectedItemStyle item ]
            ]

        viewDisplayProject item =
            viewItem (attrsForItem item)
                (ProjectIdSelected item.id)
                item.title
    in
    Focus.focusTracker
        [ A.id (subMenuDomId SelectProjectSubMenu)
        , class "absolute top-1 left--1 shadow-1 bg-white"
        , Key.onEscape CloseSubMenu
        , Focus.onFocusLost SubMenuLostFocus
        , tabindex -1
        ]
        (List.map viewDisplayProject items)


viewItem : List (H.Attribute msg) -> msg -> String -> Html msg
viewItem attrs action title =
    TextButton.view (class "ph2 pv1" :: attrs)
        action
        title
