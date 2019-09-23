module TodoContextMenu exposing (Config, Model, Msg, init, open, subscriptions, triggerId, update, view)

import Accessibility.Styled.Key as AKey
import Basics.Extra exposing (flip)
import Browser.Dom as Dom exposing (Element)
import BrowserSize exposing (BrowserSize)
import Css exposing (absolute, left, position, px, top, width)
import Focus
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as A exposing (class, tabindex)
import HtmlExtra as HX
import List.Extra as LX
import Maybe.Extra as MX
import MovePopup
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Task
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type alias MenuItem msg =
    { msg : msg, title : String }


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
    , activeIdx : Maybe Int
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
    | Next
    | Prev
    | ActiveSelected


rootMenuItems : List (MenuItem OpenedMsg)
rootMenuItems =
    [ { msg = Edit, title = "Edit" }
    , { msg = OpenSubMenu SelectProjectSubMenu, title = "Move To ..." }
    ]


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
                        , activeIdx = Nothing
                        , maybeSubMenuState = Nothing
                        }
                    , focusRoot
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

                        Next ->
                            ( Opened
                                { state
                                    | activeIdx =
                                        state.activeIdx
                                            |> Maybe.map nextRootIdx
                                            |> MX.orElse (Just 0)
                                }
                            , Cmd.none
                            )

                        Prev ->
                            ( Opened
                                { state
                                    | activeIdx =
                                        state.activeIdx
                                            |> Maybe.map prevRootIdx
                                            |> MX.orElse (Just lastRootIdx)
                                }
                            , Cmd.none
                            )

                        ActiveSelected ->
                            case state.activeIdx |> Maybe.andThen (flip LX.getAt rootMenuItems) of
                                Just rootMI ->
                                    update config (OpenedMsg rootMI.msg) model

                                Nothing ->
                                    ( model, Cmd.none )

        Focused _ ->
            ( model, Cmd.none )


nextRootIdx : Int -> Int
nextRootIdx idx =
    modBy (List.length rootMenuItems) (idx + 1)


prevRootIdx : Int -> Int
prevRootIdx idx =
    modBy (List.length rootMenuItems) (idx - 1)


lastRootIdx =
    List.length rootMenuItems - 1


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


rootDomId =
    "todo-context-menu"


focusRoot =
    Focus.focusId rootDomId


focusSubMenu : SubMenu -> Cmd msg
focusSubMenu subMenu =
    Focus.autoFocusWithinId (subMenuDomId subMenu)


restoreFocusCmd : Config msg -> TodoId -> Cmd msg
restoreFocusCmd config todoId =
    triggerId todoId |> Focus.attempt (Focused >> config.toMsg)


restoreFocusOnSubMenuClose : SubMenu -> Cmd msg
restoreFocusOnSubMenuClose _ =
    --    subMenuTriggerDomId menu |> Focus.focusId
    focusRoot


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


viewOpened : (SubMenu -> Html OpenedMsg) -> OpenedState -> Html OpenedMsg
viewOpened renderSubMenu { activeIdx, anchor, maybeSubMenuState } =
    Focus.styledFocusTracker
        [ rootStyles anchor ]
        [ A.id rootDomId
        , class "shadow-1 bg-white"
        , Key.onDown
            [ Key.up Prev
            , Key.down Next
            , Key.enterOrSpace ActiveSelected
            ]
        , Focus.onFocusLost FocusLost
        , tabindex 0
        ]
        (viewRootMenuItems activeIdx renderSubMenu)


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


viewRootMenuItems : Maybe Int -> (SubMenu -> Html OpenedMsg) -> List (Html OpenedMsg)
viewRootMenuItems activeIdx renderSubMenu =
    rootMenuItems
        |> List.indexedMap
            (\idx item ->
                let
                    keyboard =
                        if Just idx == activeIdx then
                            [ class "bg-light-blue" ]

                        else
                            []

                    attrs =
                        keyboard
                in
                case item.msg of
                    Edit ->
                        viewItem attrs Edit "Edit"

                    OpenSubMenu SelectProjectSubMenu ->
                        viewSubmenuTriggerItem attrs SelectProjectSubMenu renderSubMenu

                    _ ->
                        HX.none
            )


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


viewItem : List (H.Attribute msg) -> msg -> String -> Html msg
viewItem attrs action title =
    TextButton.view (class "ph2 pv1" :: tabindex -1 :: attrs)
        action
        title
