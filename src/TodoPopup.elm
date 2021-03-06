module TodoPopup exposing
    ( ClosedBy(..)
    , Model
    , Msg
    , SubPopup(..)
    , firstFocusable
    , getTodoId
    , init
    , movePopupConfig
    , open
    , schedulePopupConfig
    , subscriptions
    , triggerElDomId
    , update
    , view
    )

import Browser.Dom as Dom
import BrowserSize
import Calendar
import Css exposing (absolute, left, position, px, top, width)
import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events exposing (on)
import HtmlExtra as HX
import Json.Decode as JD exposing (Decoder)
import MovePopupOld
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import SchedulePopup
import Task
import Time
import Todo
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = PopupOpening TodoId
    | PopupOpened TodoId SubPopup Dom.Element
    | PopupClosed


init : Model
init =
    PopupClosed


closed : Model
closed =
    PopupClosed


type SubPopup
    = ScheduleSubPopup
    | MoveSubPopup
    | NoSubPopup


firstFocusable : String
firstFocusable =
    "todo-popup--first-focusable"


type Msg
    = SetSubPopup SubPopup
    | BrowserResized BrowserSize.BrowserSize
    | OpenPopup TodoId
    | GotTriggerElement (Result Dom.Error Dom.Element)
    | ClosePopup ClosedBy


type ClosedBy
    = Cancel
    | Schedule Todo.DueAt
    | Move ProjectId
    | Delete
    | Edit


open : TodoId -> Msg
open =
    OpenPopup


triggerElDomId : TodoId -> String
triggerElDomId todoId =
    "todo-popup__trigger-container__" ++ TodoId.toString todoId


getTodoId : Model -> Maybe TodoId
getTodoId model =
    case model of
        PopupOpening todoId ->
            Just todoId

        PopupOpened todoId _ _ ->
            Just todoId

        PopupClosed ->
            Nothing


subscriptions : (Msg -> msg) -> Model -> Sub msg
subscriptions toMsg _ =
    Sub.batch [ BrowserSize.onBrowserResize BrowserResized ]
        |> Sub.map toMsg


update :
    { a
        | focus : String -> Cmd msg
        , closedBy : { todoId : TodoId, closedBy : ClosedBy } -> Cmd msg
        , toMsg : Msg -> msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { focus, closedBy, toMsg } msg model =
    case msg of
        OpenPopup todoId ->
            ( PopupOpening todoId
            , Dom.getElement (triggerElDomId todoId)
                |> Task.attempt GotTriggerElement
                |> Cmd.map toMsg
            )

        BrowserResized _ ->
            case getTodoId model of
                Nothing ->
                    ( model, Cmd.none )

                Just todoId ->
                    ( model
                    , Dom.getElement (triggerElDomId todoId)
                        |> Task.attempt GotTriggerElement
                        |> Cmd.map toMsg
                    )

        GotTriggerElement triggerElResult ->
            case model of
                PopupOpening todoId ->
                    case triggerElResult of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok triggerEl ->
                            ( PopupOpened todoId NoSubPopup triggerEl, focus firstFocusable )

                PopupOpened todoId subPopup _ ->
                    case triggerElResult of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok triggerEl ->
                            ( PopupOpened todoId subPopup triggerEl, Cmd.none )

                PopupClosed ->
                    ( model, Cmd.none )

        SetSubPopup subPopup ->
            case model of
                PopupOpening _ ->
                    ( model, Cmd.none )

                PopupClosed ->
                    ( model, Cmd.none )

                PopupOpened todoId _ triggerEl ->
                    ( PopupOpened todoId subPopup triggerEl
                    , case subPopup of
                        MoveSubPopup ->
                            focus MovePopupOld.firstFocusable

                        ScheduleSubPopup ->
                            focus SchedulePopup.schedulePopupFirstFocusableDomId

                        NoSubPopup ->
                            Cmd.none
                    )

        ClosePopup by ->
            case model of
                PopupClosed ->
                    ( model, Cmd.none )

                PopupOpened todoId _ _ ->
                    ( closed, closedBy { todoId = todoId, closedBy = by } )

                PopupOpening _ ->
                    ( PopupClosed, Cmd.none )



--  VIEW


movePopupConfig : MovePopupOld.ViewConfig Msg
movePopupConfig =
    { close = SetSubPopup NoSubPopup
    , move = ClosePopup << Move
    }


schedulePopupConfig : SchedulePopup.ViewConfig Msg
schedulePopupConfig =
    { close = SetSubPopup NoSubPopup
    , schedule = ClosePopup << Schedule
    }


view :
    (Msg -> msg)
    -> ProjectId
    -> List Project
    -> Time.Zone
    -> Calendar.Date
    -> Model
    -> Html msg
view toMsg projectId projectList zone today model =
    (case model of
        PopupClosed ->
            HX.none

        PopupOpening _ ->
            HX.none

        PopupOpened _ subPopup_ triggerEl ->
            viewHelp triggerEl
                (\subPopup ->
                    if subPopup /= subPopup_ then
                        HX.none

                    else
                        case subPopup of
                            NoSubPopup ->
                                HX.none

                            MoveSubPopup ->
                                MovePopupOld.view
                                    movePopupConfig
                                    projectId
                                    projectList

                            ScheduleSubPopup ->
                                SchedulePopup.view
                                    schedulePopupConfig
                                    zone
                                    today
                )
    )
        |> H.map toMsg


type alias MenuItem =
    { action : Msg, label : String, subPopup : SubPopup }


menuItems : List MenuItem
menuItems =
    [ closeByMenuItem Edit "Edit"
    , openSubMenuItem MoveSubPopup "Move to Project"
    , openSubMenuItem ScheduleSubPopup "Schedule"
    , closeByMenuItem Delete "Delete"
    ]


popupWidth =
    256


viewHelp : Dom.Element -> (SubPopup -> Html Msg) -> Html Msg
viewHelp triggerEl viewSubPopup =
    H.node "track-focus-outside"
        [ css
            [ position absolute
            , top <| px <| triggerEl.element.y + 20
            , left <| px <| triggerEl.element.x + triggerEl.element.width - popupWidth
            , width <| px popupWidth
            ]
        , class "bg-white shadow-1"
        , class "z-1" -- if removed; causes flickering with hover icons
        , on "focusOutside" (JD.succeed (ClosePopup Cancel))
        , Key.onEscape (ClosePopup Cancel)
        , tabindex -1
        ]
        (menuItems |> List.map (viewMenuItem viewSubPopup))


viewMenuItem viewSubPopup menuItem =
    let
        id =
            if Just menuItem == maybeFirstMenuItem then
                firstFocusable

            else
                ""
    in
    div [ class "relative" ]
        [ TextButton.view_
            menuItem.action
            menuItem.label
            [ class "pa2"
            , A.id id
            ]
        , viewSubPopup menuItem.subPopup
        ]


closeByMenuItem : ClosedBy -> String -> MenuItem
closeByMenuItem closeBy label =
    MenuItem (ClosePopup closeBy) label NoSubPopup


openSubMenuItem : SubPopup -> String -> MenuItem
openSubMenuItem subPopup label =
    MenuItem (SetSubPopup subPopup) label subPopup


maybeFirstMenuItem =
    List.head menuItems
