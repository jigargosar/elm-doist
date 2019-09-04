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
    , triggerContainerDomId
    , update
    , view
    )

import Browser.Dom as Dom
import Css exposing (absolute, fixed, left, position, px, right, sticky, top, width, zero)
import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events exposing (on)
import HtmlExtra as HX
import Json.Decode as JD exposing (Decoder)
import MovePopup
import ProjectId exposing (ProjectId)
import SchedulePopup
import Task
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


triggerContainerDomId : TodoId -> String
triggerContainerDomId todoId =
    "todo-popup__trigger-container__" ++ TodoId.toString todoId


getTodoId : Model -> Maybe TodoId
getTodoId model =
    case model of
        PopupOpening todoId ->
            Just todoId

        PopupOpened todoId subPopup element ->
            Just todoId

        PopupClosed ->
            Nothing


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
            , Dom.getElement (triggerContainerDomId todoId)
                |> Task.attempt GotTriggerElement
                |> Cmd.map toMsg
            )

        GotTriggerElement triggerElResult ->
            let
                _ =
                    Debug.log "TodoPopup: Trigger Element" triggerElResult
            in
            case model of
                PopupOpening todoId ->
                    case triggerElResult of
                        Err _ ->
                            ( model, Cmd.none )

                        Ok triggerEl ->
                            ( PopupOpened todoId NoSubPopup triggerEl, focus firstFocusable )

                PopupOpened todoId subPopup element ->
                    ( model, Cmd.none )

                PopupClosed ->
                    ( model, Cmd.none )

        SetSubPopup subPopup ->
            case model of
                PopupOpening todoId ->
                    ( model, Cmd.none )

                PopupClosed ->
                    ( model, Cmd.none )

                PopupOpened todoId _ triggerEl ->
                    ( PopupOpened todoId subPopup triggerEl
                    , case subPopup of
                        MoveSubPopup ->
                            focus MovePopup.firstFocusable

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


movePopupConfig : MovePopup.ViewConfig Msg
movePopupConfig =
    { close = SetSubPopup NoSubPopup
    , move = ClosePopup << Move
    }


schedulePopupConfig : SchedulePopup.ViewConfig Msg
schedulePopupConfig =
    { close = SetSubPopup NoSubPopup
    , schedule = ClosePopup << Schedule
    }


view : (Msg -> msg) -> TodoId -> (SubPopup -> Html Msg) -> Model -> Html msg
view toMsg todoId viewSubPopup model =
    (case model of
        PopupClosed ->
            HX.none

        PopupOpening _ ->
            HX.none

        PopupOpened todoId_ subPopup_ triggerEl ->
            if todoId /= todoId_ then
                HX.none

            else
                viewHelp triggerEl
                    (\subPopup ->
                        if subPopup /= subPopup_ then
                            HX.none

                        else
                            viewSubPopup subPopup
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
        [ TextButton.view
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
