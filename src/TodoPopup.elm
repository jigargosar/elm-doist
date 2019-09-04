module TodoPopup exposing
    ( ClosedBy(..)
    , Model
    , Msg
    , SubPopup(..)
    , firstFocusable
    , init
    , movePopupConfig
    , open
    , schedulePopupConfig
    , triggerContainerDomId
    , update
    , view
    )

import Browser.Dom as Dom
import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes as A exposing (class, tabindex)
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


viewHelp : Dom.Element -> (SubPopup -> Html Msg) -> Html Msg
viewHelp triggerEl viewSubPopup =
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
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
