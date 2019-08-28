module SchedulePopup exposing (Model, Msg, initialValue, openFor, update, view)

import Accessibility.Styled.Key as Key
import Calendar
import Focus
import Html.Styled as H exposing (Html, div, text)
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (preventDefaultOn)
import HtmlStyledExtra as HX
import Millis
import Time
import Todo
import TodoId exposing (TodoId)
import UI.TextButton as TextButton
import UpdateExtra exposing (command, pure)


type Model
    = Open TodoId
    | Closed


initialValue =
    Closed


isOpenFor : TodoId -> Model -> Bool
isOpenFor todoId_ model =
    case model of
        Open tid ->
            todoId_ == tid

        Closed ->
            False


type Msg
    = OpenFor TodoId
    | Close
    | OnSetSchedule TodoId Todo.DueAt


openFor =
    OpenFor


update :
    { focus : String -> Cmd msg
    , onSetSchedule : TodoId -> Todo.DueAt -> msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config message model =
    case message of
        OpenFor todoId ->
            Open todoId
                |> pure
                |> command (config.focus firstFocusable)

        Close ->
            pure Closed

        OnSetSchedule todoId dueAt ->
            pure Closed


view :
    { toMsg : Msg -> msg
    }
    -> Time.Zone
    -> Calendar.Date
    -> TodoId
    -> Model
    -> Html msg
view conf here today todoId model =
    HX.viewIf (isOpenFor todoId model) (viewHelp conf here today todoId)


firstFocusable =
    "schedule-popup__first-focusable"


popupContainer =
    "schedule-popup__container"


viewHelp : { a | toMsg : Msg -> msg } -> Time.Zone -> Calendar.Date -> TodoId -> Html msg
viewHelp conf zone today todoId =
    let
        todayFmt =
            Millis.formatDate "ddd MMM yyyy" zone (Calendar.toMillis today)

        yesterday =
            Calendar.decrementDay today

        yesterdayFmt =
            Millis.formatDate "ddd MMM yyyy" zone (yesterday |> Calendar.toMillis)

        setDueMsg : Todo.DueAt -> msg
        setDueMsg =
            conf.toMsg << OnSetSchedule todoId

        viewSetDueButton action label attrs =
            TextButton.view (setDueMsg <| action) label (class "ph3 pv2" :: attrs)

        closeMsg =
            conf.toMsg Close
    in
    div
        [ A.id popupContainer
        , class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , Focus.onFocusOutsideDomId popupContainer closeMsg
        , preventDefaultOn "keydown" (Key.escape ( closeMsg, True ))
        ]
        [ div [ class "bg-white pa3 lh-copy shadow-1" ]
            [ div [ class " b  " ] [ text "Due Date" ]
            , viewSetDueButton (Todo.DueAt <| Calendar.toMillis today)
                ("Today: " ++ todayFmt)
                [ A.id firstFocusable ]
            , viewSetDueButton (Todo.DueAt <| Calendar.toMillis yesterday)
                ("Yesterday: " ++ yesterdayFmt)
                []
            , viewSetDueButton Todo.NoDue "No Due Date" []
            ]
        ]
