module SchedulePopup exposing
    ( SchedulePopupModel(..)
    , ViewConfig
    , schedulePopupFirstFocusableDomId
    , view
    )

import Calendar
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Millis exposing (Millis)
import Time exposing (Zone)
import Todo exposing (DueAt)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type SchedulePopupModel
    = SchedulePopupOpened TodoId
    | SchedulePopupClosed


schedulePopupFirstFocusableDomId : String
schedulePopupFirstFocusableDomId =
    "schedule-popup__first-focusable"


type alias ViewConfig msg =
    { close : msg
    , schedule : DueAt -> msg
    }


view :
    ViewConfig msg
    -> Zone
    -> Calendar.Date
    -> Html msg
view conf zone today =
    let
        todayFmt =
            Millis.formatDate "ddd MMM yyyy" zone (Calendar.toMillis today)

        yesterday =
            Calendar.decrementDay today

        yesterdayFmt =
            Millis.formatDate "ddd MMM yyyy" zone (yesterday |> Calendar.toMillis)

        setDueMsg : DueAt -> msg
        setDueMsg =
            -- SchedulePopupDueAtSelected
            conf.schedule

        viewSetDueButton dueAt label attrs =
            TextButton.view (setDueMsg <| dueAt) label (class "ph3 pv2" :: attrs)

        closeMsg : msg
        closeMsg =
            -- CloseSchedulePopup
            conf.close
    in
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , tabindex -1
        , on "focusOutside" (JD.succeed closeMsg)
        , Key.onEscape closeMsg
        ]
        [ div [ class "bg-white pa3 lh-copy shadow-1" ]
            [ div [ class " b  " ] [ text "Due Date" ]
            , viewSetDueButton (Todo.DueAt <| Calendar.toMillis today)
                ("Today: " ++ todayFmt)
                [ A.id schedulePopupFirstFocusableDomId ]
            , viewSetDueButton (Todo.DueAt <| Calendar.toMillis yesterday)
                ("Yesterday: " ++ yesterdayFmt)
                []
            , viewSetDueButton Todo.NoDue "No Due Date" []
            ]
        ]
