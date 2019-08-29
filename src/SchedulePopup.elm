module SchedulePopup exposing
    ( Location(..)
    , Model
    , Msg
    , initialValue
    , openFor
    , update
    , view
    )

import Accessibility.Styled.Key as Key
import Calendar
import Html.Styled as H exposing (Html, div, text)
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (on, preventDefaultOn)
import HtmlStyledExtra as HX
import Json.Decode as JD
import Millis
import Time
import Todo
import TodoId exposing (TodoId)
import UI.TextButton as TextButton
import UpdateExtra exposing (command, pure, toCmd)


type Location
    = TodoPopup
    | InlineEditTodo
    | TodoItem


type Model
    = Opened Location TodoId
    | Closed


initialValue =
    Closed


type Msg
    = OpenFor Location TodoId
    | Close Bool
    | OnSetScheduleAndClose Todo.DueAt


openFor : Location -> TodoId -> Msg
openFor =
    OpenFor


update :
    { focus : String -> Cmd msg
    , onClose : Location -> TodoId -> Bool -> Maybe Todo.DueAt -> msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update conf message model =
    case message of
        OpenFor loc todoId ->
            Opened loc todoId
                |> pure
                |> command (conf.focus firstFocusable)

        Close restoreFocus ->
            onClose conf restoreFocus Nothing model

        OnSetScheduleAndClose dueAt ->
            onClose conf False (Just dueAt) model


onClose conf restoreFocus maybeDueAt model =
    case model of
        Opened loc todoId ->
            ( Closed
            , conf.onClose loc todoId restoreFocus maybeDueAt
                |> toCmd
            )

        Closed ->
            pure model


isOpenFor : Location -> TodoId -> Model -> Bool
isOpenFor loc todoId model =
    model == Opened loc todoId


view :
    { toMsg : Msg -> msg
    , location : Location
    }
    -> Time.Zone
    -> Calendar.Date
    -> TodoId
    -> Model
    -> Html msg
view conf here today todoId model =
    HX.viewIf (isOpenFor conf.location todoId model) (viewHelp conf here today todoId)


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
            conf.toMsg << OnSetScheduleAndClose

        viewSetDueButton action label attrs =
            TextButton.view (setDueMsg <| action) label (class "ph3 pv2" :: attrs)

        closeMsg restoreFocus =
            conf.toMsg <| Close restoreFocus
    in
    H.node "track-focus-outside"
        [ A.id popupContainer
        , class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons

        --        , Focus.onFocusOutsideDomId popupContainer (conf.toMsg FocusOutside)
        , on "focusOutside" (JD.succeed <| closeMsg False)
        , preventDefaultOn "keydown"
            (JD.lazy
                (\_ ->
                    JD.field "defaultPrevented" JD.bool
                        |> JD.andThen
                            (\defaultPrevented ->
                                if defaultPrevented then
                                    JD.fail "defaultPrevented"

                                else
                                    Key.escape ( closeMsg True, True )
                            )
                )
            )
        , tabindex -1
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
