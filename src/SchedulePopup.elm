module SchedulePopup exposing (Model, Msg, initialValue, openFor, update, view)

import Accessibility.Styled.Key as Key
import Focus
import Html.Styled as H exposing (Html, div, text)
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (preventDefaultOn)
import HtmlStyledExtra as HX
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)
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


openFor =
    OpenFor


update : { focus : String -> Cmd msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        OpenFor todoId ->
            Open todoId
                |> pure
                |> command (config.focus firstFocusable)

        Close ->
            pure Closed


view : (Msg -> msg) -> TodoId -> Model -> Html msg
view toMsg todoId model =
    HX.viewIf (isOpenFor todoId model) viewHelp
        |> H.map toMsg


firstFocusable =
    "schedule-popup__first-focusable"


popupContainer =
    "schedule-popup__container"


viewHelp : Html Msg
viewHelp =
    div
        [ A.id popupContainer
        , class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , Focus.onFocusOutsideDomId popupContainer Close
        , preventDefaultOn "keydown" (Key.escape ( Close, True ))
        ]
        --               (menuItemModelList |> List.indexedMap viewMenuItem)
        [ div [ A.id firstFocusable, tabindex 0, class "pointer pa3" ] [ text "schedule popup" ] ]
