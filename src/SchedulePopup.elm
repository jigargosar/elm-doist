module SchedulePopup exposing (Model, Msg, initialValue, openFor, update, view)

import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class)
import HtmlStyledExtra as HX
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)


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


openFor =
    OpenFor


update : Msg -> Model -> Model
update message model =
    case message of
        OpenFor todoId ->
            Open todoId


view : TodoId -> Model -> Html msg
view todoId model =
    HX.viewIf (isOpenFor todoId model)
        viewHelp


viewHelp =
    div
        [ {- A.id menuDomId
             ,
          -}
          class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons

        --               , Focus.onFocusOutsideDomId menuDomId (closeMsg False)
        --               , preventDefaultOn "keydown" (Key.escape ( closeMsg True, True ))
        ]
        --               (menuItemModelList |> List.indexedMap viewMenuItem)
        [ div [] [ text "schedule popup" ] ]
