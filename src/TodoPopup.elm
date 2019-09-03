module TodoPopup exposing (SubPopup(..), TodoPopupModel, ViewConfig, closed, firstFocusable, init, opened, openedWithSub, view)

import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (on)
import HtmlExtra as HX
import Json.Decode as JD exposing (Decoder)
import MovePopup
import ProjectId exposing (ProjectId)
import SchedulePopup
import Todo
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type TodoPopupModel
    = TodoPopupOpen TodoId SubPopup
    | TodoPopupClosed


init : TodoPopupModel
init =
    TodoPopupClosed


opened : TodoId -> TodoPopupModel
opened todoId =
    TodoPopupOpen todoId NoSubPopup


closed : TodoPopupModel
closed =
    TodoPopupClosed


openedWithSub : TodoId -> SubPopup -> TodoPopupModel
openedWithSub =
    TodoPopupOpen


type SubPopup
    = ScheduleSubPopup
    | MoveSubPopup
    | NoSubPopup


firstFocusable : String
firstFocusable =
    "todo-popup--first-focusable"


type Msg
    = SetSubPopup TodoId SubPopup
    | OpenPopup TodoId
    | ClosePopup


type ClosedBy
    = Cancel
    | Schedule TodoId Todo.DueAt
    | Move TodoId ProjectId
    | Delete TodoId
    | Edit TodoId


update { focus } msg _ =
    case msg of
        SetSubPopup todoId subPopup ->
            ( TodoPopupOpen todoId subPopup
            , case subPopup of
                MoveSubPopup ->
                    focus MovePopup.firstFocusable

                ScheduleSubPopup ->
                    focus SchedulePopup.schedulePopupFirstFocusableDomId

                NoSubPopup ->
                    Cmd.none
            )

        OpenPopup todoId ->
            ( opened todoId, focus firstFocusable )

        ClosePopup ->
            ( closed, Cmd.none )



-- VIEW


type alias ViewConfig msg =
    { edit : TodoId -> msg
    , move : TodoId -> msg
    , delete : TodoId -> msg
    , schedule : TodoId -> msg
    , close : msg
    }


view :
    ViewConfig msg
    -> TodoId
    -> (SubPopup -> Html msg)
    -> TodoPopupModel
    -> Html msg
view config todoId viewSubPopup model =
    case model of
        TodoPopupClosed ->
            HX.none

        TodoPopupOpen todoId_ subPopup_ ->
            if todoId /= todoId_ then
                HX.none

            else
                viewHelp
                    config
                    todoId
                    (\subPopup ->
                        if subPopup /= subPopup_ then
                            HX.none

                        else
                            viewSubPopup subPopup
                    )


viewHelp :
    ViewConfig msg
    -> TodoId
    -> (SubPopup -> Html msg)
    -> Html msg
viewHelp config todoId viewSubPopup =
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , on "focusOutside" (JD.succeed config.close)
        , Key.onEscape config.close
        , tabindex -1
        ]
        (let
            containerDiv =
                div [ class "relative" ]
         in
         [ containerDiv
            [ TextButton.view (config.edit todoId)
                "Edit"
                [ class "pa2", A.id firstFocusable ]
            ]
         , containerDiv
            [ TextButton.view (config.move todoId)
                "Move to Project"
                [ class "pa2" ]
            , viewSubPopup MoveSubPopup
            ]
         , containerDiv
            [ TextButton.view (config.schedule todoId)
                "Schedule"
                [ class "pa2" ]
            , viewSubPopup ScheduleSubPopup
            ]
         , containerDiv
            [ TextButton.view (config.delete todoId)
                "Delete"
                [ class "pa2" ]
            ]
         ]
        )
