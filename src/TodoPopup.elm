module TodoPopup exposing
    ( ClosedBy(..)
    , Msg
    , SubPopup(..)
    , TodoPopupModel
    , firstFocusable
    , init
    , movePopupConfig
    , open
    , schedulePopupConfig
    , update
    , view
    )

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
    | ClosePopup ClosedBy


open : TodoId -> Msg
open =
    OpenPopup


type ClosedBy
    = Cancel
    | Schedule TodoId Todo.DueAt
    | Move TodoId ProjectId
    | Delete TodoId
    | Edit TodoId


update :
    { a
        | focus : String -> Cmd msg
        , closedBy : ClosedBy -> Cmd msg
    }
    -> Msg
    -> c
    -> ( TodoPopupModel, Cmd msg )
update { focus, closedBy } msg _ =
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

        ClosePopup by ->
            ( closed, closedBy by )



--  VIEW


movePopupConfig : TodoId -> MovePopup.ViewConfig Msg
movePopupConfig todoId =
    { close = SetSubPopup todoId NoSubPopup
    , move = ClosePopup << Move todoId
    }


schedulePopupConfig : TodoId -> SchedulePopup.ViewConfig Msg
schedulePopupConfig todoId =
    { close = SetSubPopup todoId NoSubPopup
    , schedule = ClosePopup << Schedule todoId
    }


view : (Msg -> msg) -> TodoId -> (SubPopup -> Html Msg) -> TodoPopupModel -> Html msg
view toMsg todoId viewSubPopup model =
    (case model of
        TodoPopupClosed ->
            HX.none

        TodoPopupOpen todoId_ subPopup_ ->
            if todoId /= todoId_ then
                HX.none

            else
                newViewHelp
                    todoId
                    (\subPopup ->
                        if subPopup /= subPopup_ then
                            HX.none

                        else
                            viewSubPopup subPopup
                    )
    )
        |> H.map toMsg


newViewHelp todoId viewSubPopup =
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , on "focusOutside" (JD.succeed (ClosePopup Cancel))
        , Key.onEscape (ClosePopup Cancel)
        , tabindex -1
        ]
        (let
            containerDiv =
                div [ class "relative" ]
         in
         [ containerDiv
            [ TextButton.view (ClosePopup <| Edit todoId)
                "Edit"
                [ class "pa2", A.id firstFocusable ]
            ]
         , containerDiv
            [ TextButton.view (SetSubPopup todoId MoveSubPopup)
                "Move to Project"
                [ class "pa2" ]
            , viewSubPopup MoveSubPopup
            ]
         , containerDiv
            [ TextButton.view (SetSubPopup todoId ScheduleSubPopup)
                "Schedule"
                [ class "pa2" ]
            , viewSubPopup ScheduleSubPopup
            ]
         , containerDiv
            [ TextButton.view (ClosePopup <| Delete todoId)
                "Delete"
                [ class "pa2" ]
            ]
         ]
        )
