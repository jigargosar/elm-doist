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


type Model
    = PopupOpen TodoId SubPopup
    | PopupClosed


init : Model
init =
    PopupClosed


opened : TodoId -> Model
opened todoId =
    PopupOpen todoId NoSubPopup


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
    -> Model
    -> ( Model, Cmd msg )
update { focus, closedBy } msg model =
    case msg of
        OpenPopup todoId ->
            ( opened todoId, focus firstFocusable )

        SetSubPopup subPopup ->
            case model of
                PopupClosed ->
                    ( model, Cmd.none )

                PopupOpen todoId _ ->
                    ( PopupOpen todoId subPopup
                    , case subPopup of
                        MoveSubPopup ->
                            focus MovePopup.firstFocusable

                        ScheduleSubPopup ->
                            focus SchedulePopup.schedulePopupFirstFocusableDomId

                        NoSubPopup ->
                            Cmd.none
                    )

        ClosePopup by ->
            ( closed, closedBy by )



--  VIEW


movePopupConfig : TodoId -> MovePopup.ViewConfig Msg
movePopupConfig todoId =
    { close = SetSubPopup NoSubPopup
    , move = ClosePopup << Move todoId
    }


schedulePopupConfig : TodoId -> SchedulePopup.ViewConfig Msg
schedulePopupConfig todoId =
    { close = SetSubPopup NoSubPopup
    , schedule = ClosePopup << Schedule todoId
    }


view : (Msg -> msg) -> TodoId -> (SubPopup -> Html Msg) -> Model -> Html msg
view toMsg todoId viewSubPopup model =
    (case model of
        PopupClosed ->
            HX.none

        PopupOpen todoId_ subPopup_ ->
            if todoId /= todoId_ then
                HX.none

            else
                viewHelp
                    todoId
                    (\subPopup ->
                        if subPopup /= subPopup_ then
                            HX.none

                        else
                            viewSubPopup subPopup
                    )
    )
        |> H.map toMsg


viewHelp todoId viewSubPopup =
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

            viewBtn id action label =
                TextButton.view action
                    label
                    [ class "pa2", A.id id ]

            viewFirstBtn : msg -> String -> Html msg
            viewFirstBtn =
                viewBtn firstFocusable

            viewRemainingBtn : msg -> String -> Html msg
            viewRemainingBtn =
                viewBtn ""
         in
         [ containerDiv
            [ viewFirstBtn (ClosePopup <| Edit todoId) "Edit"
            ]
         , containerDiv
            [ viewRemainingBtn (SetSubPopup MoveSubPopup) "Move to Project"
            , viewSubPopup MoveSubPopup
            ]
         , containerDiv
            [ viewRemainingBtn (SetSubPopup ScheduleSubPopup) "Schedule"
            , viewSubPopup ScheduleSubPopup
            ]
         , containerDiv
            [ viewRemainingBtn (ClosePopup <| Delete todoId) "Delete"
            ]
         ]
        )
