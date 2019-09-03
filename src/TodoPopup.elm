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
    | Schedule Todo.DueAt
    | Move ProjectId
    | Delete
    | Edit


update :
    { a
        | focus : String -> Cmd msg
        , closedBy : { todoId : TodoId, closedBy : ClosedBy } -> Cmd msg
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
            case model of
                PopupClosed ->
                    ( model, Cmd.none )

                PopupOpen todoId _ ->
                    ( closed, closedBy { todoId = todoId, closedBy = by } )



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

        PopupOpen todoId_ subPopup_ ->
            if todoId /= todoId_ then
                HX.none

            else
                viewHelp
                    (\subPopup ->
                        if subPopup /= subPopup_ then
                            HX.none

                        else
                            viewSubPopup subPopup
                    )
    )
        |> H.map toMsg


viewHelp viewSubPopup =
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
            [ viewFirstBtn (ClosePopup Edit) "Edit"
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
            [ viewRemainingBtn (ClosePopup Delete) "Delete"
            ]
         ]
        )


type alias MenuItem =
    { action : Msg, label : String, subPopup : SubPopup }


closeMenuItem : ClosedBy -> String -> MenuItem
closeMenuItem closeBy label =
    MenuItem (ClosePopup closeBy) label NoSubPopup


openSubPopupMenuItem : SubPopup -> String -> MenuItem
openSubPopupMenuItem subPopup label =
    MenuItem (SetSubPopup subPopup) label subPopup


menuItems =
    [ closeMenuItem Edit "Edit"
    , openSubPopupMenuItem MoveSubPopup "Move to Project"
    , openSubPopupMenuItem ScheduleSubPopup "Schedule"
    , closeMenuItem Delete "Delete"
    ]
