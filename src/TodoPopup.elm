module TodoPopup exposing
    ( ClosedBy(..)
    , Model
    , Msg
    , SubPopup(..)
    , firstFocusable
    , init
    , isOpenFor
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


type ClosedBy
    = Cancel
    | Schedule Todo.DueAt
    | Move ProjectId
    | Delete
    | Edit


open : TodoId -> Msg
open =
    OpenPopup


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


isOpenFor : TodoId -> Model -> Bool
isOpenFor todoId model =
    case model of
        PopupClosed ->
            False

        PopupOpen todoId_ _ ->
            todoId == todoId_


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


type alias MenuItem =
    { action : Msg, label : String, subPopup : SubPopup }


menuItems : List MenuItem
menuItems =
    [ closeByMenuItem Edit "Edit"
    , openSubMenuItem MoveSubPopup "Move to Project"
    , openSubMenuItem ScheduleSubPopup "Schedule"
    , closeByMenuItem Delete "Delete"
    ]


viewHelp viewSubPopup =
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
