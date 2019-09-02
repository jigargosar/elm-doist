module IET exposing (Config, Model, Msg, initial, startEditing, update, viewEditingForTodoId)

import Browser.Dom as Dom
import Calendar
import Css exposing (minWidth, none, px, resize)
import Html.Styled as H exposing (Attribute, Html, div, textarea)
import Html.Styled.Attributes as A
    exposing
        ( class
        , css
        , rows
        , tabindex
        , value
        )
import Html.Styled.Events exposing (onInput)
import HtmlExtra as HX
import Json.Encode exposing (Value)
import Maybe exposing (Maybe)
import Maybe.Extra as MX
import Millis exposing (Millis)
import SchedulePopup
import Task
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type Editable a
    = Editable a a


getCurrent : Editable a -> a
getCurrent (Editable _ a) =
    a


initEditable : a -> Editable a
initEditable new =
    Editable new new


changeEditable : a -> Editable a -> Editable a
changeEditable new (Editable old _) =
    Editable old new


isDirty (Editable old new) =
    old /= new


type alias Edit =
    { todoId : TodoId
    , title : Editable String
    , dueAt : Editable Todo.DueAt
    , schedulePopupOpened : Bool
    }


initEdit : TodoId -> { title : String, dueAt : DueAt } -> Edit
initEdit todoId { title, dueAt } =
    Edit todoId (initEditable title) (initEditable dueAt) False


type Model
    = Editing Edit
    | Closed


initial : Model
initial =
    Closed


firstFocusableDomId =
    "inline-edit-todo__first-focusable"


schedulePopupFirstFocusableDomId : String
schedulePopupFirstFocusableDomId =
    "inline-edit-todo__schedule-popup__first-focusable"


type UpdateMsg
    = Cancel
    | Save
    | OpenSchedulePopup
    | CloseSchedulePopup
    | TitleChanged String
    | DueAtChanged Todo.DueAt


type Msg
    = StartEditing TodoId { title : String, dueAt : Todo.DueAt }
    | OnUpdateMsg UpdateMsg


startEditing : TodoId -> { title : String, dueAt : DueAt } -> Msg
startEditing =
    StartEditing


type alias Config msg =
    { onSaveOrOverwrite : TodoId -> { title : String, dueAt : Todo.DueAt } -> msg
    , focus : String -> msg
    , onChanged : Value -> msg
    , toMsg : Msg -> msg
    }


hasChanges : Edit -> Bool
hasChanges edit =
    isDirty edit.dueAt || isDirty edit.title


saveIfDirty : Config msg -> Edit -> Cmd msg
saveIfDirty config edit =
    if hasChanges edit then
        config.onSaveOrOverwrite edit.todoId
            { title = getCurrent edit.title
            , dueAt = getCurrent edit.dueAt
            }
            |> Task.succeed
            |> Task.perform identity

    else
        Cmd.none


update :
    Config msg
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config message model =
    let
        returnNoOp =
            ( model, Cmd.none )
    in
    case message of
        StartEditing todoId fields ->
            case model of
                Closed ->
                    ( Editing <| initEdit todoId fields, Cmd.none )

                Editing edit ->
                    ( Editing <| initEdit todoId fields
                    , saveIfDirty config edit
                    )

        OnUpdateMsg msg ->
            case model of
                Closed ->
                    returnNoOp

                Editing edit ->
                    case msg of
                        Cancel ->
                            ( Closed, Cmd.none )

                        Save ->
                            ( Closed, saveIfDirty config edit )

                        OpenSchedulePopup ->
                            ( Editing
                                { edit
                                    | schedulePopupOpened = True
                                }
                            , Cmd.none
                            )

                        CloseSchedulePopup ->
                            ( Editing
                                { edit | schedulePopupOpened = False }
                            , Cmd.none
                            )

                        TitleChanged newTitle ->
                            ( Editing
                                { edit | title = changeEditable newTitle edit.title }
                            , Cmd.none
                            )

                        DueAtChanged newDueAt ->
                            ( Editing { edit | dueAt = changeEditable newDueAt edit.dueAt }
                            , Cmd.none
                            )


type alias ViewConfig msg =
    { openSchedulePopup : msg
    , titleChanged : String -> msg
    , cancel : msg
    , save : msg
    , viewSchedulePopup : Bool -> Zone -> Calendar.Date -> Html msg
    }


schedulePopupConfig_ =
    { close = CloseSchedulePopup
    , dueAtSelected = DueAtChanged
    , firstFocusableDomId = schedulePopupFirstFocusableDomId
    }


viewConfig : ViewConfig UpdateMsg
viewConfig =
    { openSchedulePopup = OpenSchedulePopup
    , titleChanged = TitleChanged
    , cancel = Cancel
    , save = Save
    , viewSchedulePopup =
        \isOpen zone today ->
            if isOpen then
                SchedulePopup.view schedulePopupConfig_ zone today

            else
                HX.empty
    }


viewEditingForTodoId : (Msg -> msg) -> TodoId -> Zone -> Calendar.Date -> Model -> Maybe (Html msg)
viewEditingForTodoId toMsg todoId zone today model =
    case model of
        Editing editModel ->
            if editModel.todoId == todoId then
                Just
                    (H.map (OnUpdateMsg >> toMsg)
                        (viewEditing viewConfig zone today editModel)
                    )

            else
                Nothing

        Closed ->
            Nothing


viewEditing :
    ViewConfig msg
    -> Time.Zone
    -> Calendar.Date
    -> Edit
    -> Html msg
viewEditing conf here today editModel =
    div
        [ class "pv3 ph2 "
        , tabindex 0
        , Key.onEscape conf.cancel
        ]
        [ div [ class "flex ba b--moon-gray" ]
            [ viewTitleInput conf editModel
            , viewDueAt conf here today editModel
            ]
        , div [ class "flex hs3 lh-copy" ]
            [ TextButton.primary conf.save "Save" [ class "pa2" ]
            , TextButton.secondary conf.cancel "Cancel" [ class "pa2" ]
            ]
        ]


viewTitleInput :
    ViewConfig msg
    -> Edit
    -> Html msg
viewTitleInput conf editModel =
    H.node "auto-resize-textarea"
        [ class "flex-grow-1 flex br b--moon-gray" ]
        [ textarea
            [ A.id firstFocusableDomId
            , class "pa1 flex-grow-1 lh-copy bn"
            , value <| getCurrent editModel.title
            , onInput conf.titleChanged
            , Key.onEnter conf.save
            , rows 1
            , css [ resize none ]
            , class "overflow-hidden"
            ]
            []
        ]


viewDueAt :
    ViewConfig msg
    -> Zone
    -> Calendar.Date
    -> Edit
    -> Html msg
viewDueAt conf here today edit =
    let
        ( dueAtLabel, dueAtCls ) =
            getCurrent edit.dueAt
                |> Todo.dueAtToMillis
                |> MX.unpack
                    (\_ -> ( "Schedule", "gray" ))
                    (\mi ->
                        ( Millis.formatDate "MMM dd" here <| mi, "near-black" )
                    )
    in
    div [ class "flex relative" ]
        [ TextButton.secondary conf.openSchedulePopup
            dueAtLabel
            [ class "pa1"
            , class dueAtCls
            , css [ minWidth <| px 100 ]
            ]
        , conf.viewSchedulePopup edit.schedulePopupOpened here today
        ]
