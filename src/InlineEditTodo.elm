module InlineEditTodo exposing
    ( Config
    , Model
    , Msg
    , initial
    , isOpenForTodoId
    , startEditing
    , update
    , view
    )

import Calendar
import Css exposing (minWidth, none, px, resize)
import Editable
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
import Maybe.Extra as MX
import Millis exposing (Millis)
import SchedulePopup
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type alias Edit =
    { todoId : TodoId
    , title : Editable.Editable String
    , dueAt : Editable.Editable Todo.DueAt
    , schedulePopupOpened : Bool
    }


initEdit : TodoId -> { title : String, dueAt : DueAt } -> Edit
initEdit todoId { title, dueAt } =
    Edit todoId (Editable.initEditable title) (Editable.initEditable dueAt) False


type Model
    = Editing Edit
    | Closed


initial : Model
initial =
    Closed


firstFocusableDomId =
    "inline-edit-todo__first-focusable"


type EditingMsg
    = Cancel
    | Save
    | OpenSchedulePopup
    | CloseSchedulePopup
    | TitleChanged String
    | DueAtChanged Todo.DueAt


type Msg
    = StartEditing TodoId { title : String, dueAt : Todo.DueAt }
    | OnEditingMsg EditingMsg


startEditing : TodoId -> { title : String, dueAt : DueAt } -> Msg
startEditing =
    StartEditing


type alias Config msg =
    { onSaveOrOverwrite : TodoId -> { title : String, dueAt : Todo.DueAt } -> Cmd msg
    , focus : String -> Cmd msg
    }


update :
    Config msg
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config message model =
    case message of
        StartEditing todoId fields ->
            case model of
                Closed ->
                    ( Editing <| initEdit todoId fields
                    , config.focus firstFocusableDomId
                    )

                Editing edit ->
                    ( Editing <| initEdit todoId fields
                    , Cmd.batch
                        [ saveIfDirty config edit
                        , config.focus firstFocusableDomId
                        ]
                    )

        OnEditingMsg msg ->
            case model of
                Closed ->
                    ( model, Cmd.none )

                Editing edit ->
                    updateEditingModel config msg edit


updateEditingModel : Config msg -> EditingMsg -> Edit -> ( Model, Cmd msg )
updateEditingModel config msg edit =
    case msg of
        Cancel ->
            ( Closed, Cmd.none )

        Save ->
            ( Closed, saveIfDirty config edit )

        OpenSchedulePopup ->
            ( Editing { edit | schedulePopupOpened = True }
            , config.focus SchedulePopup.schedulePopupFirstFocusableDomId
            )

        CloseSchedulePopup ->
            ( Editing { edit | schedulePopupOpened = False }
            , Cmd.none
            )

        TitleChanged newTitle ->
            ( Editing { edit | title = Editable.changeEditable newTitle edit.title }
            , Cmd.none
            )

        DueAtChanged newDueAt ->
            ( Editing
                { edit
                    | dueAt = Editable.changeEditable newDueAt edit.dueAt
                    , schedulePopupOpened = False
                }
            , Cmd.none
            )


hasChanges : Edit -> Bool
hasChanges edit =
    Editable.isDirty edit.dueAt || Editable.isDirty edit.title


saveIfDirty : Config msg -> Edit -> Cmd msg
saveIfDirty config edit =
    if hasChanges edit then
        config.onSaveOrOverwrite edit.todoId
            { title = Editable.getCurrent edit.title
            , dueAt = Editable.getCurrent edit.dueAt
            }

    else
        Cmd.none



-- VIEW


type alias ViewConfig msg =
    { openSchedulePopup : msg
    , titleChanged : String -> msg
    , cancel : msg
    , save : msg
    , viewSchedulePopup : Bool -> Zone -> Calendar.Date -> Html msg
    }


schedulePopupConfig_ : SchedulePopup.ViewConfig EditingMsg
schedulePopupConfig_ =
    { close = CloseSchedulePopup
    , schedule = DueAtChanged
    }


viewConfig : ViewConfig EditingMsg
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
                HX.none
    }


isOpenForTodoId : TodoId -> Model -> Bool
isOpenForTodoId todoId model =
    case model of
        Editing editModel ->
            editModel.todoId == todoId

        Closed ->
            False


view :
    (Msg -> msg)
    -> TodoId
    -> Zone
    -> Calendar.Date
    -> Model
    -> Html msg
view toMsg todoId zone today model =
    case model of
        Editing editModel ->
            if editModel.todoId == todoId then
                H.map (OnEditingMsg >> toMsg)
                    (viewEditing viewConfig zone today editModel)

            else
                HX.none

        Closed ->
            HX.none


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
            , value <| Editable.getCurrent editModel.title
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
            Editable.getCurrent edit.dueAt
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
