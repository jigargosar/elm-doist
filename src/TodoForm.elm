module TodoForm exposing
    ( AddAt(..)
    , TodoForm(..)
    , TodoFormFields
    , TodoFormMsg(..)
    , onTodoFormMsg
    , viewTodoForm
    , viewTodoItemFormFields
    )

-- TODO_ FORM

import Fire
import Html.Styled as H exposing (Attribute, Html, div, text, textarea)
import Html.Styled.Attributes as A exposing (class, rows)
import Html.Styled.Events exposing (onInput)
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)
import String.Extra as SX
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UI.TextButton as TextButton


type alias TodoFormFields =
    { title : String, dueAt : Todo.DueAt, projectId : ProjectId }


initTodoFormFields : Todo -> TodoFormFields
initTodoFormFields todo =
    { title = todo.title, dueAt = todo.dueAt, projectId = todo.projectId }


type AddAt
    = Start
    | End


type alias EditTodoFormInfo =
    { todoId : TodoId
    , initial : TodoFormFields
    , fields : TodoFormFields
    }


type alias AddTodoFormInfo =
    { addAt : AddAt
    , fields : TodoFormFields
    }


type TodoFormKind
    = AddKind AddAt
    | EditKind TodoId TodoFormFields


type alias TodoFormAlias =
    { fields : TodoFormFields, kind : TodoFormKind }


type TodoForm
    = EditTodoForm EditTodoFormInfo
    | AddTodoForm AddTodoFormInfo


initAddTodoForm : AddAt -> ProjectId -> TodoForm
initAddTodoForm addAt projectId =
    AddTodoForm <| AddTodoFormInfo addAt { title = "", dueAt = Todo.notDue, projectId = projectId }


initEditTodoForm : Todo -> TodoForm
initEditTodoForm todo =
    EditTodoForm (EditTodoFormInfo todo.id (initTodoFormFields todo) (initTodoFormFields todo))


type TodoFormMsg
    = TodoFormSaveClicked
    | TodoFormChanged TodoForm
    | TodoFormDeleteClicked
    | TodoFormCancelClicked
    | AddNewTodoClicked AddAt ProjectId
    | EditTodoClicked Todo



-- Update: TodoForm Helpers


onTodoFormMsg :
    { patchTodoCmd : TodoId -> List Todo.Msg -> Cmd msg
    , addNewTodoCmd : TodoFormFields -> Cmd msg
    }
    -> TodoFormMsg
    -> { b | maybeTodoForm : Maybe TodoForm }
    -> ( { b | maybeTodoForm : Maybe TodoForm }, Cmd msg )
onTodoFormMsg config message model =
    let
        persistEditing : EditTodoFormInfo -> Cmd msg
        persistEditing =
            patchEditingTodoCmd config

        persistNew : AddTodoFormInfo -> Cmd msg
        persistNew =
            persistNewTodoCmd config
    in
    case message of
        AddNewTodoClicked addAt projectId ->
            let
                addTodoForm =
                    initAddTodoForm addAt projectId
                        |> Just
            in
            case model.maybeTodoForm of
                Nothing ->
                    ( { model | maybeTodoForm = addTodoForm }, Cmd.none )

                Just (AddTodoForm _) ->
                    ( model, Cmd.none )

                Just (EditTodoForm editInfo) ->
                    ( { model | maybeTodoForm = addTodoForm }
                    , persistEditing editInfo
                    )

        EditTodoClicked todo ->
            ( { model | maybeTodoForm = initEditTodoForm todo |> Just }
            , case model.maybeTodoForm of
                Nothing ->
                    Cmd.none

                Just (AddTodoForm addInfo) ->
                    persistNew addInfo

                Just (EditTodoForm editInfo) ->
                    if editInfo.todoId == todo.id then
                        Cmd.none

                    else
                        persistEditing editInfo
            )

        TodoFormChanged form ->
            ( { model | maybeTodoForm = Just form }, Cmd.none )

        TodoFormSaveClicked ->
            ( { model | maybeTodoForm = Nothing }
            , case model.maybeTodoForm of
                Nothing ->
                    Cmd.none

                Just (EditTodoForm editInfo) ->
                    persistEditing editInfo

                Just (AddTodoForm addInfo) ->
                    persistNew addInfo
            )

        TodoFormDeleteClicked ->
            ( model
            , case model.maybeTodoForm of
                Nothing ->
                    Cmd.none

                Just (EditTodoForm editInfo) ->
                    Fire.deleteTodo editInfo.todoId

                Just (AddTodoForm _) ->
                    Cmd.none
            )

        TodoFormCancelClicked ->
            ( { model | maybeTodoForm = Nothing }, Cmd.none )


persistNewTodoCmd :
    { a | addNewTodoCmd : TodoFormFields -> Cmd msg }
    -> AddTodoFormInfo
    -> Cmd msg
persistNewTodoCmd config { fields } =
    if SX.isBlank fields.title then
        Cmd.none

    else
        config.addNewTodoCmd fields


patchEditingTodoCmd :
    { a | patchTodoCmd : TodoId -> List Todo.Msg -> Cmd msg }
    -> EditTodoFormInfo
    -> Cmd msg
patchEditingTodoCmd config editInfo =
    let
        { todoId, initial, fields } =
            editInfo

        msgList : List Todo.Msg
        msgList =
            [ if initial.title /= fields.title then
                Just <| Todo.SetTitle fields.title

              else
                Nothing
            , if initial.dueAt /= fields.dueAt then
                Just <| Todo.SetDueAt fields.dueAt

              else
                Nothing
            , if initial.projectId /= fields.projectId then
                Just <| Todo.SetProjectId fields.projectId

              else
                Nothing
            ]
                |> List.filterMap identity
    in
    if List.isEmpty msgList then
        Cmd.none

    else
        config.patchTodoCmd todoId msgList


viewTodoForm : (TodoFormMsg -> msg) -> TodoForm -> Html msg
viewTodoForm toMsg model =
    let
        config =
            { save = toMsg TodoFormSaveClicked
            , cancel = toMsg TodoFormCancelClicked
            , delete = toMsg TodoFormDeleteClicked
            }
    in
    case model of
        EditTodoForm editInfo ->
            let
                current =
                    editInfo.fields
            in
            viewTodoItemFormFields
                config
                (\title ->
                    toMsg <|
                        TodoFormChanged (EditTodoForm { editInfo | fields = { current | title = title } })
                )
                editInfo.fields

        AddTodoForm addInfo ->
            let
                { fields } =
                    addInfo
            in
            viewTodoItemFormFields
                config
                (\title ->
                    toMsg <|
                        TodoFormChanged (AddTodoForm <| { addInfo | fields = { fields | title = title } })
                )
                fields



-- VIEW


type alias TodoFormFieldsViewConfig msg =
    { save : msg
    , cancel : msg
    , delete : msg
    }


viewTodoItemFormFields : TodoFormFieldsViewConfig msg -> (String -> msg) -> TodoFormFields -> Html msg
viewTodoItemFormFields config titleChangedMsg fields =
    div [ class "pa3" ]
        [ div [ class "flex" ]
            [ div [ class "flex-grow-1" ]
                [ H.node "auto-resize-textarea"
                    [ A.property "textAreaValue" (JE.string fields.title) ]
                    [ textarea
                        [ class "pa0 lh-copy overflow-hidden w-100"
                        , rows 1
                        , onInput titleChangedMsg
                        ]
                        []
                    ]
                ]
            , div [] [ text "schedule" ]
            ]
        , div [ class "flex hs3 lh-copy" ]
            [ TextButton.primary config.save "Save" []
            , TextButton.primary config.cancel "Cancel" []
            , TextButton.primary config.delete "Delete" []
            ]
        ]
