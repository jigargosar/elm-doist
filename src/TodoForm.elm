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

import AddTodoForm
import EditTodoForm
import Fire
import Html.Styled as H exposing (Attribute, Html, div, text, textarea)
import Html.Styled.Attributes as A exposing (class, rows)
import Html.Styled.Events exposing (onInput)
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)
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
    , form : EditTodoForm.Model
    }


type alias AddTodoFormInfo =
    { addAt : AddAt
    , form : AddTodoForm.Model
    }


type TodoForm
    = EditTodoForm EditTodoFormInfo
    | AddTodoForm AddTodoFormInfo


initAddTodoForm : AddAt -> ProjectId -> TodoForm
initAddTodoForm addAt projectId =
    AddTodoForm <| AddTodoFormInfo addAt (AddTodoForm.init projectId)


initEditTodoForm : Todo -> TodoForm
initEditTodoForm todo =
    EditTodoForm (EditTodoFormInfo todo.id (EditTodoForm.init todo))


type TodoFormMsg
    = TodoFormSaveClicked
    | TodoFormChanged TodoForm
    | TodoFormDeleteClicked
    | TodoFormCancelClicked
    | AddNewTodoClicked AddAt ProjectId
    | EditTodoClicked Todo
    | AddTodoFormChanged AddTodoForm.Model
    | EditTodoFormChanged EditTodoForm.Model



-- Update: TodoForm Helpers


onTodoFormMsg :
    { persistEdit : EditTodoForm.Model -> Cmd msg
    , persistNew : AddTodoForm.Model -> Cmd msg
    }
    -> TodoFormMsg
    -> { b | maybeTodoForm : Maybe TodoForm }
    -> ( { b | maybeTodoForm : Maybe TodoForm }, Cmd msg )
onTodoFormMsg config message model =
    let
        persistEditing : EditTodoFormInfo -> Cmd msg
        persistEditing info =
            config.persistEdit info.form

        persistNew : AddTodoFormInfo -> Cmd msg
        persistNew info =
            config.persistNew info.form
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

        AddTodoFormChanged form ->
            case model.maybeTodoForm of
                Just (AddTodoForm addInfo) ->
                    ( { model | maybeTodoForm = Just (AddTodoForm { addInfo | form = form }) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditTodoFormChanged form ->
            case model.maybeTodoForm of
                Just (EditTodoForm info) ->
                    ( { model | maybeTodoForm = Just (EditTodoForm { info | form = form }) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
            EditTodoForm.view
                { save = TodoFormSaveClicked
                , cancel = TodoFormSaveClicked
                , changed = EditTodoFormChanged
                , delete = TodoFormDeleteClicked
                }
                editInfo.form
                |> H.map toMsg

        AddTodoForm addInfo ->
            AddTodoForm.view
                { save = TodoFormSaveClicked, cancel = TodoFormSaveClicked, changed = AddTodoFormChanged }
                addInfo.form
                |> H.map toMsg



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
