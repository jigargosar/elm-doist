module TodoForm exposing
    ( AddAt(..)
    , TodoForm(..)
    , TodoFormMsg(..)
    , onTodoFormMsg
    , viewTodoForm
    )

-- TODO_ FORM

import AddTodoForm
import EditTodoForm
import Fire
import Html.Styled as H exposing (Attribute, Html)
import ProjectId exposing (ProjectId)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)


type AddAt
    = Start
    | End


type TodoForm
    = EditTodoForm
        { todoId : TodoId
        , form : EditTodoForm.Model
        }
    | AddTodoForm
        { addAt : AddAt
        , form : AddTodoForm.Model
        }


initAddTodoForm : AddAt -> ProjectId -> TodoForm
initAddTodoForm addAt projectId =
    AddTodoForm <|
        { addAt = addAt
        , form = AddTodoForm.init projectId
        }


initEditTodoForm : Todo -> TodoForm
initEditTodoForm todo =
    EditTodoForm { todoId = todo.id, form = EditTodoForm.init todo }


type TodoFormMsg
    = TodoFormSaveClicked
    | TodoFormCancelClicked
    | TodoFormChanged TodoForm
    | TodoFormDeleteClicked
    | AddNewTodoClicked AddAt ProjectId
    | EditTodoClicked Todo



-- Update: TodoForm Helpers


onTodoFormMsg :
    { persistEdit : EditTodoForm.Model -> Cmd msg
    , persistNew : AddTodoForm.Model -> Cmd msg
    }
    -> TodoFormMsg
    -> { b | maybeTodoForm : Maybe TodoForm }
    -> ( { b | maybeTodoForm : Maybe TodoForm }, Cmd msg )
onTodoFormMsg config message model =
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

                Just (EditTodoForm info) ->
                    ( { model | maybeTodoForm = addTodoForm }
                    , config.persistEdit info.form
                    )

        EditTodoClicked todo ->
            ( { model | maybeTodoForm = initEditTodoForm todo |> Just }
            , case model.maybeTodoForm of
                Nothing ->
                    Cmd.none

                Just (AddTodoForm info) ->
                    config.persistNew info.form

                Just (EditTodoForm info) ->
                    if info.todoId == todo.id then
                        Cmd.none

                    else
                        config.persistEdit info.form
            )

        TodoFormChanged form ->
            ( { model | maybeTodoForm = Just form }, Cmd.none )

        TodoFormSaveClicked ->
            ( { model | maybeTodoForm = Nothing }
            , case model.maybeTodoForm of
                Nothing ->
                    Cmd.none

                Just (EditTodoForm info) ->
                    config.persistEdit info.form

                Just (AddTodoForm info) ->
                    config.persistNew info.form
            )

        TodoFormDeleteClicked ->
            ( { model | maybeTodoForm = Nothing }
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


viewTodoForm : (TodoFormMsg -> msg) -> TodoForm -> Html msg
viewTodoForm toMsg model =
    case model of
        EditTodoForm info ->
            EditTodoForm.view
                { save = TodoFormSaveClicked
                , cancel = TodoFormSaveClicked
                , changed = \form -> TodoFormChanged <| EditTodoForm { info | form = form }
                , delete = TodoFormDeleteClicked
                }
                info.form
                |> H.map toMsg

        AddTodoForm info ->
            AddTodoForm.view
                { save = TodoFormSaveClicked
                , cancel = TodoFormSaveClicked
                , changed = \form -> TodoFormChanged <| AddTodoForm { info | form = form }
                }
                info.form
                |> H.map toMsg
