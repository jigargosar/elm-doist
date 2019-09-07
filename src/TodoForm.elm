module TodoForm exposing
    ( AddAt(..)
    , Model(..)
    , TodoFormMsg(..)
    , none
    , onTodoFormMsg
    , viewTodoForm
    )

-- TODO_ FORM

import AddTodoForm
import EditTodoForm
import Fire
import Html.Styled as H exposing (Attribute, Html)
import HtmlExtra as HX
import ProjectId exposing (ProjectId)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)


type AddAt
    = Start
    | End


type Model
    = EditTodoForm
        { todoId : TodoId
        , form : EditTodoForm.Model
        }
    | AddTodoForm
        { addAt : AddAt
        , form : AddTodoForm.Model
        }
    | NoTodoForm


none : Model
none =
    NoTodoForm


initAddTodoForm : AddAt -> ProjectId -> Model
initAddTodoForm addAt projectId =
    AddTodoForm <|
        { addAt = addAt
        , form = AddTodoForm.init projectId
        }


initEditTodoForm : Todo -> Model
initEditTodoForm todo =
    EditTodoForm { todoId = todo.id, form = EditTodoForm.init todo }


type TodoFormMsg
    = TodoFormSaveClicked
    | TodoFormCancelClicked
    | TodoFormChanged Model
    | TodoFormDeleteClicked
    | AddNewTodoClicked AddAt ProjectId
    | EditTodoClicked Todo



-- Update: TodoForm Helpers


onTodoFormMsg :
    { persistEdit : EditTodoForm.Model -> Cmd msg
    , persistNew : AddTodoForm.Model -> Cmd msg
    }
    -> TodoFormMsg
    -> { b | todoForm : Model }
    -> ( { b | todoForm : Model }, Cmd msg )
onTodoFormMsg config message model =
    case message of
        AddNewTodoClicked addAt projectId ->
            let
                addTodoForm =
                    initAddTodoForm addAt projectId
            in
            case model.todoForm of
                NoTodoForm ->
                    ( { model | todoForm = addTodoForm }, Cmd.none )

                AddTodoForm _ ->
                    ( model, Cmd.none )

                EditTodoForm info ->
                    ( { model | todoForm = addTodoForm }
                    , config.persistEdit info.form
                    )

        EditTodoClicked todo ->
            ( { model | todoForm = initEditTodoForm todo }
            , case model.todoForm of
                NoTodoForm ->
                    Cmd.none

                AddTodoForm info ->
                    config.persistNew info.form

                EditTodoForm info ->
                    if info.todoId == todo.id then
                        Cmd.none

                    else
                        config.persistEdit info.form
            )

        TodoFormChanged form ->
            ( { model | todoForm = form }, Cmd.none )

        TodoFormSaveClicked ->
            ( { model | todoForm = none }
            , case model.todoForm of
                NoTodoForm ->
                    Cmd.none

                EditTodoForm info ->
                    config.persistEdit info.form

                AddTodoForm info ->
                    config.persistNew info.form
            )

        TodoFormDeleteClicked ->
            ( { model | todoForm = none }
            , case model.todoForm of
                NoTodoForm ->
                    Cmd.none

                EditTodoForm editInfo ->
                    Fire.deleteTodo editInfo.todoId

                AddTodoForm _ ->
                    Cmd.none
            )

        TodoFormCancelClicked ->
            ( { model | todoForm = NoTodoForm }, Cmd.none )


viewTodoForm : (TodoFormMsg -> msg) -> Model -> Html msg
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

        NoTodoForm ->
            HX.none
