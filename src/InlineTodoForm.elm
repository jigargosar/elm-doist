module InlineTodoForm exposing
    ( AddAt(..)
    , Model
    , Msg
    , add
    , edit
    , init
    , update
    , view
    )

import Focus
import Html.Styled as H exposing (Html)
import Project exposing (ProjectList)
import ProjectId exposing (ProjectId)
import Task
import Todo exposing (Todo)
import TodoForm
import TodoId exposing (TodoId)


type Model
    = Opened OpenedState
    | Closed


type OpenedState
    = Add AddState
    | Edit EditState


type alias AddState =
    { addAt : AddAt, form : TodoForm.Model }


type alias EditState =
    { todo : Todo, form : TodoForm.Model }


type AddAt
    = Start
    | End


init : Model
init =
    Closed


getAddTodoForm : Model -> Maybe TodoForm.Model
getAddTodoForm model =
    case model of
        Opened (Add state) ->
            Just state.form

        _ ->
            Nothing


type Msg
    = AddClicked AddAt ProjectId
    | EditClicked Todo
    | TodoFormMsg TodoForm.Msg
    | SaveClicked TodoForm.Fields
    | CancelClicked
    | Focused Focus.FocusResult


add : AddAt -> ProjectId -> Msg
add =
    AddClicked


edit : Todo -> Msg
edit =
    EditClicked


getAddState : Model -> Maybe AddState
getAddState model =
    case model of
        Opened (Add state) ->
            Just state

        _ ->
            Nothing


update :
    { toMsg : Msg -> msg
    , onAdded : TodoForm.Fields -> msg
    , onEdited : TodoId -> List Todo.Msg -> msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config message model =
    case message of
        AddClicked addAt projectId ->
            let
                newModel =
                    model
                        |> getAddState
                        |> Maybe.map (\state -> { state | addAt = addAt })
                        |> Maybe.withDefault { addAt = addAt, form = TodoForm.fromProjectId projectId }
                        |> Add
                        |> Opened
            in
            ( newModel
            , Cmd.batch
                [ focusForm config
                , notifyIfEditing config model
                ]
            )

        EditClicked todo ->
            ( Opened (Edit { todo = todo, form = TodoForm.fromTodo todo })
            , Cmd.batch
                [ focusForm config
                , notifyAddedOrEdited config model
                ]
            )

        TodoFormMsg msg ->
            case model of
                Opened (Add state) ->
                    let
                        ( newTodoForm, cmd ) =
                            todoFormUpdate msg state.form
                    in
                    ( Opened (Add { state | form = newTodoForm }), Cmd.map config.toMsg cmd )

                Opened (Edit state) ->
                    let
                        ( newTodoForm, cmd ) =
                            todoFormUpdate msg state.form
                    in
                    ( Opened (Edit { state | form = newTodoForm }), Cmd.map config.toMsg cmd )

                Closed ->
                    ( model, Cmd.none )

        SaveClicked _ ->
            ( Closed, notifyAddedOrEdited config model )

        CancelClicked ->
            ( Closed, Cmd.none )

        Focused _ ->
            ( model, Cmd.none )


focusForm : { a | toMsg : Msg -> msg } -> Cmd msg
focusForm config =
    Focus.attempt Focused TodoForm.firstFocusable
        |> Cmd.map config.toMsg


todoFormUpdate : TodoForm.Msg -> TodoForm.Model -> ( TodoForm.Model, Cmd Msg )
todoFormUpdate =
    TodoForm.update
        { toMsg = TodoFormMsg
        , onSave = SaveClicked
        , onCancel = CancelClicked
        }


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


notifyEdited :
    { a | onEdited : TodoId -> List Todo.Msg -> msg }
    -> Todo
    -> TodoForm.Model
    -> Cmd msg
notifyEdited config todo todoForm =
    getPatchMsgList (TodoForm.getFields todoForm) todo
        |> config.onEdited todo.id
        |> perform


notifyIfEditing :
    { a | onEdited : TodoId -> List Todo.Msg -> msg }
    -> Model
    -> Cmd msg
notifyIfEditing config model =
    case model of
        Opened (Edit state) ->
            notifyEdited config state.todo state.form

        _ ->
            Cmd.none


notifyAdded : { a | onAdded : TodoForm.Fields -> msg } -> TodoForm.Model -> Cmd msg
notifyAdded config todoForm =
    TodoForm.getFields todoForm
        |> config.onAdded
        |> perform


notifyAddedOrEdited :
    { a
        | onAdded : TodoForm.Fields -> msg
        , onEdited : TodoId -> List Todo.Msg -> msg
    }
    -> Model
    -> Cmd msg
notifyAddedOrEdited config model =
    case model of
        Opened (Add state) ->
            notifyAdded config state.form

        Opened (Edit state) ->
            notifyEdited config state.todo state.form

        Closed ->
            Cmd.none


getPatchMsgList : TodoForm.Fields -> Todo -> List Todo.Msg
getPatchMsgList fields todo =
    let
        msgList =
            [ if todo.title /= fields.title then
                Just <| Todo.SetTitle fields.title

              else
                Nothing
            , if todo.projectId /= fields.projectId then
                Just <| Todo.SetProjectId fields.projectId

              else
                Nothing
            ]
                |> List.filterMap identity
    in
    msgList


view :
    (Msg -> msg)
    ->
        { closed : () -> out
        , add : AddAt -> Html msg -> out
        , edit : TodoId -> Html msg -> out
        }
    -> ProjectList
    -> Model
    -> out
view toMsg config projectList model =
    case model of
        Closed ->
            config.closed ()

        Opened openedState ->
            let
                todoFormView todoForm =
                    TodoForm.view projectList todoForm |> H.map (TodoFormMsg >> toMsg)
            in
            case openedState of
                Add { addAt, form } ->
                    config.add addAt (todoFormView form)

                Edit { todo, form } ->
                    config.edit todo.id (todoFormView form)
