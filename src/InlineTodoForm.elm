module InlineTodoForm exposing (AddAt(..), Model, Msg, add, close, edit, init, update)

import ProjectId exposing (ProjectId)
import Task
import Todo exposing (Todo)
import TodoForm
import TodoId exposing (TodoId)


type Model
    = Model (Maybe Internal)


type alias Internal =
    ( Meta, TodoForm.Model )


type AddAt
    = Start
    | End


type Meta
    = AddTodoFormMeta AddAt
    | EditTodoFormMeta Todo


init : Model
init =
    Model Nothing


type Msg
    = AddClicked AddAt ProjectId
    | EditClicked Todo
    | Closed


add : AddAt -> ProjectId -> Msg
add =
    AddClicked


edit : Todo -> Msg
edit =
    EditClicked


close : Msg
close =
    Closed


unwrap : Model -> Maybe Internal
unwrap (Model internal) =
    internal


update :
    { toMsg : Msg -> msg
    , onAdded : TodoForm.Fields -> Cmd msg
    , onEdited : TodoId -> List Todo.Msg -> msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config msg model =
    case msg of
        AddClicked addAt projectId ->
            let
                addTodoForm : TodoForm.Model
                addTodoForm =
                    TodoForm.init "" Todo.notDue projectId

                addTodoFormMeta =
                    AddTodoFormMeta addAt

                addTodoFormWithMeta =
                    ( addTodoFormMeta, addTodoForm )

                ( newTodoFormWithMeta, cmd ) =
                    case unwrap model of
                        Just ( AddTodoFormMeta _, todoForm ) ->
                            ( ( addTodoFormMeta, todoForm ), Cmd.none )

                        Just ( EditTodoFormMeta todo, todoForm ) ->
                            ( addTodoFormWithMeta
                            , notifyEdited config todo todoForm
                            )

                        Nothing ->
                            ( addTodoFormWithMeta, Cmd.none )
            in
            ( Model <| Just newTodoFormWithMeta, cmd )

        EditClicked todo ->
            let
                cmd : Cmd msg
                cmd =
                    notifyAddedOrEdited config model
            in
            ( Model <|
                Just <|
                    ( EditTodoFormMeta todo, TodoForm.init todo.title todo.dueAt todo.projectId )
            , cmd
            )

        Closed ->
            ( Model Nothing, Cmd.none )


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


notifyAdded : { a | onAdded : TodoForm.Fields -> msg } -> TodoForm.Model -> msg
notifyAdded config todoForm =
    TodoForm.getFields todoForm
        |> config.onAdded


notifyAddedOrEdited :
    { a
        | onAdded : TodoForm.Fields -> Cmd msg
        , onEdited : TodoId -> List Todo.Msg -> msg
    }
    -> Model
    -> Cmd msg
notifyAddedOrEdited config model =
    case unwrap model of
        Just ( meta, todoForm ) ->
            let
                fields =
                    TodoForm.getFields todoForm
            in
            case meta of
                AddTodoFormMeta _ ->
                    notifyAdded config todoForm

                EditTodoFormMeta todo ->
                    notifyEdited config todo todoForm

        Nothing ->
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
