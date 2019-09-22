module InlineTodoForm exposing
    ( AddAt(..)
    , Config
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
    = Opened Internal
    | Closed


type Meta
    = Add AddAt
    | Edit Todo


type alias Internal =
    { meta : Meta
    , form : TodoForm.Model
    }


type AddAt
    = Start
    | End


init : Model
init =
    Closed


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


type alias Config msg =
    { toMsg : Msg -> msg
    , onAdded : TodoForm.Fields -> msg
    , onEdited : TodoId -> List Todo.Msg -> msg
    }


update :
    Config msg
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config message model =
    case message of
        AddClicked addAt projectId ->
            case model of
                Opened { meta, form } ->
                    case meta of
                        Add _ ->
                            ( Opened { meta = Add addAt, form = form }
                            , focusForm config
                            )

                        Edit todo ->
                            ( Opened { meta = Add addAt, form = TodoForm.fromProjectId projectId }
                            , Cmd.batch
                                [ notifyEdited config todo form
                                , focusForm config
                                ]
                            )

                Closed ->
                    ( Opened { meta = Add addAt, form = TodoForm.fromProjectId projectId }
                    , focusForm config
                    )

        EditClicked todo ->
            ( Opened { meta = Edit todo, form = TodoForm.fromTodo todo }
            , Cmd.batch
                [ focusForm config
                , notifyAddedOrEdited config model
                ]
            )

        TodoFormMsg msg ->
            case model of
                Opened openedModel ->
                    let
                        ( newTodoForm, cmd ) =
                            todoFormUpdate msg openedModel.form
                    in
                    ( Opened { openedModel | form = newTodoForm }, Cmd.map config.toMsg cmd )

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
        Opened { meta, form } ->
            case meta of
                Add _ ->
                    notifyAdded config form

                Edit todo ->
                    notifyEdited config todo form

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

        Opened { meta, form } ->
            let
                todoFormView =
                    viewForm toMsg projectList form
            in
            case meta of
                Add addAt ->
                    config.add addAt todoFormView

                Edit todo ->
                    config.edit todo.id todoFormView


viewForm : (Msg -> msg) -> ProjectList -> TodoForm.Model -> Html msg
viewForm toMsg projectList form =
    TodoForm.view projectList form |> H.map (TodoFormMsg >> toMsg)
