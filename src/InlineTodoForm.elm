module InlineTodoForm exposing (AddAt(..), Model, Msg, add, edit, init, update, view)

import Basics.Extra exposing (flip)
import Html.Styled as H exposing (Html)
import Maybe.Extra as MX
import Project exposing (ProjectList)
import ProjectId exposing (ProjectId)
import Return
import Task
import Todo exposing (Todo)
import TodoForm
import TodoId exposing (TodoId)


type Model
    = Opened OpenState
    | Closed


type alias OpenState =
    ( Meta, TodoForm.Model )


type AddAt
    = Start
    | End


type Meta
    = AddTodoFormMeta AddAt
    | EditTodoFormMeta Todo


closed : Model
closed =
    Closed


opened : OpenState -> Model
opened =
    Opened


init : Model
init =
    closed


mapOpened : (OpenState -> OpenState) -> Model -> Model
mapOpened fn model =
    case model of
        Opened openState ->
            fn openState |> Opened

        Closed ->
            model


getAddTodoForm : Model -> Maybe TodoForm.Model
getAddTodoForm model =
    case model of
        Opened ( AddTodoFormMeta _, todoForm ) ->
            Just todoForm

        _ ->
            Nothing


getTodoForm : Model -> Maybe TodoForm.Model
getTodoForm model =
    case model of
        Opened ( _, todoForm ) ->
            Just todoForm

        _ ->
            Nothing


setTodoForm : TodoForm.Model -> Model -> Model
setTodoForm todoForm =
    mapOpened (Tuple.mapSecond (always todoForm))


type Msg
    = AddClicked AddAt ProjectId
    | EditClicked Todo
    | TodoFormMsg TodoForm.Msg
    | SaveClicked TodoForm.Fields
    | CancelClicked


add : AddAt -> ProjectId -> Msg
add =
    AddClicked


edit : Todo -> Msg
edit =
    EditClicked


update :
    { toMsg : Msg -> msg
    , onAdded : TodoForm.Fields -> Cmd msg
    , onEdited : TodoId -> List Todo.Msg -> msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config message model =
    case message of
        AddClicked addAt projectId ->
            let
                newMeta =
                    AddTodoFormMeta addAt

                newTodoForm =
                    model
                        |> getAddTodoForm
                        |> Maybe.withDefault (TodoForm.fromProjectId projectId)
            in
            ( opened ( newMeta, newTodoForm ), notifyIfEditing config model )

        EditClicked todo ->
            ( opened ( EditTodoFormMeta todo, TodoForm.fromTodo todo )
            , notifyAddedOrEdited config model
            )

        TodoFormMsg msg ->
            getTodoForm model
                |> MX.unwrap ( model, Cmd.none )
                    (todoFormUpdate msg >> Tuple.mapFirst (flip setTodoForm model))
                |> Return.mapCmd config.toMsg

        SaveClicked _ ->
            ( closed, notifyAddedOrEdited config model )

        CancelClicked ->
            ( closed, Cmd.none )


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
        Opened ( EditTodoFormMeta todo, todoForm ) ->
            notifyEdited config todo todoForm

        _ ->
            Cmd.none


notifyAdded : { a | onAdded : TodoForm.Fields -> b } -> TodoForm.Model -> b
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
    case model of
        Opened ( meta, todoForm ) ->
            case meta of
                AddTodoFormMeta _ ->
                    notifyAdded config todoForm

                EditTodoFormMeta todo ->
                    notifyEdited config todo todoForm

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

        Opened ( meta, todoForm ) ->
            let
                todoFormView =
                    TodoForm.view projectList todoForm |> H.map (TodoFormMsg >> toMsg)
            in
            case meta of
                AddTodoFormMeta addAt ->
                    config.add addAt todoFormView

                EditTodoFormMeta todo ->
                    config.edit todo.id todoFormView
