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
    = Model Internal


type alias Internal =
    Maybe OpenState


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
    Model Nothing


opened : OpenState -> Model
opened =
    Just >> Model


init : Model
init =
    closed


map : (Maybe OpenState -> Maybe OpenState) -> Model -> Model
map fn =
    unwrap >> fn >> Model


mapOpened : (OpenState -> OpenState) -> Model -> Model
mapOpened fn =
    map (Maybe.map fn)


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


unwrap : Model -> Maybe OpenState
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
update config message model =
    case message of
        AddClicked addAt projectId ->
            let
                addTodoFormMeta =
                    AddTodoFormMeta addAt

                addTodoFormWithMeta =
                    ( addTodoFormMeta, TodoForm.fromProjectId projectId )

                ( newTodoFormWithMeta, cmd ) =
                    case unwrap model of
                        Just ( meta, todoForm ) ->
                            case meta of
                                AddTodoFormMeta _ ->
                                    ( ( addTodoFormMeta, todoForm ), Cmd.none )

                                EditTodoFormMeta todo ->
                                    ( addTodoFormWithMeta
                                    , notifyEdited config todo todoForm
                                    )

                        Nothing ->
                            ( addTodoFormWithMeta, Cmd.none )
            in
            ( opened newTodoFormWithMeta, cmd )

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


getTodoForm : Model -> Maybe TodoForm.Model
getTodoForm =
    unwrap >> Maybe.map Tuple.second


setTodoForm : TodoForm.Model -> Model -> Model
setTodoForm todoForm =
    mapOpened (Tuple.mapSecond (always todoForm))


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
    case unwrap model of
        Just ( meta, todoForm ) ->
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
    case unwrap model of
        Nothing ->
            config.closed ()

        Just ( AddTodoFormMeta addAt, todoForm ) ->
            config.add addAt (TodoForm.view projectList todoForm |> H.map (TodoFormMsg >> toMsg))

        Just ( EditTodoFormMeta todo, todoForm ) ->
            config.edit todo.id (TodoForm.view projectList todoForm |> H.map (TodoFormMsg >> toMsg))
