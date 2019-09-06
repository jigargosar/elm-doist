module TodoForm exposing (..)

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


type TodoForm
    = Edit TodoId TodoFormFields TodoFormFields
    | Add AddAt TodoFormFields


type TodoFormMsg
    = Save
    | Set TodoForm
    | Delete
    | Cancel
    | OpenAdd AddAt ProjectId
    | OpenEdit Todo


type alias TodoFormConfig msg =
    { set : TodoForm -> msg
    , save : msg
    , cancel : msg
    , delete : msg
    , openAdd : AddAt -> ProjectId -> msg
    , openEdit : Todo -> msg
    }



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
        persistEditing : TodoId -> TodoFormFields -> TodoFormFields -> Cmd msg
        persistEditing =
            patchEditingTodoCmd config

        persistNew : TodoFormFields -> Cmd msg
        persistNew =
            persistNewTodoCmd config
    in
    case message of
        OpenAdd addAt projectId ->
            let
                newTodoForm =
                    Add addAt { title = "", dueAt = Todo.notDue, projectId = projectId }
                        |> Just
            in
            case model.maybeTodoForm of
                Nothing ->
                    ( { model | maybeTodoForm = newTodoForm }, Cmd.none )

                Just (Add _ fields) ->
                    ( { model | maybeTodoForm = Add addAt fields |> Just }, Cmd.none )

                Just (Edit editingTodoId initialFields currentFields) ->
                    ( { model | maybeTodoForm = newTodoForm }
                    , persistEditing editingTodoId initialFields currentFields
                    )

        OpenEdit todo ->
            let
                newTodoForm =
                    Edit todo.id (initTodoFormFields todo) (initTodoFormFields todo)
                        |> Just
            in
            case model.maybeTodoForm of
                Nothing ->
                    ( { model | maybeTodoForm = newTodoForm }
                    , Cmd.none
                    )

                Just (Add _ fields) ->
                    ( { model | maybeTodoForm = newTodoForm }
                    , persistNew fields
                    )

                Just (Edit editingTodoId initialFields fields) ->
                    if editingTodoId == todo.id then
                        ( model, Cmd.none )

                    else
                        ( { model | maybeTodoForm = newTodoForm }
                        , persistEditing editingTodoId initialFields fields
                        )

        Set form ->
            ( { model | maybeTodoForm = Just form }, Cmd.none )

        Save ->
            let
                newModel =
                    { model | maybeTodoForm = Nothing }
            in
            case model.maybeTodoForm of
                Nothing ->
                    ( newModel, Cmd.none )

                Just (Edit editingTodoId initialFields currentFields) ->
                    ( newModel
                    , persistEditing editingTodoId initialFields currentFields
                    )

                Just (Add _ fields) ->
                    ( newModel, persistNew fields )

        Delete ->
            case model.maybeTodoForm of
                Nothing ->
                    ( model, Cmd.none )

                Just (Edit editingTodoId _ _) ->
                    ( model, Fire.deleteTodo editingTodoId )

                Just (Add _ _) ->
                    ( model, Cmd.none )

        Cancel ->
            ( { model | maybeTodoForm = Nothing }, Cmd.none )


persistNewTodoCmd config fields =
    if SX.isBlank fields.title then
        Cmd.none

    else
        config.addNewTodoCmd fields


patchEditingTodoCmd config editingTodoId initialFields currentFields =
    let
        msgList : List Todo.Msg
        msgList =
            [ if initialFields.title /= currentFields.title then
                Just <| Todo.SetTitle currentFields.title

              else
                Nothing
            , if initialFields.dueAt /= currentFields.dueAt then
                Just <| Todo.SetDueAt currentFields.dueAt

              else
                Nothing
            , if initialFields.projectId /= currentFields.projectId then
                Just <| Todo.SetProjectId currentFields.projectId

              else
                Nothing
            ]
                |> List.filterMap identity
    in
    if List.isEmpty msgList then
        Cmd.none

    else
        config.patchTodoCmd editingTodoId msgList


viewTodoItemForm : TodoFormConfig msg -> (String -> msg) -> TodoFormFields -> Html msg
viewTodoItemForm config titleChangedMsg fields =
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
