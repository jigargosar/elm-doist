module AddTodoForm exposing (Model, getValid, initAtEnd, initAtStart, updateFields)

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


type AddAt
    = Start
    | End


type alias AddTodoFormInfo =
    { addAt : AddAt
    , title : String
    , dueAt : Todo.DueAt
    , projectId : ProjectId
    }


type Model
    = Model AddTodoFormInfo


init : AddAt -> ProjectId -> Model
init addAt projectId =
    Model { addAt = addAt, title = "", dueAt = Todo.notDue, projectId = projectId }


initAtStart : ProjectId -> Model
initAtStart =
    init Start


initAtEnd : ProjectId -> Model
initAtEnd =
    init End


isValid : Model -> Bool
isValid (Model { title }) =
    SX.isBlank title |> not


getValid : Model -> Maybe { a | title : String, dueAt : Todo.DueAt, projectId : ProjectId }
getValid (Model info) =
    if SX.isBlank info.title then
        Nothing

    else
        Just info


updateFields :
    { a | title : String, dueAt : Todo.DueAt, projectId : ProjectId }
    -> Model
    -> Model
updateFields { title, dueAt, projectId } (Model info) =
    Model { info | title = title, dueAt = dueAt, projectId = projectId }


type alias ViewConfig msg =
    { save : msg
    , cancel : msg
    , changed : Model -> msg
    }


view : ViewConfig msg -> Model -> Html msg
view config (Model info) =
    let
        fields =
            info

        titleChangedMsg newTitle =
            config.changed (Model { info | title = newTitle })
    in
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
            ]
        ]
