module EditTodoForm exposing (Model, init, view)

import Html.Styled as H exposing (Attribute, Html, div, text, textarea)
import Html.Styled.Attributes as A exposing (class, rows)
import Html.Styled.Events exposing (onInput)
import Json.Encode as JE exposing (Value)
import Todo exposing (DueAt, Todo, TodoList)
import UI.TextButton as TextButton


type alias Internals =
    { todo : Todo
    , title : String
    }


type Model
    = Model Internals


init : Todo -> Model
init todo =
    Model { todo = todo, title = todo.title }


type alias ViewConfig msg =
    { save : msg
    , cancel : msg
    , delete : msg
    , changed : Model -> msg
    }


view : ViewConfig msg -> Model -> Html msg
view config (Model ({ title } as info)) =
    let
        titleChanged newTitle =
            config.changed (Model { info | title = newTitle })
    in
    div [ class "pa3" ]
        [ div [ class "flex" ]
            [ div [ class "flex-grow-1" ]
                [ H.node "auto-resize-textarea"
                    [ A.property "textAreaValue" (JE.string title) ]
                    [ textarea
                        [ class "pa0 lh-copy overflow-hidden w-100"
                        , rows 1
                        , onInput titleChanged
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
