module InlineEditTodo exposing
    ( Model
    , decoder
    , dueAtOrDefault
    , fromTodo
    , getTodoId
    , idEq
    , maybeEncoder
    , setDueAt
    , setTitle
    , titleOrDefault
    , toUpdateMessages
    )

import Accessibility.Styled.Key as Key
import BasicsExtra exposing (ifElse)
import Css exposing (minWidth, none, px, resize)
import Html.Styled as H exposing (Attribute, Html, div, textarea)
import Html.Styled.Attributes as A
    exposing
        ( class
        , css
        , rows
        , tabindex
        , value
        )
import Html.Styled.Events exposing (onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Maybe as M exposing (Maybe)
import Maybe.Extra as MX
import Millis exposing (Millis)
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UI.Key
import UI.TextButton as TextButton


type alias ModelRecord =
    { todo : Todo, title : Maybe String, dueAt : Maybe DueAt }


type Model
    = Model ModelRecord


encoder : Model -> Value
encoder (Model { todo, title, dueAt }) =
    JE.object
        (( "todo", Todo.encoder todo )
            :: MX.unwrap []
                (\t -> [ ( "title", JE.string t ) ])
                title
            ++ MX.unwrap []
                (\da -> [ ( "dueAt", Todo.dueAtEncoder da ) ])
                dueAt
        )


maybeEncoder : Maybe Model -> Value
maybeEncoder =
    MX.unwrap JE.null encoder


decoder : Decoder Model
decoder =
    JD.succeed ModelRecord
        |> JDP.required "todo" Todo.decoder
        |> JDP.optional "title" (JD.nullable JD.string) Nothing
        |> JDP.optional "dueAt" (JD.nullable Todo.dueAtDecoder) Nothing
        |> JD.map Model


fromRecord : ModelRecord -> Model
fromRecord =
    Model


fromTodo : Todo -> Model
fromTodo todo =
    { todo = todo, title = Nothing, dueAt = Nothing } |> fromRecord


setDueAt : DueAt -> Model -> Model
setDueAt dueAt (Model modelRecord) =
    { modelRecord | dueAt = Just dueAt }
        |> Model


setTitle : String -> Model -> Model
setTitle title (Model modelRecord) =
    { modelRecord | title = Just title }
        |> Model


idEq : TodoId -> Model -> Bool
idEq todoId_ (Model modelRecord) =
    modelRecord.todo.id == todoId_


toUpdateMessages : Model -> Maybe ( Todo, List Todo.Msg )
toUpdateMessages (Model { todo, dueAt, title }) =
    [ dueAt |> M.map Todo.SetDueAt, title |> M.map Todo.SetTitle ]
        |> List.filterMap identity
        |> (\l -> ifElse (List.isEmpty l) Nothing (Just ( todo, l )))


getTodoId : Model -> TodoId
getTodoId (Model { todo }) =
    todo.id


titleOrDefault : Model -> String
titleOrDefault (Model { todo, title }) =
    title
        |> Maybe.withDefault todo.title


dueAtOrDefault : Model -> DueAt
dueAtOrDefault (Model { todo, dueAt }) =
    dueAt |> Maybe.withDefault todo.dueAt


inlineEditTodoTitleDomId todoId =
    TodoId.toString todoId ++ "inline-edit-todo-title-dom-id"


view :
    { onEditDueStart : TodoId -> msg
    , onSetTitle : TodoId -> String -> msg
    , cancelMsg : msg
    , saveMsg : msg
    }
    -> Time.Zone
    -> Model
    -> Html msg
view { onEditDueStart, onSetTitle, cancelMsg, saveMsg } here model =
    let
        titleValue =
            titleOrDefault model

        dueAtValue =
            dueAtOrDefault model
                |> Todo.dueAtToMillis

        todoId =
            getTodoId model

        viewIP =
            H.node "auto-resize-textarea"
                [ class "flex-grow-1 flex ba b--moon-gray" ]
                [ textarea
                    [ A.id <| inlineEditTodoTitleDomId todoId
                    , class "pa1 flex-grow-1 lh-copy bn"
                    , value titleValue
                    , onInput (onSetTitle todoId)
                    , rows 1
                    , css [ resize none ]
                    , class "overflow-hidden"
                    ]
                    []
                ]

        ( txt, cls ) =
            dueAtValue
                |> MX.unpack
                    (\_ -> ( "Schedule", "gray" ))
                    (\mi ->
                        ( Millis.formatDate "MMM dd" here <| mi, "near-black" )
                    )

        viewDue =
            TextButton.secondary (onEditDueStart <| todoId)
                txt
                [ class "pa1 ba b--moon-gray"
                , class cls
                , css [ minWidth <| px 100 ]
                ]
    in
    div
        [ class "pv3 ph2"
        , tabindex 0
        , UI.Key.onKeyDownPreventDefault cancelMsg [ Key.escape ]
        ]
        [ div [ class "flex" ]
            [ viewIP
            , viewDue
            ]
        , div [ class "flex hs3 lh-copy" ]
            [ TextButton.primary saveMsg "Save" [ class "pa2" ]
            , TextButton.secondary cancelMsg "Cancel" [ class "pa2" ]
            ]
        ]
