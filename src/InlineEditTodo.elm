module InlineEditTodo exposing (Model, decoder, dueAtOrDefault, fromTodo, idEq, maybeEncoder, setDueAt, setTitle, titleOrDefault, toUpdateMessages, todoId)

import BasicsExtra exposing (ifElse)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Maybe as M
import Maybe.Extra as MX
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)


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


todoId : Model -> TodoId
todoId (Model { todo }) =
    todo.id


titleOrDefault : Model -> String
titleOrDefault (Model { todo, title }) =
    title
        |> Maybe.withDefault todo.title


dueAtOrDefault : Model -> DueAt
dueAtOrDefault (Model { todo, dueAt }) =
    dueAt |> Maybe.withDefault todo.dueAt
