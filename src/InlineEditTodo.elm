module InlineEditTodo exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import Todo exposing (DueAt, Todo, TodoList)


type alias ModelRecord =
    { todo : Todo, title : Maybe String, dueAt : Maybe DueAt }


type Model
    = Model ModelRecord


decoder : Decoder Model
decoder =
    JD.succeed ModelRecord
        |> JDP.required "todo" Todo.decoder
        |> JDP.optional "title" (JD.nullable JD.string) Nothing
        |> JDP.optional "dueAt" (JD.nullable Todo.dueAtDecoder) Nothing
        |> JD.map Model


encoder : Model -> Value
encoder (Model { todo, title, dueAt }) =
    JE.object
        (( "todo", Todo.encoder todo )
            :: Maybe.Extra.unwrap []
                (\t -> [ ( "title", JE.string t ) ])
                title
            ++ Maybe.Extra.unwrap []
                (\da -> [ ( "dueAt", Todo.dueAtEncoder da ) ])
                dueAt
        )


maybeEncoder : Maybe Model -> Value
maybeEncoder =
    Maybe.Extra.unwrap JE.null encoder


setDueAt : Maybe DueAt -> Model -> Model
setDueAt dueAt (Model modelRecord) =
    { modelRecord | dueAt = dueAt }
        |> Model


toRecord : Model -> ModelRecord
toRecord (Model modelRecord) =
    modelRecord


fromRecord : ModelRecord -> Model
fromRecord =
    Model
