module InlineEditTodo exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import Todo exposing (DueAt, Todo, TodoList)


type alias Model =
    { todo : Todo, title : Maybe String, dueAt : Maybe DueAt }


decoder : Decoder Model
decoder =
    JD.succeed Model
        |> JDP.required "todo" Todo.decoder
        |> JDP.optional "title" (JD.nullable JD.string) Nothing
        |> JDP.optional "dueAt" (JD.nullable Todo.dueAtDecoder) Nothing


encoder : Model -> Value
encoder { todo, title, dueAt } =
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
