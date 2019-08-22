module TodoId exposing (TodoId, decoder, encoder, new, toString)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type TodoId
    = TodoId String


decoder : Decoder TodoId
decoder =
    JD.string |> JD.map TodoId


encoder : TodoId -> Value
encoder (TodoId id) =
    JE.string id


new : TodoId
new =
    TodoId ""


toString : TodoId -> String
toString (TodoId id) =
    id
