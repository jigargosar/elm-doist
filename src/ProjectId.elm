module ProjectId exposing (ProjectId, decoder, default, encoder, fromString, toString)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type ProjectId
    = ProjectId String


unwrap (ProjectId id) =
    id


decoder : Decoder ProjectId
decoder =
    JD.string |> JD.map ProjectId


encoder : ProjectId -> Value
encoder =
    unwrap >> JE.string


default : ProjectId
default =
    ProjectId ""


fromString : String -> Maybe ProjectId
fromString pidStr =
    Just <| ProjectId pidStr


toString : ProjectId -> String
toString =
    unwrap
