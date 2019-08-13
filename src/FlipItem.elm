module FlipItem exposing (FlipItem, decoder, listDecoder)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias FlipItem =
    { id : Int
    , title : String
    , done : Bool
    }


decoder : Decoder FlipItem
decoder =
    JD.succeed FlipItem
        |> JDP.required "id" JD.int
        |> JDP.required "title" JD.string
        |> JDP.required "completed" JD.bool


listDecoder : Decoder (List FlipItem)
listDecoder =
    JD.list decoder
