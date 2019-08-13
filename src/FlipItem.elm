module FlipItem exposing (FlipItem, fiDecoder, fiListDecoder)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias FlipItem =
    { id : Int
    , title : String
    , done : Bool
    }


fiDecoder : Decoder FlipItem
fiDecoder =
    JD.succeed FlipItem
        |> JDP.required "id" JD.int
        |> JDP.required "title" JD.string
        |> JDP.required "completed" JD.bool


fiListDecoder : Decoder (List FlipItem)
fiListDecoder =
    JD.list fiDecoder
