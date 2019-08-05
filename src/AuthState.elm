module AuthState exposing (AuthState(..), UID, decoder, initial, view)

import Html exposing (Html, div, text)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias UID =
    String


type alias User =
    { displayName : String
    , email : String
    , uid : UID
    }


userDecoder : Decoder User
userDecoder =
    JD.succeed User
        |> JDP.optional "displayName" JD.string ""
        |> JDP.optional "email" JD.string ""
        |> JDP.required "uid" JD.string


type AuthState
    = Unknown
    | UnknownCached User
    | SignedIn User
    | NotSignedIn


initial : AuthState
initial =
    Unknown


view : AuthState -> Html msg
view model =
    div [] [ text (Debug.toString model) ]


decoder : Decoder AuthState
decoder =
    JD.oneOf [ JD.null NotSignedIn, userDecoder |> JD.map SignedIn ]
