module AuthState exposing (AuthState(..), UID, decoder, initial, view)

import Html exposing (Html, div, text)
import Json.Decode as JD exposing (Decoder)


type alias UID =
    String


type AuthState
    = Unknown
    | SignedIn UID
    | NotSignedIn


initial : AuthState
initial =
    Unknown


view : AuthState -> Html msg
view model =
    div [] [ text (Debug.toString model) ]


decoder : Decoder AuthState
decoder =
    JD.oneOf [ JD.null NotSignedIn, JD.string |> JD.map SignedIn ]
