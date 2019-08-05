module AuthState exposing (AuthState(..), UID, initial, view)

import Html exposing (Html, div, text)


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
