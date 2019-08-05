module AuthState exposing (AuthState(..), UID, initial)


type alias UID =
    String


type AuthState
    = Unknown
    | SignedIn UID
    | NotSignedIn


initial : AuthState
initial =
    Unknown
