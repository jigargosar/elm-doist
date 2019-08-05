module AuthState exposing (AuthState(..), UID)


type alias UID =
    String


type AuthState
    = Unknown
    | SignedIn UID
    | NotSignedIn
