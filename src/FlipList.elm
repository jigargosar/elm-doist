module FlipList exposing (FlipItem, FlipList(..), Msg, empty, init, update)


type alias FlipItem =
    { id : Int
    , text : String
    }


type FlipList
    = FlipList (List FlipItem)


type Msg
    = NoOp


empty : FlipList
empty =
    FlipList []


init : ( FlipList, Cmd Msg )
init =
    ( empty, Cmd.none )


update : Msg -> FlipList -> ( FlipList, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )
