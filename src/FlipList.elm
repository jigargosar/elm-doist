module FlipList exposing (FlipItem, FlipList(..), Msg, empty, init, update)

import Array
import List.Extra


type alias FlipItem =
    { id : Int
    , text : String
    }


fiFromInt i =
    FlipItem i


type FlipList
    = FlipList (List FlipItem)


type Msg
    = NoOp


times : Int -> (Int -> a) -> List a
times =
    List.Extra.initialize


flipItems =
    times 10 fiFromInt


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
