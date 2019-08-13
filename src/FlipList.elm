module FlipList exposing (FlipItem, FlipList, Msg, empty, init, update, view)

import BasicsExtra exposing (callWith)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Keyed as K
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Result exposing (Result)
import Result.Extra
import UpdateExtra exposing (pure)


type alias FlipItem =
    { id : Int
    , title : String
    , done : Bool
    }


fiDecoder : Decoder FlipItem
fiDecoder =
    JD.succeed FlipItem
        |> JDP.required "Id" JD.int
        |> JDP.required "Title" JD.string
        |> JDP.required "Completed" JD.bool


fiListDecoder : Decoder (List FlipItem)
fiListDecoder =
    JD.list fiDecoder


type FlipList
    = FlipList (List FlipItem)


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotTodos (HttpResult (List FlipItem))


empty : FlipList
empty =
    FlipList []


init : ( FlipList, Cmd Msg )
init =
    ( empty
    , Http.get
        { url = "http://jsonplaceholder.typicode.com/todos"
        , expect = Http.expectJson GotTodos fiListDecoder
        }
    )


type alias Return =
    ( FlipList, Cmd Msg )


update : Msg -> FlipList -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotTodos res ->
            res
                |> Result.Extra.unpack onHttpError onGotFIList
                |> callWith model


onHttpError : Http.Error -> FlipList -> Return
onHttpError _ =
    pure


onGotFIList : List FlipItem -> FlipList -> Return
onGotFIList fiList _ =
    FlipList fiList
        |> pure


view : FlipList -> Html Msg
view (FlipList fl) =
    div [ class "measure-wide center" ]
        [ div [] [ text "FlipListDemo" ]
        , viewList fl
        ]


viewList : List FlipItem -> Html msg
viewList fl =
    let
        viewItem : FlipItem -> ( String, Html msg )
        viewItem fi =
            ( fi.id |> String.fromInt, div [] [ text fi.title ] )
    in
    K.node "div" [] (List.map viewItem fl)
