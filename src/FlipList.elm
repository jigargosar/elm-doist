module FlipList exposing (FlipItem, FlipList, Msg, empty, init, update, view)

import BasicsExtra exposing (callWith)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as K
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Random
import Random.List
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
        |> JDP.required "id" JD.int
        |> JDP.required "title" JD.string
        |> JDP.required "completed" JD.bool


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
    | OnShuffle
    | GotRandomShuffled (List FlipItem)


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

        OnShuffle ->
            onShuffle model

        GotRandomShuffled fl ->
            pure (FlipList fl)


onShuffle (FlipList fl) =
    ( FlipList fl, Random.List.shuffle fl |> Random.generate GotRandomShuffled )


onHttpError : Http.Error -> FlipList -> Return
onHttpError err =
    let
        _ =
            Debug.log "HTTP Err" err
    in
    pure


onGotFIList : List FlipItem -> FlipList -> Return
onGotFIList fiList _ =
    fiList
        |> List.take 10
        |> FlipList
        |> pure


view : FlipList -> Html Msg
view (FlipList fl) =
    div [ class "measure-wide center vs3" ]
        [ div [ class "pv1 b " ] [ text "FlipListDemo" ]
        , div [ class "flex hs3" ]
            [ button [ onClick OnShuffle ] [ text "Shuffle" ]
            ]
        , viewList fl
        ]


viewList : List FlipItem -> Html msg
viewList fl =
    K.node "div" [ class "vs1" ] (List.map viewItem fl)


viewItem : FlipItem -> ( String, Html msg )
viewItem fi =
    let
        strId =
            fi.id |> String.fromInt
    in
    ( strId
    , div
        [ class "bg-black-80 white ba br-pill lh-copy pv1"
        , class "ph3"
        ]
        [ text fi.title ]
    )
