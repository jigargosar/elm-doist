module FlipList exposing (FlipList, Msg, empty, init, update, view)

import BasicsExtra exposing (callWith)
import FlipItem exposing (FlipItem)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as K
import Http
import Random
import Random.List
import Result exposing (Result)
import Result.Extra
import UpdateExtra exposing (pure)


type FlipList
    = FlipList (List FlipItem)


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotFlipItems (HttpResult (List FlipItem))
    | OnShuffle
    | GotRandomShuffled (List FlipItem)


empty : FlipList
empty =
    FlipList []


init : ( FlipList, Cmd Msg )
init =
    ( empty
    , FlipItem.fetch GotFlipItems
    )


type alias Return =
    ( FlipList, Cmd Msg )


update : Msg -> FlipList -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotFlipItems res ->
            res
                |> Result.Extra.unpack onHttpError onGotFIList
                |> callWith model

        OnShuffle ->
            onShuffle model

        GotRandomShuffled fl ->
            pure (FlipList fl)


onShuffle model =
    let
        (FlipList fl) =
            model
    in
    ( model, Random.List.shuffle fl |> Random.generate GotRandomShuffled )


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
        [ text <| strId ++ ": " ++ fi.title ]
    )
