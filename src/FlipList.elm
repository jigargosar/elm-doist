module FlipList exposing (FlipList, Msg, empty, init, update, view)

import BasicsExtra exposing (callWith)
import Browser.Dom as Dom exposing (Element)
import FlipItem exposing (FlipItem)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes as A exposing (class)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as K
import Http
import Random
import Random.List
import Result exposing (Result)
import Result.Extra
import Task exposing (Task)
import UpdateExtra exposing (pure)


type alias FlippingModel =
    { from : List FlipItem
    , to : List FlipItem
    }


type FlipList
    = Stable (List FlipItem)
    | Flipping FlippingModel


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotFlipItems (HttpResult (List FlipItem))
    | OnShuffle
    | GotRandomShuffled (List FlipItem)
    | OnGotFlipDomInfo (Result Dom.Error FlipDomInfo)


empty : FlipList
empty =
    Stable []


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
            onGotShuffled fl model

        OnGotFlipDomInfo res ->
            res
                |> Result.Extra.unpack onDomError onGotFlipDomInfo
                |> callWith model


type alias ElInfo =
    ( FlipItem.Id, Element )


type alias FlipDomInfo =
    { from : List ElInfo, to : List ElInfo }


getEl : String -> FlipItem -> Task Dom.Error ElInfo
getEl idPrefix fi =
    let
        domId =
            idPrefix ++ "-" ++ FlipItem.strId fi
    in
    Dom.getElement domId
        |> Task.map (\el -> ( fi.id, el ))


onGotShuffled shuffled model =
    case model of
        Stable fl ->
            let
                from =
                    fl

                to =
                    shuffled

                fromTasks =
                    from |> List.map (getEl "from") |> Task.sequence

                toTasks =
                    to |> List.map (getEl "to") |> Task.sequence

                sequencedTask =
                    Task.map2 FlipDomInfo fromTasks toTasks
            in
            ( Flipping <| { from = from, to = to }
            , sequencedTask |> Task.attempt OnGotFlipDomInfo
            )

        Flipping _ ->
            pure model


onGotFlipDomInfo el model =
    let
        _ =
            Debug.log "el" el
    in
    pure model


onShuffle : FlipList -> Return
onShuffle model =
    case model of
        Stable fl ->
            ( model, Random.List.shuffle fl |> Random.generate GotRandomShuffled )

        Flipping _ ->
            pure model


onHttpError : Http.Error -> FlipList -> Return
onHttpError err =
    let
        _ =
            Debug.log "HTTP Err" err
    in
    pure


onDomError : Dom.Error -> FlipList -> Return
onDomError err =
    let
        _ =
            Debug.log "Dom Err" err
    in
    pure


onGotFIList : List FlipItem -> FlipList -> Return
onGotFIList fiList _ =
    fiList
        |> List.take 10
        |> Stable
        |> pure


view : FlipList -> Html Msg
view model =
    case model of
        Stable fl ->
            div [ class "measure-wide center vs3" ]
                [ div [ class "pv1 b " ] [ text "FlipListDemo" ]
                , div [ class "flex hs3" ]
                    [ button [ onClick OnShuffle ] [ text "Shuffle" ]
                    ]
                , viewList "" fl
                ]

        Flipping rec ->
            div [ class "measure-wide center vs3" ]
                [ div [ class "pv1 b " ] [ text "FlipListDemo" ]
                , div [ class "flex hs3" ]
                    [ button [ onClick OnShuffle ] [ text "Shuffle" ]
                    ]
                , div [ class "relative" ]
                    [ K.node "div"
                        [ class "absolute vs1" ]
                        (List.map (viewItem "to-") rec.to)
                    , K.node "div"
                        [ class "o-50 absolute vs1" ]
                        (List.map (viewItem "from-") rec.from)
                    ]
                ]


viewList : String -> List FlipItem -> Html msg
viewList idPrefix fl =
    K.node "div" [ class "vs1" ] (List.map (viewItem idPrefix) fl)


viewItem : String -> FlipItem -> ( String, Html msg )
viewItem idPrefix fi =
    let
        domId =
            idPrefix ++ FlipItem.strId fi

        strId =
            FlipItem.strId fi
    in
    ( strId
    , div
        [ class "bg-black-80 white ba br-pill lh-copy pv1"
        , class "ph3"
        , A.id domId
        ]
        [ text <| strId ++ ": " ++ fi.title ]
    )
