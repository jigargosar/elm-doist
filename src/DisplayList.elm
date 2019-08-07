module DisplayList exposing (DisplayList, Msg, changeList, initial, removed, subscriptions, toList, update)

import Browser.Events
import Now exposing (Millis)
import Return
import Set exposing (Set)
import Time


type alias AnimatingRec a =
    { from : List a
    , to : List a
    , elapsed : Float
    }


type DisplayList a
    = Initial (List a)
    | Animating (AnimatingRec a)


initial : List a -> DisplayList a
initial =
    Initial


type Msg
    = EndAnimation Millis
    | OnAnimationFrame Float


subscriptions : DisplayList a -> Sub Msg
subscriptions model =
    case model of
        Initial _ ->
            Sub.none

        Animating _ ->
            Browser.Events.onAnimationFrameDelta OnAnimationFrame


changeList : List a -> DisplayList a -> DisplayList a
changeList newList model =
    case model of
        Initial oldList ->
            Animating { from = oldList, to = newList, elapsed = 0 }

        Animating rec ->
            Animating { rec | to = newList, elapsed = 0 }


type alias Return a =
    Return.Return Msg (DisplayList a)


update : Msg -> DisplayList a -> DisplayList a
update msg model =
    case msg of
        EndAnimation _ ->
            case model of
                Initial _ ->
                    model

                Animating rec ->
                    Initial rec.to

        OnAnimationFrame delta ->
            case model of
                Initial _ ->
                    model

                Animating rec ->
                    Animating { rec | elapsed = delta + rec.elapsed }


toList : DisplayList a -> List a
toList model =
    case model of
        Initial l ->
            l

        Animating rec ->
            rec.from


removed : (a -> comparable) -> DisplayList a -> Set comparable
removed toId model =
    case model of
        Initial _ ->
            Set.empty

        Animating rec ->
            let
                oldIds =
                    rec.from |> List.map toId |> Set.fromList

                newIds =
                    rec.to |> List.map toId |> Set.fromList
            in
            Set.diff oldIds newIds
