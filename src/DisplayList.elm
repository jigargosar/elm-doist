module DisplayList exposing (DisplayList, Msg, changeList, initial, subscriptions, update)

import Now exposing (Millis)
import Return
import Time


type alias AnimatingRec a =
    { from : List a
    , to : List a
    , new : Maybe (List a)
    }


type DisplayList a
    = Initial (List a)
    | Animating (AnimatingRec a)


initial : List a -> DisplayList a
initial =
    Initial


type Msg
    = EndAnimation Millis


subscriptions : DisplayList a -> Sub Msg
subscriptions model =
    case model of
        Initial _ ->
            Sub.none

        Animating _ ->
            Time.every 3000 (Time.posixToMillis >> EndAnimation)


changeList : List a -> DisplayList a -> DisplayList a
changeList newList model =
    case model of
        Initial oldList ->
            Animating { from = oldList, to = newList, new = Nothing }

        Animating rec ->
            Animating { rec | new = Just newList }


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
                    case rec.new of
                        Just new ->
                            Animating { from = rec.to, to = new, new = Nothing }

                        Nothing ->
                            Initial rec.to
