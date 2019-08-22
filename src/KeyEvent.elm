module KeyEvent exposing (enterKeyDecoder, onEnter, spaceKeyDecoder)

import BasicsExtra exposing (ifElse)
import Html exposing (Attribute)
import Html.Events
import Json.Decode as JD exposing (Decoder)


onEnter : a -> Attribute a
onEnter tagger =
    Html.Events.on "keydown"
        (JD.field "key" JD.string
            |> JD.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            JD.succeed tagger

                        _ ->
                            JD.fail "Not Interested"
                )
        )


keyDecoder : String -> a -> Decoder a
keyDecoder targetKey tagger =
    JD.at [ "key" ] JD.string
        |> JD.andThen
            (\key ->
                ifElse (key == targetKey)
                    (JD.succeed tagger)
                    (JD.fail "")
            )


enterKeyDecoder : a -> Decoder a
enterKeyDecoder =
    keyDecoder "Enter"


spaceKeyDecoder : a -> Decoder a
spaceKeyDecoder =
    keyDecoder " "
