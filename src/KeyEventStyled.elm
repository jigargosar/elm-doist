module KeyEventStyled exposing (enterKeyDecoder, escKeyDecoder, spaceKeyDecoder)

import BasicsExtra exposing (ifElse)
import Html.Styled exposing (Attribute)
import Html.Styled.Events
import Json.Decode as JD exposing (Decoder)


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


escKeyDecoder : a -> Decoder a
escKeyDecoder =
    keyDecoder "Escape"
