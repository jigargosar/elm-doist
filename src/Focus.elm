module Focus exposing (onFocusOutsideDomId)

import BasicsExtra exposing (ifElse)
import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (on)
import Json.Decode as JD exposing (Decoder)


onFocusOutsideDomId : String -> msg -> Attribute msg
onFocusOutsideDomId domId tagger =
    on "focusout"
        (JD.field "relatedTarget"
            (JD.oneOf
                [ JD.null tagger
                , outsideElIdDecoder domId tagger
                ]
            )
        )


outsideElIdDecoder : String -> msg -> Decoder msg
outsideElIdDecoder domId msg =
    isOutsideElIdDecoder domId
        |> JD.andThen
            (\isOut ->
                ifElse isOut
                    (JD.succeed msg)
                    (JD.fail "")
            )


isOutsideElIdDecoder : String -> Decoder Bool
isOutsideElIdDecoder containerDomId =
    JD.oneOf
        [ JD.field "id" JD.string
            |> JD.andThen
                (\id ->
                    ifElse (containerDomId == id)
                        -- found match by id
                        (JD.succeed False)
                        -- try next decoder
                        (JD.fail "continue")
                )
        , JD.lazy (\_ -> isOutsideElIdDecoder containerDomId |> JD.field "parentNode")

        -- fallback when all previous decoders failed
        , JD.succeed True
        ]
