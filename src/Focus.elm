module Focus exposing (onFocusOutsideDomId)

import BasicsExtra exposing (ifElse)
import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (on)
import Json.Decode as JD exposing (Decoder)


onFocusOutsideDomId : String -> msg -> Attribute msg
onFocusOutsideDomId domId msg =
    on "focusout"
        (JD.oneOf
            [ JD.field "relatedTarget" (JD.null True)
            , JD.field "relatedTarget" (isOutsideElIdDecoder domId)
            ]
            |> JD.andThen
                (\isFocusOutside ->
                    ifElse isFocusOutside
                        (JD.succeed msg)
                        (JD.fail "not interested")
                )
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
