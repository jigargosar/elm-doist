port module Focus exposing (FocusResult, attempt, autoFocusWithin, autoFocusWithinId, dataAutoFocus, onFocusOutsideDomId, outsideElIdDecoder)

import BasicsExtra exposing (ifElse)
import Browser.Dom as Dom exposing (focus)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as A
import Html.Styled.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Task


onFocusOutsideDomId : String -> msg -> Attribute msg
onFocusOutsideDomId domId tagger =
    on "focusout"
        (JD.field "relatedTarget" <| outsideElIdDecoder domId tagger)


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


type alias FocusResult =
    Result Dom.Error ()


attempt : (FocusResult -> msg) -> String -> Cmd msg
attempt focusedMsg domId =
    focus domId |> Task.attempt focusedMsg



-- JS


port focusSelector : String -> Cmd msg


autoFocusWithin : String -> Cmd msg
autoFocusWithin selector =
    focusSelector (selector ++ " " ++ " [data-autofocus=true]")


autoFocusWithinId : String -> Cmd msg
autoFocusWithinId id =
    autoFocusWithin ("#" ++ id)


dataAutoFocus : Bool -> Attribute msg
dataAutoFocus bool =
    A.attribute "data-autofocus" (boolToAttr bool)


boolToAttr bool =
    if bool then
        "true"

    else
        "false"
