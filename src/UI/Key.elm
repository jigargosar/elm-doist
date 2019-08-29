module UI.Key exposing (onEnter, onEnterOrSpace, onEscape)

import Accessibility.Styled.Key exposing (enter, escape, space)
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)


unlessDefaultPrevented : msg -> Decoder msg
unlessDefaultPrevented msg =
    JD.field "defaultPrevented" JD.bool
        |> JD.andThen
            (\defaultPrevented ->
                if defaultPrevented then
                    JD.fail "Failing because defaultPrevented"

                else
                    JD.succeed msg
            )


onDown : List (Decoder msg) -> Attribute msg
onDown list =
    preventDefaultOn "keydown"
        (unlessDefaultPrevented list
            |> JD.andThen (JD.oneOf >> JD.map preventDefault)
        )


preventDefault : msg -> ( msg, Bool )
preventDefault msg =
    Tuple.pair msg True


onEnter : msg -> Attribute msg
onEnter msg =
    onDown [ enter msg ]


onEnterOrSpace : msg -> Attribute msg
onEnterOrSpace msg =
    onDown [ space msg, enter msg ]


onEscape : msg -> Attribute msg
onEscape msg =
    onDown [ escape msg ]
