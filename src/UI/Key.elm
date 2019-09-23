module UI.Key exposing (onEnter, onEnterOrSpace, onEscape)

import Accessibility.Styled.Key exposing (enter, escape, space)
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events as E
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



--onDown : List (Decoder msg) -> Attribute msg
--onDown list =
--    preventDefaultOn "keydown"
--        (unlessDefaultPrevented list
--            |> JD.andThen (JD.oneOf >> JD.map preventDefault)
--        )


onDown : List (Decoder msg) -> Attribute msg
onDown list =
    E.custom "keydown"
        (list
            |> (JD.oneOf >> JD.map stopBoth)
        )



--stopPropagation : msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
--stopPropagation msg =
--    { message = msg, stopPropagation = True, preventDefault = False }


stopBoth : msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
stopBoth msg =
    { message = msg, stopPropagation = True, preventDefault = True }


onEnter : msg -> Attribute msg
onEnter msg =
    onDown [ enter msg ]


onEnterOrSpace : msg -> Attribute msg
onEnterOrSpace msg =
    onDown [ space msg, enter msg ]


onEscape : msg -> Attribute msg
onEscape msg =
    onDown [ escape msg ]
