module UI.Key exposing (down, enterOrSpace, onDown, onEnter, onEnterOrSpace, onEscape, up)

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


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string


succeedForKey : String -> msg -> Decoder msg
succeedForKey key msg =
    JD.andThen (forKey key msg) keyDecoder


forKey : String -> msg -> String -> Decoder msg
forKey key msg keyCode =
    if keyCode == key then
        JD.succeed msg

    else
        JD.fail keyCode


up : msg -> Decoder msg
up =
    succeedForKey "ArrowUp"


down : msg -> Decoder msg
down =
    succeedForKey "ArrowDown"


enterOrSpace : a -> Decoder a
enterOrSpace msg =
    JD.oneOf [ enter msg, space msg ]
