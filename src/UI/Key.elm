module UI.Key exposing (enter, escape, onDown, onEnter, onEscape, space)

import Accessibility.Styled.Key as Key
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)


preventDefault : msg -> ( msg, Bool )
preventDefault msg =
    Tuple.pair msg True


onDown : List (Decoder msg) -> Attribute msg
onDown list =
    preventDefaultOn "keydown" (JD.lazy (\_ -> JD.oneOf list |> JD.map preventDefault))


escape : msg -> Decoder msg
escape =
    Key.escape


enter : msg -> Decoder msg
enter =
    Key.enter


space : msg -> Decoder msg
space =
    Key.space


onEnter : msg -> Attribute msg
onEnter msg =
    onDown [ enter msg ]


onEscape : msg -> Attribute msg
onEscape msg =
    onDown [ escape msg ]
