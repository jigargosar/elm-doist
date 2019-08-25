module UI.Button exposing (..)

import Accessibility.Styled.Key as Key
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes
    exposing
        ( class
        , tabindex
        )
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)


btnKDDecoder msg =
    JD.lazy (\_ -> JD.oneOf [ Key.enter msg, Key.space msg ])


preventDefault : msg -> ( msg, Bool )
preventDefault msg =
    Tuple.pair msg True


onKeyDownPreventDefault : msg -> List (msg -> Decoder msg) -> Attribute msg
onKeyDownPreventDefault msg keys =
    let
        decoder =
            JD.lazy
                (\_ ->
                    keys
                        |> List.map ((|>) msg)
                        |> JD.oneOf
                        |> JD.map preventDefault
                )
    in
    preventDefaultOn "keydown" decoder


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    preventDefaultOn "click" (JD.succeed ( msg, True ))


view : msg -> List (Attribute msg) -> List (Html msg) -> Html msg
view msg attrs =
    div
        ([ onClickPreventDefault msg
         , onKeyDownPreventDefault msg [ Key.enter, Key.space ]
         , tabindex 0
         , class "pointer"
         ]
            ++ attrs
        )
