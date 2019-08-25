module UI.Button exposing (styled, view)

import Accessibility.Styled.Key as Key
import Css exposing (Style)
import FunctionalCss as FCss
import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (class, css, tabindex)
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)


view : msg -> List (Attribute msg) -> List (Html msg) -> Html msg
view =
    styled []


styled : List Style -> msg -> List (Attribute msg) -> List (Html msg) -> Html msg
styled styles msg attrs =
    H.styled div
        (FCss.pointer :: styles)
        ([ onClickPreventDefault msg
         , onKeyDownPreventDefault msg [ Key.enter, Key.space ]
         , tabindex 0
         ]
            ++ attrs
        )


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
