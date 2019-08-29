module UI.Button exposing (styled, view)

import Css exposing (Style)
import FunctionalCss as FCss
import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (tabindex)
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)
import UI.Key as Key


view : msg -> List (Attribute msg) -> List (Html msg) -> Html msg
view =
    styled []


styled : List Style -> msg -> List (Attribute msg) -> List (Html msg) -> Html msg
styled styles msg attrs =
    H.styled div
        (FCss.pointer :: styles)
        ([ preventDefaultOnClick msg
         , Key.onDown [ Key.enter msg, Key.space msg ]
         , tabindex 0
         ]
            ++ attrs
        )


preventDefaultOnClick : msg -> Attribute msg
preventDefaultOnClick msg =
    preventDefaultOn "click" (JD.succeed ( msg, True ))
