module IconButton exposing (default)

import Accessibility.Styled exposing (Attribute)
import Accessibility.Styled.Key as Key
import Button
import FontAwesome.Icon as FAI
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes
    exposing
        ( class
        , tabindex
        )
import Html.Styled.Events exposing (preventDefaultOn)
import HtmlStyledExtra exposing (viewMaybe)
import Json.Decode as JD exposing (Decoder)
import Svg
import Svg.Attributes


buttonHelp : msg -> List (Attribute msg) -> FAI.Icon -> List (Svg.Attribute msg) -> Html msg
buttonHelp action attrs icon iconSvgAttrs =
    let
        btnKDDecoder msg =
            JD.lazy (\_ -> JD.oneOf [ Key.enter msg, Key.space msg ])
    in
    div
        ([ preventDefaultOn "click" <| JD.succeed ( action, True )
         , preventDefaultOn "keydown" <| btnKDDecoder ( action, True )
         , tabindex 0
         , class "pointer"
         ]
            ++ attrs
        )
        [ icon
            |> FAI.viewStyled (Svg.Attributes.class "gray" :: iconSvgAttrs)
            |> H.fromUnstyled
        ]


default : msg -> FAI.Icon -> List (Attribute msg) -> Html msg
default action icon attrs =
    Button.button action
        |> Button.withIcon icon
        |> Button.withAttrs attrs
        |> Button.toHtml
