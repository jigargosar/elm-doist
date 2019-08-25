module IconButton exposing (Icon, fa, faStyled, view)

import Accessibility.Styled exposing (Attribute)
import Accessibility.Styled.Key as Key
import FontAwesome.Icon as FAI
import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes
    exposing
        ( class
        , tabindex
        )
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)
import Svg
import Svg.Attributes


type Icon msg
    = FA FAI.Icon
    | FAStyled FAI.Icon (List (Svg.Attribute msg))


fa =
    FA


faStyled =
    FAStyled


view : msg -> List (Attribute msg) -> Icon msg -> Html msg
view action attrs icon =
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
        [ case icon of
            FA faIcon ->
                faIcon
                    |> FAI.viewStyled (Svg.Attributes.class "gray" :: [])
                    |> H.fromUnstyled

            FAStyled faIcon svgAttrs ->
                faIcon
                    |> FAI.viewStyled (Svg.Attributes.class "gray" :: svgAttrs)
                    |> H.fromUnstyled
        ]
