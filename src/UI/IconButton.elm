module UI.IconButton exposing (Icon, fa, faStyled, view)

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
import UI.Button as Button


type Icon msg
    = FA FAI.Icon
    | FAStyled FAI.Icon (List (Svg.Attribute msg))


fa =
    FA


faStyled =
    FAStyled


view : msg -> List (Attribute msg) -> Icon msg -> Html msg
view action attrs icon =
    Button.view action
        (tabindex 0 :: class "pointer" :: attrs)
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
