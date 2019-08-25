module UI.IconButton exposing (Icon, fa, faStyled, view)

import Accessibility.Styled exposing (Attribute)
import FontAwesome.Icon as FAI
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes
    exposing
        ( class
        , tabindex
        )
import Svg
import Svg.Attributes as SA
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
                    |> FAI.viewStyled (SA.class "gray" :: [])
                    |> H.fromUnstyled

            FAStyled faIcon svgAttrs ->
                faIcon
                    |> FAI.viewStyled (SA.class "gray" :: svgAttrs)
                    |> H.fromUnstyled
        ]
