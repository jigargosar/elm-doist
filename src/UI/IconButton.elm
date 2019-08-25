module UI.IconButton exposing (Icon, fa, view)

import Accessibility.Styled exposing (Attribute)
import FontAwesome.Icon as FAI
import Html.Styled as H exposing (Attribute, Html)
import Svg
import Svg.Attributes as SA
import UI.Button as Button


type Icon msg
    = FA FAI.Icon


fa =
    FA


view : msg -> List (Attribute msg) -> Icon msg -> Html msg
view action attrs icon =
    Button.view action
        attrs
        [ case icon of
            FA faIcon ->
                faIcon
                    |> FAI.viewStyled (SA.class "gray" :: [])
                    |> H.fromUnstyled
        ]
