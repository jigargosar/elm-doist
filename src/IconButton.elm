module IconButton exposing (default)

import Accessibility.Styled exposing (Attribute)
import Button
import FontAwesome.Icon
import Html.Styled exposing (Html)


default : msg -> FontAwesome.Icon.Icon -> List (Attribute msg) -> Html msg
default action icon attrs =
    Button.button action
        |> Button.withIcon icon
        |> Button.withAttrs attrs
        |> Button.toHtml
