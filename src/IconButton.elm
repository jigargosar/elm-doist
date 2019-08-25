module IconButton exposing (..)

import Accessibility.Styled exposing (Attribute)
import Button
import FontAwesome.Icon
import Html.Styled exposing (Html)


iconButton : msg -> FontAwesome.Icon.Icon -> List (Attribute msg) -> Html msg
iconButton action icon attrs =
    Button.button action
        |> Button.withIcon icon
        |> Button.withAttrs attrs
        |> Button.toHtml
