module IconButton exposing (..)

import Accessibility.Styled exposing (Attribute)
import Button
import FontAwesome.Icon
import Html.Styled exposing (Html)


withAttrs : msg -> FontAwesome.Icon.Icon -> List (Attribute msg) -> Html msg
withAttrs action icon attrs =
    Button.button action
        |> Button.withIcon icon
        |> Button.withAttrs attrs
        |> Button.toHtml


plain : msg -> FontAwesome.Icon.Icon -> Html msg
plain msg icon =
    withAttrs msg icon []
