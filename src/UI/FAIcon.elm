module UI.FAIcon exposing (..)

import FontAwesome.Icon as FAI
import Html.Styled as H
import Svg
import Svg.Attributes as SA


view : FAI.Icon -> H.Html msg
view faIcon =
    faIcon
        |> FAI.viewStyled (SA.class "gray" :: [])
        |> H.fromUnstyled


viewStyled : List (Svg.Attribute msg) -> FAI.Icon -> H.Html msg
viewStyled svgAttrs faIcon =
    faIcon
        |> FAI.viewStyled (SA.class "gray" :: svgAttrs)
        |> H.fromUnstyled
