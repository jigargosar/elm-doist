module UI.FAIcon exposing (..)

import FontAwesome.Icon as FAI
import Html.Styled as H
import Svg.Attributes as SA


view faIcon =
    faIcon
        |> FAI.viewStyled (SA.class "gray" :: [])
        |> H.fromUnstyled


viewStyled faIcon svgAttrs =
    faIcon
        |> FAI.viewStyled (SA.class "gray" :: svgAttrs)
        |> H.fromUnstyled
