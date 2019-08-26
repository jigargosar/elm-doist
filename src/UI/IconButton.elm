module UI.IconButton exposing (view)

import FontAwesome.Icon as FAI
import Html.Styled exposing (Attribute, Html)
import Svg
import UI.Button as Button
import UI.FAIcon as FAIcon


view : msg -> List (Attribute msg) -> FAI.Icon -> List (Svg.Attribute msg) -> Html msg
view action attrs icon iconAttrs =
    Button.view action attrs [ FAIcon.styled iconAttrs icon ]
