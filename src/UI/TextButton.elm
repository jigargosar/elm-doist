module UI.TextButton exposing (plain, primary, secondary)

import Html.Styled exposing (Attribute, Html)
import UI.Button


primary : msg -> String -> List (Attribute msg) -> Html msg
primary action label attrs =
    UI.Button.view action UI.Button.Plain label attrs


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary action label attrs =
    UI.Button.view action UI.Button.Plain label attrs


plain : msg -> String -> List (Attribute msg) -> Html msg
plain action label attrs =
    UI.Button.view action UI.Button.Plain label attrs
