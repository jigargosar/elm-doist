module UI.TextButton exposing (plain, primary, secondary)

import Html.Styled exposing (Attribute, Html)
import UI.Button as B


buttonHelp : B.Role -> msg -> String -> List (Attribute msg) -> Html (B.Config msg)
buttonHelp role msg label attrs =
    let
        config : B.Config msg
        config =
            { role = role, label = label, attrs = attrs }
    in
    B.view config msg


primary : msg -> String -> List (Attribute msg) -> Html msg
primary =
    buttonHelp B.Primary


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary =
    buttonHelp B.Secondary


plain : msg -> String -> List (Attribute msg) -> Html msg
plain =
    buttonHelp B.Plain
