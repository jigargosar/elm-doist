module UI.TextButton exposing (plain, primary, secondary)

import Html.Styled exposing (Attribute, Html)
import UI.Button as B


buttonHelp role action label attrs =
    B.button action
        |> B.withLabel label
        |> B.withRole role
        |> B.withAttrs attrs
        |> B.toHtml


primary : msg -> String -> List (Attribute msg) -> Html msg
primary =
    buttonHelp B.Primary


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary =
    buttonHelp B.Secondary


plain : msg -> String -> List (Attribute msg) -> Html msg
plain =
    buttonHelp B.Plain
