module TextButton exposing (plain, primary, secondary)

import Button
import Html.Styled exposing (Attribute, Html)


buttonHelp role action label attrs =
    Button.button action
        |> Button.withLabel label
        |> Button.withRole role
        |> Button.withAttrs attrs
        |> Button.toHtml


primary : msg -> String -> List (Attribute msg) -> Html msg
primary =
    buttonHelp Button.Primary


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary =
    buttonHelp Button.Secondary


plain : msg -> String -> List (Attribute msg) -> Html msg
plain =
    buttonHelp Button.Plain
