module UI.TextButton exposing (plain, primary, secondary)

import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes
    exposing
        ( class
        )
import UI.Button as Button


type Variant
    = Primary
    | Secondary
    | Plain


view :
    msg
    -> Variant
    -> String
    -> List (Attribute msg)
    -> Html msg
view action role label attrs =
    let
        variantClasses =
            case role of
                Plain ->
                    ""

                Primary ->
                    "underline blue"

                Secondary ->
                    "underline gray"
    in
    Button.view action
        (class variantClasses :: attrs)
        [ text label ]


primary : msg -> String -> List (Attribute msg) -> Html msg
primary action label attrs =
    view action Primary label attrs


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary action label attrs =
    view action Secondary label attrs


plain : msg -> String -> List (Attribute msg) -> Html msg
plain action label attrs =
    view action Plain label attrs
