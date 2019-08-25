module UI.TextButton exposing (plain, primary, secondary)

import Css exposing (..)
import FunctionalCss as FCss
import Html.Styled exposing (Attribute, Html, text)
import Html.Styled.Attributes exposing (class, css)
import UI.Button as Button


type Variant
    = Primary
    | Secondary
    | Plain


primaryStyle =
    Css.batch [ FCss.underline, FCss.blue ]


secondaryStyle =
    Css.batch [ textDecoration3 underline solid (hex "#777") ]


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
                    []

                Primary ->
                    [ primaryStyle ]

                Secondary ->
                    [ secondaryStyle ]
    in
    Button.view action
        (css variantClasses :: attrs)
        [ text label ]


primary : msg -> String -> List (Attribute msg) -> Html msg
primary action =
    view action Primary


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary action =
    view action Secondary


plain : msg -> String -> List (Attribute msg) -> Html msg
plain action =
    view action Plain
