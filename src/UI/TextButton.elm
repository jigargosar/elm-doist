module UI.TextButton exposing (plain, primary, secondary)

import Accessibility.Styled.Key as Key
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes
    exposing
        ( class
        , tabindex
        )
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)
import UI.Button as Button


type Role
    = Primary
    | Secondary
    | Plain


view :
    msg
    -> Role
    -> String
    -> List (Attribute msg)
    -> Html msg
view action role label attrs =
    Button.view action
        attrs
        [ div
            [ class <|
                case role of
                    Plain ->
                        ""

                    Primary ->
                        "underline blue"

                    Secondary ->
                        "underline gray"
            ]
            [ text label ]
        ]


primary : msg -> String -> List (Attribute msg) -> Html msg
primary action label attrs =
    view action Primary label attrs


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary action label attrs =
    view action Secondary label attrs


plain : msg -> String -> List (Attribute msg) -> Html msg
plain action label attrs =
    view action Plain label attrs
