module UI.TextButton exposing (plain, primary, secondary)

import Css exposing (..)
import FunctionalCss as FCss
import Html.Styled exposing (Attribute, Html, text)
import UI.Button as Button


primaryStyle =
    Css.batch [ FCss.underline, FCss.blue ]


secondaryStyle =
    Css.batch [ FCss.underline, FCss.gray ]


styled :
    List Style
    -> msg
    -> String
    -> List (Attribute msg)
    -> Html msg
styled styles action label attrs =
    Button.styled styles
        action
        attrs
        [ text label ]


primary : msg -> String -> List (Attribute msg) -> Html msg
primary =
    styled [ primaryStyle ]


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary =
    styled [ secondaryStyle ]


plain : msg -> String -> List (Attribute msg) -> Html msg
plain =
    styled []
