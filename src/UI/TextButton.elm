module UI.TextButton exposing (primary, primaryStyle, secondary, secondaryStyle, styled, styled_, view, view_)

import Css exposing (..)
import FunctionalCss as FCss
import Html.Styled exposing (Attribute, Html, text)
import UI.Button as Button


primary : msg -> String -> List (Attribute msg) -> Html msg
primary =
    styled_ [ primaryStyle ]


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary =
    styled_ [ secondaryStyle ]


view_ : msg -> String -> List (Attribute msg) -> Html msg
view_ =
    styled_ []


view : List (Attribute msg) -> msg -> String -> Html msg
view =
    styled []


primaryStyle =
    Css.batch [ FCss.underline, FCss.blue ]


secondaryStyle =
    Css.batch [ FCss.underline, FCss.gray ]


styled_ :
    List Style
    -> msg
    -> String
    -> List (Attribute msg)
    -> Html msg
styled_ styles action label attrs =
    Button.styled styles
        action
        attrs
        [ text label ]


styled :
    List Style
    -> List (Attribute msg)
    -> msg
    -> String
    -> Html msg
styled styles attrs action label =
    Button.styled styles
        action
        attrs
        [ text label ]
