module UI.TextButton exposing (primary, primaryStyle, secondary, secondaryStyle, styled, styled2, view, view2)

import Css exposing (..)
import FunctionalCss as FCss
import Html.Styled exposing (Attribute, Html, text)
import UI.Button as Button


primary : msg -> String -> List (Attribute msg) -> Html msg
primary =
    styled [ primaryStyle ]


secondary : msg -> String -> List (Attribute msg) -> Html msg
secondary =
    styled [ secondaryStyle ]


view : msg -> String -> List (Attribute msg) -> Html msg
view =
    styled []


view2 : List (Attribute msg) -> msg -> String -> Html msg
view2 =
    styled2 []


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


styled2 :
    List Style
    -> List (Attribute msg)
    -> msg
    -> String
    -> Html msg
styled2 styles attrs action label =
    Button.styled styles
        action
        attrs
        [ text label ]
