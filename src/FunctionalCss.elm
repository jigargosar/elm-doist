module FunctionalCss exposing (..)

import Css exposing (Style)
import CssColors


underline : Style
underline =
    Css.textDecoration Css.underline


noDecoration : Style
noDecoration =
    Css.textDecoration Css.none


blue : Style
blue =
    Css.color CssColors.blue


white : Style
white =
    Css.color CssColors.white


gray : Style
gray =
    Css.color CssColors.gray


pointer : Style
pointer =
    Css.cursor Css.pointer


bold =
    Css.fontWeight Css.bold


noStyle : Style
noStyle =
    Css.batch []


styleIf : Bool -> Style -> Style
styleIf bool style =
    if bool then
        style

    else
        noStyle
