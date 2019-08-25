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


gray : Style
gray =
    Css.color CssColors.gray


pointer : Style
pointer =
    Css.cursor Css.pointer
