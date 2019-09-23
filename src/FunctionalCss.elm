module FunctionalCss exposing (..)

import Array exposing (Array)
import BasicsExtra exposing (callWith)
import Css exposing (Rem, Style, hex, rem)
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


spacingArray : Array Rem
spacingArray =
    [ 0, 0.25, 0.5, 1, 1.5, 2, 4, 8 ]
        |> List.map rem
        |> Array.fromList


sp : Int -> Rem
sp idx =
    spacingArray |> Array.get idx |> Maybe.withDefault (rem (toFloat idx))


pv : Int -> Style
pv v =
    [ Css.paddingTop, Css.paddingBottom ]
        |> List.map (callWith (sp v))
        |> Css.batch


ph : Int -> Style
ph v =
    [ Css.paddingLeft, Css.paddingRight ]
        |> List.map (callWith (sp v))
        |> Css.batch


hexLightBlue =
    hex "#96ccff"


bgLightBlue : Style
bgLightBlue =
    Css.backgroundColor hexLightBlue
