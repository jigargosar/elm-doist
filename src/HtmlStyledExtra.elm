module HtmlStyledExtra exposing (empty, viewIf, viewUnless)

-- VIEW HELPERS

import Html.Styled exposing (Html, text)


viewIf bool v =
    if bool then
        v

    else
        text ""


viewUnless bool v =
    viewIf (not bool) v


empty : Html msg
empty =
    text ""
