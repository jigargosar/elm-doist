module HtmlExtra exposing (viewIf, viewUnless)

-- VIEW HELPERS

import Html exposing (text)


viewIf bool v =
    if bool then
        v

    else
        text ""


viewUnless bool v =
    viewIf (not bool) v
