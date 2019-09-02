module HtmlExtra exposing (none, viewIf, viewIfListNotEmpty)

-- VIEW HELPERS

import Html.Styled exposing (Html, text)


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf bool vfn =
    if bool then
        vfn ()

    else
        text ""


none : Html msg
none =
    text ""


viewIfListNotEmpty : (List a -> Html msg) -> List a -> Html msg
viewIfListNotEmpty vfn list =
    if List.isEmpty list then
        none

    else
        vfn list
