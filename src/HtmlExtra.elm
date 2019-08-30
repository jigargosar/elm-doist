module HtmlExtra exposing (empty, viewIf, viewIfNotEmpty)

-- VIEW HELPERS

import Html.Styled exposing (Html, text)


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf bool vfn =
    if bool then
        vfn ()

    else
        text ""


empty : Html msg
empty =
    text ""


viewIfNotEmpty : (List a -> Html msg) -> List a -> Html msg
viewIfNotEmpty vfn list =
    if List.isEmpty list then
        empty

    else
        vfn list
