module HtmlExtra exposing (empty, viewIf, viewNonEmptyList)

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


viewNonEmptyList : (List a -> Html msg) -> List a -> Html msg
viewNonEmptyList vfn list =
    if List.isEmpty list then
        empty

    else
        vfn list
