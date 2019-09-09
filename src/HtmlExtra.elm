module HtmlExtra exposing (idIf, none, viewIf, viewIfListNotEmpty)

-- VIEW HELPERS

import Html.Styled exposing (Html, text)
import Html.Styled.Attributes as A


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


idIf : Bool -> (() -> String) -> Html.Styled.Attribute msg
idIf bool domId =
    A.id <|
        if bool then
            domId ()

        else
            ""
