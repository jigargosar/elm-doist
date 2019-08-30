module Errors exposing (Errors, add, addDecodeError, detailView, empty, fromStrings, mapErrorList, viewError)

import Html.Styled exposing (Html, div, li, ol, text)
import Html.Styled.Attributes exposing (class)
import HtmlExtra
import Json.Decode as JD


type alias Error =
    String


type Errors
    = Errors (List Error)


empty : Errors
empty =
    [] |> fromStrings


fromStrings : List String -> Errors
fromStrings errors =
    errors |> Errors


add : String -> Errors -> Errors
add error =
    mapErrorList (\errors -> error :: errors)


mapErrorList : (List Error -> List Error) -> Errors -> Errors
mapErrorList fn (Errors errors) =
    fn errors |> Errors


addDecodeError : JD.Error -> Errors -> Errors
addDecodeError error =
    add (JD.errorToString error)


detailView : Errors -> Html msg
detailView (Errors errors) =
    errors
        |> HtmlExtra.viewNonEmptyList
            (\_ ->
                div [ class "vs3" ]
                    [ div [ class "ttu tracked" ] [ text "Errors:" ]
                    , ol [ class "vs3" ] (List.map viewError errors)
                    ]
            )


viewError : Error -> Html msg
viewError error =
    li [] [ text error ]
