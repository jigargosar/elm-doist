module HasErrors exposing (Error, Errors, HasErrors, detailView, empty, fromStrings, prependDecodeError, prependString)

import Html.Styled exposing (Html, div, li, ol, text)
import Html.Styled.Attributes exposing (class)
import HtmlStyledExtra
import Json.Decode as JD


type alias Error =
    String


type alias Errors =
    List Error


type alias HasErrors a =
    { a | errors : Errors }


empty : List Error
empty =
    []


fromStrings : List String -> Errors
fromStrings errors =
    errors


prependString : String -> HasErrors a -> HasErrors a
prependString error model =
    { model | errors = error :: model.errors }


prependDecodeError : JD.Error -> HasErrors a -> HasErrors a
prependDecodeError error =
    prependString (JD.errorToString error)


detailView : HasErrors a -> Html msg
detailView { errors } =
    HtmlStyledExtra.viewUnless (errors |> List.isEmpty) <|
        div [ class "ph3 vs3" ]
            [ div [ class "ttu tracked" ] [ text "Errors:" ]
            , ol [ class "vs3" ] (List.map viewError errors)
            ]


viewError : Error -> Html msg
viewError error =
    li [] [ text error ]
