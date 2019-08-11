module HasErrors exposing (Error, ErrorList, HasErrors, empty, fromStrings, prependDecodeError, prependString, view)

import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class)
import HtmlStyledExtra
import Json.Decode as JD


type alias Error =
    String


type alias ErrorList =
    List Error


type alias HasErrors a =
    { a | errors : ErrorList }


empty : List Error
empty =
    []


fromStrings : List String -> ErrorList
fromStrings errors =
    errors


prependString : String -> HasErrors a -> HasErrors a
prependString error model =
    { model | errors = error :: model.errors }


prependDecodeError : JD.Error -> HasErrors a -> HasErrors a
prependDecodeError error =
    prependString (JD.errorToString error)


view : ErrorList -> Html msg
view errors =
    HtmlStyledExtra.viewUnless (errors |> List.isEmpty) <|
        div [ class "ph3 flex hs3" ]
            [ div [ class "ttu tracked" ] [ text "Errors:" ]
            , div [ class "vs3" ] (List.map viewError errors)
            ]


viewError : Error -> Html msg
viewError error =
    div [] [ text error ]
