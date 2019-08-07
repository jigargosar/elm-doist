module HasErrors exposing (Error, HasErrors, initial, prependDecodeError, prependString, view)

import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class)
import Json.Decode as JD


type alias Error =
    String


type alias HasErrors a =
    { a | errors : List Error }


initial : List Error
initial =
    []


prependString : String -> HasErrors a -> HasErrors a
prependString error model =
    { model | errors = error :: model.errors }


prependDecodeError : JD.Error -> HasErrors a -> HasErrors a
prependDecodeError error =
    prependString (JD.errorToString error)


view : List Error -> Html msg
view list =
    div [ class "vs3" ] (List.map viewError list)


viewError : Error -> Html msg
viewError error =
    div [] [ text error ]
