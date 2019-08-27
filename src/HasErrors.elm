module HasErrors exposing (HasErrors, add, addDecodeError, detailView)

import Errors exposing (Errors)
import Html.Styled exposing (Html)
import Json.Decode as JD


type alias HasErrors a =
    { a | errors : Errors }


add : String -> HasErrors a -> HasErrors a
add error =
    mapErrors (Errors.prependString error)


mapErrors : (Errors -> Errors) -> HasErrors a -> HasErrors a
mapErrors fn model =
    { model | errors = fn model.errors }


addDecodeError : JD.Error -> HasErrors a -> HasErrors a
addDecodeError error =
    mapErrors (Errors.prependDecodeError error)


detailView : HasErrors a -> Html msg
detailView { errors } =
    Errors.detailView errors
