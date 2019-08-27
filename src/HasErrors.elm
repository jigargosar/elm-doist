module HasErrors exposing (HasErrors, add, addDecodeError, addDomFocusError, detailView)

import Browser.Dom as Dom
import Errors exposing (Errors)
import Html.Styled exposing (Html)
import Json.Decode as JD


type alias HasErrors a =
    { a | errors : Errors }


mapErrors : (Errors -> Errors) -> HasErrors a -> HasErrors a
mapErrors fn model =
    { model | errors = fn model.errors }


add : String -> HasErrors a -> HasErrors a
add error =
    mapErrors (Errors.add error)


addDecodeError : JD.Error -> HasErrors a -> HasErrors a
addDecodeError error =
    mapErrors (Errors.addDecodeError error)


addDomFocusError : Dom.Error -> HasErrors a -> HasErrors a
addDomFocusError (Dom.NotFound domId) =
    add ("DomFocusError NotFound: " ++ domId)


detailView : HasErrors a -> Html msg
detailView { errors } =
    Errors.detailView errors
