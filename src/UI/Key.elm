module UI.Key exposing (onKeyDownPreventDefault, pdOnKeydown)

import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)


preventDefault : msg -> ( msg, Bool )
preventDefault msg =
    Tuple.pair msg True


onKeyDownPreventDefault : msg -> List (msg -> Decoder msg) -> Attribute msg
onKeyDownPreventDefault msg keys =
    let
        decoder =
            JD.lazy
                (\_ ->
                    keys
                        |> List.map ((|>) msg)
                        |> JD.oneOf
                        |> JD.map preventDefault
                )
    in
    preventDefaultOn "keydown" decoder


pdOnKeydown : Decoder ( msg, Bool ) -> Attribute msg
pdOnKeydown =
    preventDefaultOn "keydown"
