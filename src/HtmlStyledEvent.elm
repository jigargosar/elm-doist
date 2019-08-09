module HtmlStyledEvent exposing (onDomIdClicked, targetDomIdEqDecoder)

import Html.Styled.Events exposing (on)
import Json.Decode as JD


targetDomIdEqDecoder targetDomId tagger =
    JD.at [ "target", "id" ] JD.string
        |> JD.andThen
            (\domId ->
                if domId == targetDomId then
                    JD.succeed tagger

                else
                    JD.fail ("Not Clicked on:" ++ targetDomId)
            )


onDomIdClicked targetDomId tagger =
    on "click" (targetDomIdEqDecoder targetDomId tagger)
