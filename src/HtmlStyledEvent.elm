module HtmlStyledEvent exposing (onSelfClick)

import Html.Styled.Events exposing (on)
import Json.Decode as JD


selfClickDecoder selfDomId tagger =
    JD.at [ "target", "id" ] JD.string
        |> JD.andThen
            (\domId ->
                if domId == selfDomId then
                    JD.succeed tagger

                else
                    JD.fail ("Not Clicked on:" ++ selfDomId)
            )


onSelfClick selfDomId tagger =
    on "click" (selfClickDecoder selfDomId tagger)
