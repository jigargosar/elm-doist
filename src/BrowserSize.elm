module BrowserSize exposing (BrowserSize, decoder, encoder, fromViewport, initial, onBrowserResize)

import Browser.Events
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)


type alias BrowserSize =
    { width : Int, height : Int }


initial : BrowserSize
initial =
    { width = 0, height = 0 }


decoder : Decoder BrowserSize
decoder =
    JD.succeed BrowserSize
        |> JDP.required "width" JD.int
        |> JDP.required "height" JD.int


encoder : BrowserSize -> Value
encoder { width, height } =
    JE.object
        [ ( "width", JE.int width )
        , ( "height", JE.int height )
        ]


onBrowserResize : (BrowserSize -> msg) -> Sub msg
onBrowserResize tagger =
    Browser.Events.onResize (\w h -> BrowserSize w h |> tagger)


fromViewport : { a | width : Float, height : Float } -> BrowserSize
fromViewport vp =
    BrowserSize (round vp.width) (round vp.height)
