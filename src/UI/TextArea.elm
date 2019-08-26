module UI.TextArea exposing (Model, init)

import Browser.Dom as Dom exposing (focus)
import Task


type Model
    = Model { domId : String, value : String, height : Maybe Float }


type Msg
    = Focused (Result Dom.Error ())
    | GotViewport (Result Dom.Error Dom.Viewport)


init : String -> String -> ( Model, Cmd Msg )
init domId value =
    ( Model { domId = domId, value = value, height = Nothing }, focus domId |> Task.attempt Focused )
