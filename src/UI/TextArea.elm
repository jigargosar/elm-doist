module UI.TextArea exposing (Model, init)

import Browser.Dom as Dom exposing (focus)
import Html.Styled exposing (Attribute, Html, textarea)
import Html.Styled.Attributes as A exposing (value)
import Html.Styled.Events exposing (onInput)
import Result.Extra as RX
import Return exposing (Return)
import Task
import UpdateExtra exposing (pure)


type Model
    = Model { domId : String, value : String, height : Maybe Float }


type Msg
    = Focused (Result Dom.Error ())
    | GotViewport (Result Dom.Error Dom.Viewport)
    | GotInput String


init : String -> String -> ( Model, Cmd Msg )
init domId value =
    ( Model { domId = domId, value = value, height = Nothing }
    , Cmd.batch
        [ focus domId |> Task.attempt Focused
        , Dom.getViewportOf domId |> Task.attempt GotViewport
        ]
    )


update : (Msg -> msg) -> Msg -> Model -> Return msg Model
update toMsg msg (Model model) =
    (case msg of
        Focused _ ->
            pure model

        GotViewport result ->
            result
                |> RX.unpack (\_ -> pure model)
                    (\{ scene } -> pure { model | height = Just scene.height })

        GotInput string ->
            pure model
    )
        |> Tuple.mapFirst Model


view : (Msg -> msg) -> List (Attribute msg) -> Model -> Html msg
view toMsg attrs (Model mr) =
    textarea
        ([ A.id mr.domId, value mr.value, onInput (GotInput >> toMsg) ] ++ attrs)
        []
