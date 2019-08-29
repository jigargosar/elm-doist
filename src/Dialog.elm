module Dialog exposing (Model(..), decoder, encoder, firstFocusable, init, view)

import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (class, tabindex)
import Html.Styled.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)
import UI.Key as Key


type Model
    = Closed
    | MoveToProjectDialog TodoId ProjectId


init : Model
init =
    Closed


decoder : Decoder Model
decoder =
    JD.field "tag" JD.string
        |> JD.andThen dialogDecoderForTag


encoder : Model -> Value
encoder dialog =
    case dialog of
        Closed ->
            JE.object [ ( "tag", JE.string "Closed" ) ]

        MoveToProjectDialog todoId projectId ->
            JE.object
                [ ( "tag", JE.string "MoveToProjectDialog" )
                , ( "todoId", TodoId.encoder todoId )
                , ( "projectId", ProjectId.encoder projectId )
                ]


dialogDecoderForTag : String -> Decoder Model
dialogDecoderForTag tag =
    case tag of
        "Closed" ->
            JD.succeed Closed

        "MoveToProjectDialog" ->
            JD.map2 MoveToProjectDialog
                (JD.field "todoId" TodoId.decoder)
                (JD.field "projectId" ProjectId.decoder)

        _ ->
            JD.fail ("Invalid Dialog Tag:" ++ tag)


firstFocusable =
    "dialog__first_focusable_domId"


view : msg -> List (Html msg) -> Html msg
view onOverlayClickOrEscapePressed content =
    div
        [ class "z-1 fixed absolute--fill flex items-center justify-center"
        , Key.onEscape onOverlayClickOrEscapePressed
        , tabindex -1
        ]
        [ div
            [ class "absolute absolute--fill bg-black-50"
            , onClick onOverlayClickOrEscapePressed
            ]
            []
        , div [ class "absolute" ] content
        , H.node "style" [] [ text "body { overflow: hidden; }" ]
        ]
