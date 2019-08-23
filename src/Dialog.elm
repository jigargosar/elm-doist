module Dialog exposing (Dialog(..), decoder, encoder, view)

import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)


type Dialog
    = NoDialog
    | MoveToProjectDialog TodoId ProjectId
    | DueDialog TodoId


decoder : Decoder Dialog
decoder =
    JD.field "tag" JD.string
        |> JD.andThen dialogDecoderForTag


encoder : Dialog -> Value
encoder dialog =
    case dialog of
        NoDialog ->
            JE.object [ ( "tag", JE.string "NoDialog" ) ]

        MoveToProjectDialog todoId projectId ->
            JE.object
                [ ( "tag", JE.string "MoveToProjectDialog" )
                , ( "todoId", TodoId.encoder todoId )
                , ( "projectId", ProjectId.encoder projectId )
                ]

        DueDialog todoId ->
            JE.object
                [ ( "tag", JE.string "DueDialog" )
                , ( "todoId", TodoId.encoder todoId )
                ]


dialogDecoderForTag : String -> Decoder Dialog
dialogDecoderForTag tag =
    case tag of
        "NoDialog" ->
            JD.succeed NoDialog

        "MoveToProjectDialog" ->
            JD.map2 MoveToProjectDialog
                (JD.field "todoId" TodoId.decoder)
                (JD.field "projectId" ProjectId.decoder)

        "DueDialog" ->
            JD.field "todoId" TodoId.decoder
                |> JD.map DueDialog

        _ ->
            JD.fail ("Invalid Dialog Tag:" ++ tag)


view : msg -> List (Html msg) -> Html msg
view onOverlayClick content =
    div
        [ class "absolute absolute--fill flex items-center justify-center"
        ]
        [ div
            [ class "absolute absolute--fill bg-black-50"
            , onClick onOverlayClick
            ]
            []
        , div [ class "absolute" ] content
        , H.node "style" [] [ text "body { overflow: hidden; }" ]
        ]
