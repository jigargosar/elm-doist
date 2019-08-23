module Dialog exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)


type Dialog
    = NoDialog
    | MoveToProjectDialog TodoId ProjectId
    | DueDialog TodoId


dialogDecoder : Decoder Dialog
dialogDecoder =
    JD.field "tag" JD.string
        |> JD.andThen dialogDecoderForTag


dialogEncoder : Dialog -> Value
dialogEncoder dialog =
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
