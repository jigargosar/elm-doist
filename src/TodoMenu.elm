module TodoMenu exposing (Model, decoder, encoder, init, isOpenFor, openFor, todoMenuDomId, todoMenuFirstFocusableDomId, todoMenuTriggerDomId)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TodoId exposing (TodoId)


type Model
    = Open TodoId
    | Closed


encoder : Model -> Value
encoder model =
    case model of
        Open todoId ->
            JE.object
                [ ( "tag", JE.string "Open" )
                , ( "todoId", TodoId.encoder todoId )
                ]

        Closed ->
            JE.object
                [ ( "tag", JE.string "Closed" )
                ]


decoder : Decoder Model
decoder =
    let
        decoderForTag tag =
            case tag of
                "Open" ->
                    JD.field "todoId" TodoId.decoder
                        |> JD.map Open

                "Closed" ->
                    JD.succeed Closed

                _ ->
                    JD.fail ("unknown tag for TodoMenu.Model: " ++ tag)
    in
    JD.field "tag" JD.string |> JD.andThen decoderForTag


init : Model
init =
    Closed


openFor : TodoId -> Model
openFor todoId_ =
    Open todoId_


isOpenFor : TodoId -> Model -> Bool
isOpenFor todoId_ model =
    case model of
        Open tid ->
            todoId_ == tid

        Closed ->
            False


todoMenuDomId : TodoId -> String
todoMenuDomId todoId =
    "todo-menu-dom-id--" ++ TodoId.toString todoId


todoMenuTriggerDomId : TodoId -> String
todoMenuTriggerDomId todoId =
    "todo-menu-trigger-dom-id--" ++ TodoId.toString todoId


todoMenuFirstFocusableDomId : TodoId -> String
todoMenuFirstFocusableDomId todoId =
    "todo-menu--first-focusable--dom-id--" ++ TodoId.toString todoId
