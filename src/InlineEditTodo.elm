module InlineEditTodo exposing
    ( Model
    , ViewConfig
    , decoder
    , dueAtOrDefault
    , firstFocusableDomId
    , fromTodo
    , idEq
    , maybeEncoder
    , setDueAt
    , setIsScheduling
    , setTitle
    , titleOrDefault
    , toUpdateMessages
    , view
    )

import BasicsExtra exposing (ifElse)
import Calendar
import Css exposing (minWidth, none, px, resize)
import Html.Styled as H exposing (Attribute, Html, div, textarea)
import Html.Styled.Attributes as A
    exposing
        ( class
        , css
        , rows
        , tabindex
        , value
        )
import Html.Styled.Events exposing (onInput)
import HtmlExtra as HX
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Maybe as M exposing (Maybe)
import Maybe.Extra as MX
import Millis exposing (Millis)
import SchedulePopup
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type alias ModelRecord =
    { todo : Todo
    , title : Maybe String
    , dueAt : Maybe DueAt
    , isSchedulePopupOpen : Bool
    }


type Model
    = Model ModelRecord


maybeEncoder : Maybe Model -> Value
maybeEncoder =
    MX.unwrap JE.null encoder


encoder : Model -> Value
encoder (Model { todo, title, dueAt, isSchedulePopupOpen }) =
    let
        maybeNull : (a -> Value) -> Maybe a -> Value
        maybeNull enc =
            MX.unwrap JE.null enc
    in
    JE.object
        [ ( "todo", Todo.encoder todo )
        , ( "title", maybeNull JE.string title )
        , ( "dueAt", maybeNull Todo.dueAtEncoder dueAt )
        , ( "isScheduling", JE.bool isSchedulePopupOpen )
        ]


decoder : Decoder Model
decoder =
    JD.succeed ModelRecord
        |> JDP.required "todo" Todo.decoder
        |> JDP.optional "title" (JD.nullable JD.string) Nothing
        |> JDP.optional "dueAt" (JD.nullable Todo.dueAtDecoder) Nothing
        |> JDP.optional "isScheduling" JD.bool False
        |> JD.map Model


fromRecord : ModelRecord -> Model
fromRecord =
    Model


fromTodo : Todo -> Model
fromTodo todo =
    { todo = todo
    , title = Nothing
    , dueAt = Nothing
    , isSchedulePopupOpen = False
    }
        |> fromRecord


setDueAt : DueAt -> Model -> Model
setDueAt dueAt (Model modelRecord) =
    { modelRecord | dueAt = Just dueAt }
        |> Model


setTitle : String -> Model -> Model
setTitle title (Model modelRecord) =
    { modelRecord | title = Just title }
        |> Model


setIsScheduling : Bool -> Model -> Model
setIsScheduling bool (Model modelRecord) =
    { modelRecord | isSchedulePopupOpen = bool }
        |> Model


idEq : TodoId -> Model -> Bool
idEq todoId_ (Model modelRecord) =
    modelRecord.todo.id == todoId_


toUpdateMessages : Model -> Maybe ( Todo, List Todo.Msg )
toUpdateMessages (Model { todo, dueAt, title }) =
    [ dueAt |> M.map Todo.SetDueAt, title |> M.map Todo.SetTitle ]
        |> List.filterMap identity
        |> (\l -> ifElse (List.isEmpty l) Nothing (Just ( todo, l )))


titleOrDefault : Model -> String
titleOrDefault (Model { todo, title }) =
    title
        |> Maybe.withDefault todo.title


dueAtOrDefault : Model -> DueAt
dueAtOrDefault (Model { todo, dueAt }) =
    dueAt |> Maybe.withDefault todo.dueAt


firstFocusableDomId =
    "inline-edit-todo__first-focusable"


getIsSchedulePopupOpen : Model -> Bool
getIsSchedulePopupOpen (Model { isSchedulePopupOpen }) =
    isSchedulePopupOpen


type alias ViewConfig msg =
    { openSchedulePopup : msg
    , titleChanged : String -> msg
    , cancel : msg
    , save : msg
    , viewSchedulePopup : Bool -> Zone -> Calendar.Date -> Html msg
    }


view :
    ViewConfig msg
    -> Time.Zone
    -> Calendar.Date
    -> Model
    -> Html msg
view conf here today model =
    div
        [ class "pv3 ph2 "
        , tabindex 0
        , Key.onEscape conf.cancel
        ]
        [ div [ class "flex ba b--moon-gray" ]
            [ viewTitleInput conf model
            , viewDueAt conf here today model
            ]
        , div [ class "flex hs3 lh-copy" ]
            [ TextButton.primary conf.save "Save" [ class "pa2" ]
            , TextButton.secondary conf.cancel "Cancel" [ class "pa2" ]
            ]
        ]


viewTitleInput : { a | titleChanged : String -> msg, save : msg } -> Model -> Html msg
viewTitleInput conf model =
    H.node "auto-resize-textarea"
        [ class "flex-grow-1 flex br b--moon-gray" ]
        [ textarea
            [ A.id firstFocusableDomId
            , class "pa1 flex-grow-1 lh-copy bn"
            , value <| titleOrDefault model
            , onInput conf.titleChanged
            , Key.onEnter conf.save
            , rows 1
            , css [ resize none ]
            , class "overflow-hidden"
            ]
            []
        ]


viewDueAt :
    { a
        | openSchedulePopup : msg
        , viewSchedulePopup : Bool -> Zone -> c -> Html msg
    }
    -> Zone
    -> c
    -> Model
    -> Html msg
viewDueAt conf here today model =
    let
        ( dueAtLabel, dueAtCls ) =
            dueAtOrDefault model
                |> Todo.dueAtToMillis
                |> MX.unpack
                    (\_ -> ( "Schedule", "gray" ))
                    (\mi ->
                        ( Millis.formatDate "MMM dd" here <| mi, "near-black" )
                    )
    in
    div [ class "flex relative" ]
        [ TextButton.secondary conf.openSchedulePopup
            dueAtLabel
            [ class "pa1"
            , class dueAtCls
            , css [ minWidth <| px 100 ]
            ]
        , conf.viewSchedulePopup (getIsSchedulePopupOpen model) here today
        ]
