module Menu exposing (..)

import Focus
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A exposing (class, tabindex)
import HtmlExtra as HX
import Task
import UI.Key as Key


type Model
    = Model (Maybe Int)


type CloseReason
    = LostFocus
    | Canceled


type Msg item
    = Closed CloseReason
    | Selected item


type alias Config item msg =
    { toMsg : Msg item -> msg
    , selected : item -> msg
    , closed : CloseReason -> msg
    , id : String
    , title : item -> String
    }


update : Config item msg -> Msg item -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        Closed reason ->
            ( model, config.closed reason |> perform )

        Selected item ->
            ( model, config.selected item |> perform )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


view : Config item msg -> Maybe item -> List item -> Html msg
view config selected items =
    let
        viewItem item =
            HX.none
    in
    Focus.focusTracker
        [ A.id config.id
        , class "absolute top-1 left--1 shadow-1 bg-white"
        , Key.onEscape (Closed Canceled)
        , Focus.onFocusLost (Closed LostFocus)
        , tabindex -1
        ]
        (List.map viewItem items)
        |> H.map config.toMsg
