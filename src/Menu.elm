module Menu exposing (..)

import Array exposing (Array)
import BasicsExtra exposing (callWith)
import Css exposing (Rem, Style, px, rem)
import Focus
import FunctionalCss exposing (bold, noStyle, ph, pv, styleIf)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events as E
import HtmlExtra as HX
import Task
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = Model (Maybe Int)


type CloseReason
    = LostFocus
    | Canceled


type Msg item
    = Closed CloseReason
    | Selected item
    | KeyMsg Int KeyMsg


type KeyMsg
    = Up
    | Down
    | EnterOrSpace


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

        KeyMsg total keyMsg ->
            case keyMsg of
                Up ->
                    ( mapActiveIdx (roll -1 total) model, Cmd.none )

                Down ->
                    ( model, Cmd.none )

                EnterOrSpace ->
                    ( model, Cmd.none )


map : (Maybe Int -> Maybe Int) -> Model -> Model
map func (Model internal) =
    func internal |> Model


roll : Int -> Int -> Maybe Int -> Maybe Int
roll offset length activeIdx =
    Just <|
        case activeIdx of
            Nothing ->
                if offset > 0 then
                    0

                else
                    length - 1

            Just idx ->
                modBy length (idx + offset)


mapActiveIdx : (Maybe Int -> Maybe Int) -> Model -> Model
mapActiveIdx func =
    map func


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


selectedStyle : Style
selectedStyle =
    Css.batch [ bold ]


rootMenuItemStyle : Style
rootMenuItemStyle =
    Css.batch [ pv 1, ph 2 ]


view : Config item msg -> Maybe item -> List item -> Html msg
view config selected items =
    let
        isSelected item =
            selected /= Nothing && Just item == selected

        viewItem item =
            viewMenuItem
                [ css [ styleIf (isSelected item) selectedStyle ] ]
                (Selected item)
                (config.title item)

        totalItems =
            List.length items

        keyMsg =
            KeyMsg totalItems
    in
    Focus.focusTracker
        [ A.id config.id
        , class "absolute top-1 left--1 shadow-1 bg-white"
        , Key.onEscape (Closed Canceled)
        , Focus.onFocusLost (Closed LostFocus)
        , tabindex -1
        , Key.onDown [ Key.arrowDown <| keyMsg Down ]
        ]
        (List.map viewItem items)
        |> H.map config.toMsg


viewMenuItem : List (H.Attribute msg) -> msg -> String -> Html msg
viewMenuItem attrs action label =
    TextButton.styled [ rootMenuItemStyle ] attrs action label
