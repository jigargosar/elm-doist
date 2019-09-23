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


viewMenuItem attrs action label =
    TextButton.styled [ rootMenuItemStyle ] attrs action label
