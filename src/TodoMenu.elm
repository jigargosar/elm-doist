module TodoMenu exposing (MenuItems, Model, decoder, encoder, init, isOpenFor, openFor, todoMenuDomId, todoMenuFirstFocusableDomId, todoMenuTriggerDomId, view)

import Accessibility.Styled.Key as Key
import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith, eq_, ifElse)
import Browser
import Browser.Dom as Dom exposing (focus)
import Browser.Navigation as Nav
import BrowserSize exposing (BrowserSize)
import Calendar
import Css exposing (none, outline)
import Dialog
import Dict exposing (Dict)
import Dict.Extra
import Errors exposing (Errors)
import Focus
import FontAwesome.Attributes as FAA
import FontAwesome.Brands as FABrands
import FontAwesome.Regular as FAR
import FontAwesome.Solid as FAS
import FontAwesome.Styles
import FunctionalCss as FCss
import HasErrors
import Html.Styled as H exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes as A
    exposing
        ( checked
        , class
        , classList
        , css
        , disabled
        , href
        )
import Html.Styled.Events exposing (onClick, preventDefaultOn)
import HtmlStyledExtra as HX
import InlineEditTodo
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Maybe.Extra as MX
import Millis exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Result.Extra as RX
import Return
import Route exposing (Route)
import Skeleton
import String.Extra as SX
import Svg.Attributes as SA
import Task
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UI.Button as Button
import UI.FAIcon as FAIcon
import UI.IconButton as IconButton
import UI.TextButton as TextButton
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)


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


type alias MenuItems msg =
    List ( TodoId -> msg, String )


view : (TodoId -> Bool -> msg) -> MenuItems msg -> TodoId -> Model -> Html msg
view onClose menuItemModelList todoId model =
    HX.viewIf (isOpenFor todoId model)
        (viewHelp onClose menuItemModelList todoId)


viewHelp : (TodoId -> Bool -> msg) -> MenuItems msg -> TodoId -> Html msg
viewHelp onClose menuItemModelList todoId =
    let
        viewMenuItem : number -> ( TodoId -> msg, String ) -> Html msg
        viewMenuItem idx ( todoAction, label ) =
            TextButton.view (todoAction todoId)
                label
                [ A.id <|
                    ifElse (idx == 0)
                        (todoMenuFirstFocusableDomId todoId)
                        ""
                , class "pa2"
                ]

        menuDomId =
            todoMenuDomId todoId

        closeMsg : Bool -> msg
        closeMsg restoreFocus =
            onClose todoId restoreFocus
    in
    div
        [ A.id menuDomId
        , class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , Focus.onFocusOutsideDomId menuDomId (closeMsg False)
        , preventDefaultOn "keydown" (Key.escape ( closeMsg True, True ))
        ]
        (menuItemModelList |> List.indexedMap viewMenuItem)
