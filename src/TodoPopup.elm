module TodoPopup exposing
    ( MenuItems
    , Model
    , decoder
    , encoder
    , init
    , isOpenFor
    , openFor
    , todoMenuDomId
    , todoMenuFirstFocusableDomId
    , todoMenuTriggerDomId
    , view
    )

import Accessibility.Styled.Key as Key
import BasicsExtra exposing (ifElse)
import Focus
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes as A
    exposing
        ( class
        )
import Html.Styled.Events exposing (preventDefaultOn)
import HtmlStyledExtra as HX
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TodoId exposing (TodoId)
import UI.TextButton as TextButton


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
                    JD.fail ("unknown tag for TodoPopup.Model: " ++ tag)
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
