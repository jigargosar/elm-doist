module TodoPopup exposing
    ( MenuItems
    , Model
    , Msg
    , close
    , closeFor
    , decoder
    , encoder
    , firstFocusableDomId
    , initialValue
    , open
    , openFor
    , triggerDomId
    , update
    , view
    )

import Accessibility.Styled.Key as Key
import BasicsExtra exposing (ifElse)
import Browser.Dom as Dom exposing (focus)
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
import Ports
import Task
import TodoId exposing (TodoId)
import UI.TextButton as TextButton
import UpdateExtra exposing (command, commandIf, effect, pure)


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


initialValue : Model
initialValue =
    Closed


type Msg
    = OpenFor TodoId
    | CloseFor TodoId Bool
    | Focused (Result Dom.Error ())


open =
    OpenFor


close =
    CloseFor


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toMsg msg model =
    case msg of
        OpenFor todoId ->
            pure (openFor todoId)
                |> effect cacheTodoMenuEffect
                |> command (focusTodoPopupCmd todoId |> Cmd.map toMsg)

        CloseFor todoId restoreFocus ->
            closeFor todoId model
                |> Maybe.map
                    (pure
                        >> effect cacheTodoMenuEffect
                        >> commandIf restoreFocus
                            (focusDomIdCmd (triggerDomId todoId)
                                |> Cmd.map toMsg
                            )
                    )
                |> Maybe.withDefault (pure model)

        Focused _ ->
            pure model


focusDomIdCmd : String -> Cmd Msg
focusDomIdCmd domId =
    focus domId |> Task.attempt Focused


focusTodoPopupCmd : TodoId -> Cmd Msg
focusTodoPopupCmd todoId =
    let
        domId =
            firstFocusableDomId todoId
    in
    focus domId |> Task.attempt Focused


cacheTodoMenuEffect : Model -> Cmd msg
cacheTodoMenuEffect model =
    Ports.localStorageSetJsonItem
        ( "cachedTodoMenu", encoder model )


openFor : TodoId -> Model
openFor todoId_ =
    Open todoId_


closeFor : TodoId -> Model -> Maybe Model
closeFor todoId_ model =
    ifElse (isOpenFor todoId_ model)
        (Just Closed)
        Nothing


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


triggerDomId : TodoId -> String
triggerDomId todoId =
    "todo-menu-trigger-dom-id--" ++ TodoId.toString todoId


firstFocusableDomId : TodoId -> String
firstFocusableDomId todoId =
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
                        (firstFocusableDomId todoId)
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
