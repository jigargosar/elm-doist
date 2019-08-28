module TodoPopup exposing
    ( MenuItems
    , Model
    , Msg
    , close
    , decoder
    , encoder
    , initialValue
    , open
    , triggerDomId
    , update
    , view
    )

import Accessibility.Styled.Key as Key
import BasicsExtra exposing (ifElse)
import Browser.Dom as Dom
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
    let
        focus : String -> Cmd msg
        focus =
            Focus.attempt (Focused >> toMsg)
    in
    case msg of
        OpenFor todoId ->
            pure (Open todoId)
                |> effect cacheEffect
                |> command (focus (firstFocusableDomId todoId))

        CloseFor todoId restoreFocus ->
            let
                shouldClose =
                    case model of
                        Open todoId_ ->
                            todoId == todoId_

                        Closed ->
                            False
            in
            ifElse shouldClose
                (Closed
                    |> pure
                    |> effect cacheEffect
                    |> commandIf restoreFocus
                        (focus (triggerDomId todoId))
                )
                (pure model)

        Focused _ ->
            pure model


cacheEffect : Model -> Cmd msg
cacheEffect model =
    Ports.localStorageSetJsonItem
        ( "cachedTodoMenu", encoder model )


isOpenFor : TodoId -> Model -> Bool
isOpenFor todoId_ model =
    case model of
        Open tid ->
            todoId_ == tid

        Closed ->
            False


todoMenuDomId : TodoId -> String
todoMenuDomId todoId =
    "todo-popup-dom-id--" ++ TodoId.toString todoId


triggerDomId : TodoId -> String
triggerDomId todoId =
    "todo-popup-trigger-dom-id--" ++ TodoId.toString todoId


firstFocusableDomId : TodoId -> String
firstFocusableDomId todoId =
    "todo-popup--first-focusable--dom-id--" ++ TodoId.toString todoId


type alias MenuItems msg =
    List ( TodoId -> msg, String )


view :
    (Msg -> msg)
    -> MenuItems msg
    -> TodoId
    -> Model
    -> Html msg
view toMsg menuItemModelList todoId model =
    HX.viewIf (isOpenFor todoId model)
        (viewHelp toMsg menuItemModelList todoId)


viewHelp : (Msg -> msg) -> MenuItems msg -> TodoId -> Html msg
viewHelp toMsg menuItemModelList todoId =
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
            CloseFor todoId restoreFocus
                |> toMsg
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
