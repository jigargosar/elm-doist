module TodoPopup exposing
    ( MenuItem
    , MenuItems
    , Model
    , Msg
    , decoder
    , encoder
    , firstFocusableDomId
    , initialValue
    , loadFromCache
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
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (preventDefaultOn)
import HtmlStyledExtra as HX
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra as MX
import Ports
import Result.Extra as RX
import TodoId exposing (TodoId)
import UpdateExtra exposing (commandIf, effect, pure)


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
    | LoadFromCache Value


open : TodoId -> Msg
open =
    OpenFor


loadFromCache : Value -> Msg
loadFromCache =
    LoadFromCache


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toMsg msg model =
    let
        focus : String -> Cmd msg
        focus =
            Focus.attempt (Focused >> toMsg)

        focusFirst : TodoId -> Cmd msg
        focusFirst =
            firstFocusableDomId >> focus

        focusFirstEffect : Model -> Cmd msg
        focusFirstEffect =
            getTodoId >> MX.unwrap Cmd.none focusFirst
    in
    case msg of
        LoadFromCache value ->
            value
                |> JD.decodeValue (JD.field "cachedTodoMenu" decoder)
                |> RX.unwrap (pure model)
                    (pure >> effect focusFirstEffect)

        OpenFor todoId ->
            pure (Open todoId)
                |> effect cacheEffect
                |> effect focusFirstEffect

        CloseFor todoId restoreFocus ->
            ifElse (isOpenFor todoId model)
                (Closed
                    |> pure
                    |> effect cacheEffect
                    |> commandIf restoreFocus
                        (focus (triggerDomId todoId))
                )
                (pure model)

        Focused _ ->
            pure model


getTodoId : Model -> Maybe TodoId
getTodoId model =
    case model of
        Open todoId_ ->
            Just todoId_

        Closed ->
            Nothing


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


type alias MenuItem msg =
    ( TodoId -> msg, String )


type alias MenuItems msg =
    List (MenuItem msg)


view :
    (Msg -> msg)
    -> List (Html msg)
    -> TodoId
    -> Model
    -> Html msg
view toMsg menuItems todoId model =
    HX.viewIf (isOpenFor todoId model)
        (viewHelp toMsg menuItems todoId)


viewHelp : (Msg -> msg) -> List (Html msg) -> TodoId -> Html msg
viewHelp toMsg menuItems todoId =
    let
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
        , preventDefaultOn "keydown"
            (JD.field "defaultPrevented" JD.bool
                |> JD.andThen
                    (\defaultPrevented ->
                        if defaultPrevented then
                            JD.fail "defaultPrevented"

                        else
                            Key.escape ( closeMsg True, True )
                    )
            )
        , tabindex -1
        ]
        --        (menuItems |> List.indexedMap viewMenuItem)
        menuItems
