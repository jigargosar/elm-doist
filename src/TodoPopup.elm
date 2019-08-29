module TodoPopup exposing
    ( MenuItem
    , MenuItems
    , Model
    , Msg
    , firstFocusableDomId
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
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (on, preventDefaultOn)
import HtmlExtra as HX
import Json.Decode as JD exposing (Decoder)
import Maybe.Extra as MX
import TodoId exposing (TodoId)
import UpdateExtra exposing (commandIf, effect, pure)


type Model
    = Open TodoId
    | Closed


initialValue : Model
initialValue =
    Closed


type Msg
    = OpenFor TodoId
    | Close TodoId Bool
    | Focused (Result Dom.Error ())


open : TodoId -> Msg
open =
    OpenFor


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
        OpenFor todoId ->
            pure (Open todoId)
                |> effect focusFirstEffect

        Close todoId restoreFocus ->
            ifElse (isOpenFor todoId model)
                (Closed
                    |> pure
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
            Close todoId restoreFocus
                |> toMsg
    in
    H.node "track-focus-outside"
        [ A.id menuDomId
        , class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons

        --        , Focus.onFocusOutsideDomId menuDomId (closeMsg False)
        , on "focusOutside" (JD.succeed <| closeMsg False)
        , preventDefaultOn "keydown"
            (JD.lazy
                (\_ ->
                    JD.field "defaultPrevented" JD.bool
                        |> JD.andThen
                            (\defaultPrevented ->
                                if defaultPrevented then
                                    JD.fail "defaultPrevented"

                                else
                                    Key.escape ( closeMsg True, True )
                            )
                )
            )
        , tabindex -1
        ]
        --        (menuItems |> List.indexedMap viewMenuItem)
        menuItems
