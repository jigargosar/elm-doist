module TodoPopup exposing
    ( MenuItem
    , MenuItems
    , Model
    , Msg
    , firstFocusableDomId
    , initialValue
    , open
    , update
    , view
    )

import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes exposing (class, tabindex)
import Html.Styled.Events exposing (on)
import HtmlExtra as HX
import Json.Decode as JD exposing (Decoder)
import TodoId exposing (TodoId)
import UI.Key as Key
import UpdateExtra exposing (command, pure)


type Model
    = Open TodoId
    | Closed


initialValue : Model
initialValue =
    Closed


type Msg
    = OpenFor TodoId
    | Close


open : TodoId -> Msg
open =
    OpenFor


update :
    { firstFocusable : String
    , focus : String -> Cmd msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { firstFocusable, focus } msg model =
    case msg of
        OpenFor todoId ->
            pure (Open todoId)
                |> command (focus firstFocusable)

        Close ->
            Closed |> pure


isOpenFor : TodoId -> Model -> Bool
isOpenFor todoId model =
    model == Open todoId


firstFocusableDomId : String
firstFocusableDomId =
    "todo-popup--first-focusable--dom-id"


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
        (viewHelp toMsg menuItems)


viewHelp : (Msg -> msg) -> List (Html msg) -> Html msg
viewHelp toMsg menuItems =
    let
        closeMsg : msg
        closeMsg =
            toMsg Close
    in
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , on "focusOutside" (JD.succeed <| closeMsg)
        , Key.onEscape closeMsg
        , tabindex -1
        ]
        menuItems
