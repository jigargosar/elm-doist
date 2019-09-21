module SelectInput exposing (Config, Model, Msg, init, update, view)

import Css
import Focus
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events as E
import Json.Decode as JD
import ListZipper as LZ
import Task
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = IsOpen Bool


type alias Internal =
    {}


init : Model
init =
    IsOpen False


type Msg item
    = OpenPopup
    | Selected item
    | ClosePopup


type alias Config msg item =
    { id : String
    , toMsg : Msg item -> msg
    , onSelect : item -> msg
    , itemLabel : item -> String
    }


update : Config msg item -> Msg item -> Model -> ( Model, Cmd msg )
update config message _ =
    case message of
        OpenPopup ->
            ( IsOpen True
            , Focus.autoFocusWithinId (selectInputId config.id)
            )

        ClosePopup ->
            ( IsOpen False, Cmd.none )

        Selected projectId ->
            ( IsOpen False, config.onSelect projectId |> perform )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


view : Config msg item -> LZ.ListZipper item -> Model -> H.Html msg
view config items model =
    let
        open =
            case model of
                IsOpen bool ->
                    bool
    in
    viewHelp
        { id = selectInputId config.id
        , itemLabel = config.itemLabel
        , onClose = ClosePopup
        , onOpen = OpenPopup
        , onSelect = Selected
        }
        { open = open, items = items }
        |> H.map config.toMsg


selectInputId uid =
    "select-input__" ++ uid


viewHelp :
    { id : String
    , itemLabel : item -> String
    , onClose : msg
    , onOpen : msg
    , onSelect : item -> msg
    }
    -> { open : Bool, items : ( List item, item, List item ) }
    -> Html msg
viewHelp config props =
    let
        selectedItem =
            LZ.zipperFocus props.items

        allItems =
            LZ.zipperToList props.items

        firstItem =
            List.head allItems |> Maybe.withDefault selectedItem

        selectedItemStyle item =
            if item == selectedItem then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []

        attrsForItem item =
            [ Focus.dataAutoFocus (item == firstItem)
            , css [ selectedItemStyle item ]
            ]

        viewItem item =
            viewMenuItem (attrsForItem item) (config.onSelect item) (config.itemLabel item)
    in
    div (class "relative" :: [ A.id config.id ])
        [ div [ E.onClick config.onOpen ]
            [ text (config.itemLabel selectedItem)
            ]
        , if props.open then
            H.styled (H.node "track-focus-outside")
                []
                [ class "absolute top-1 left--1 shadow-1 bg-white"
                , E.on "focusOutside" (JD.succeed config.onClose)
                , Key.onEscape config.onClose
                , tabindex -1
                ]
                (List.map viewItem allItems)

          else
            text ""
        ]


viewMenuItem : List (Attribute msg) -> msg -> String -> Html msg
viewMenuItem attrs =
    TextButton.view (class "pa2" :: attrs)
