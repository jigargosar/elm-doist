module SelectInput exposing (..)

import Css
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (class, css, tabindex)
import Html.Styled.Events as E
import HtmlExtra as HX
import Json.Decode as JD
import ListZipper as LZ
import UI.Key as Key
import UI.TextButton as TextButton


view :
    { attrs : List (Attribute msg)
    , itemLabel : item -> String
    , onClose : msg
    , onOpen : msg
    , onSelect : item -> msg
    }
    -> { open : Bool, items : ( List item, item, List item ) }
    -> Html msg
view config props =
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
            [ HX.idIf (item == firstItem) (always "")
            , css [ selectedItemStyle item ]
            ]

        viewItem item =
            viewMenuItem (attrsForItem item) (config.onSelect item) (config.itemLabel item)
    in
    div [ class "relative" ]
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
