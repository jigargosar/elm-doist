module Button exposing
    ( Role(..)
    , button
    , faBtn
    , secondaryTxtBtn
    , textBtn
    , toHtml
    , withAttrs
    , withIcon
    , withLabel
    , withRole
    , withStyledIcon
    )

import Accessibility.Styled.Key as Key
import FontAwesome.Icon
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes
    exposing
        ( class
        , tabindex
        )
import Html.Styled.Events exposing (preventDefaultOn)
import HtmlStyledExtra exposing (viewMaybe)
import Json.Decode as JD exposing (Decoder)
import Svg
import Svg.Attributes


type Role
    = Primary
    | Secondary
    | Plain


type alias Config msg =
    { role : Role
    , icon : Maybe ( FontAwesome.Icon.Icon, List (Svg.Attribute msg) )
    , text : Maybe String
    , attrs : List (Attribute msg)
    }


type Option msg
    = Role Role
    | Icon FontAwesome.Icon.Icon (List (Svg.Attribute msg))
    | Text String
    | Attrs (List (Attribute msg))


type Button msg
    = Button msg (List (Option msg))


addOption option (Button msg options) =
    Button msg (option :: options)


withRole : Role -> Button msg -> Button msg
withRole role =
    addOption (Role role)


withAttrs : List (Attribute msg) -> Button msg -> Button msg
withAttrs attrs =
    addOption (Attrs attrs)


withIcon : FontAwesome.Icon.Icon -> Button msg -> Button msg
withIcon icon =
    addOption (Icon icon [])


withStyledIcon : FontAwesome.Icon.Icon -> List (Svg.Attribute msg) -> Button msg -> Button msg
withStyledIcon icon svgAttrs =
    addOption (Icon icon svgAttrs)


withLabel : String -> Button msg -> Button msg
withLabel txt =
    addOption (Text txt)


toHtml : Button msg -> Html msg
toHtml (Button action options) =
    options
        |> List.foldr
            (\opt acc ->
                case opt of
                    Role role ->
                        { acc | role = role }

                    Icon icon svgAttrs ->
                        { acc | icon = Just ( icon, svgAttrs ) }

                    Text txt ->
                        { acc | text = Just txt }

                    Attrs attrs ->
                        { acc | attrs = attrs }
            )
            defaults
        |> (\config -> buttonHelp config action)


defaults : Config msg
defaults =
    { role = Plain, icon = Nothing, text = Nothing, attrs = [] }


button : msg -> Button msg
button action =
    Button action []


buttonHelp : Config msg -> msg -> Html msg
buttonHelp conf action =
    let
        btnKDDecoder msg =
            JD.lazy (\_ -> JD.oneOf [ Key.enter msg, Key.space msg ])
    in
    div
        ([ preventDefaultOn "click" <| JD.succeed ( action, True )
         , preventDefaultOn "keydown" <| btnKDDecoder ( action, True )
         , tabindex 0
         , class "pointer"
         ]
            ++ conf.attrs
        )
        [ conf.icon
            |> viewMaybe
                (\( icon, svgAttrs ) ->
                    icon
                        |> FontAwesome.Icon.viewStyled (Svg.Attributes.class "gray" :: svgAttrs)
                        |> H.fromUnstyled
                )
        , conf.text
            |> viewMaybe
                (\txt ->
                    div
                        [ class <|
                            case conf.role of
                                Plain ->
                                    ""

                                Primary ->
                                    "underline blue"

                                Secondary ->
                                    "underline gray"
                        ]
                        [ text txt ]
                )
        ]


textBtn : msg -> List (Attribute msg) -> String -> Html msg
textBtn =
    secondaryTxtBtn


secondaryTxtBtn : msg -> List (Attribute msg) -> String -> Html msg
secondaryTxtBtn action attrs txt =
    button action
        |> withLabel txt
        |> withRole Secondary
        |> withAttrs attrs
        |> toHtml


faBtn : msg -> FontAwesome.Icon.Icon -> List (Attribute msg) -> Html msg
faBtn action icon attrs =
    button action
        |> withRole Secondary
        |> withIcon icon
        |> withAttrs attrs
        |> toHtml
