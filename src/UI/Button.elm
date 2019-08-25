module UI.Button exposing
    ( Option(..)
    , Role(..)
    , button
    , toHtml
    , withAttrs
    , withLabel
    , withRole
    )

import Accessibility.Styled.Key as Key
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes
    exposing
        ( class
        , tabindex
        )
import Html.Styled.Events exposing (preventDefaultOn)
import HtmlStyledExtra exposing (viewMaybe)
import Json.Decode as JD exposing (Decoder)


type Role
    = Primary
    | Secondary
    | Plain


type alias Config msg =
    { role : Role
    , text : Maybe String
    , attrs : List (Attribute msg)
    }


type Option msg
    = Role Role
    | Label String
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


withLabel : String -> Button msg -> Button msg
withLabel txt =
    addOption (Label txt)


toHtml : Button msg -> Html msg
toHtml (Button action options) =
    configFromOptions options
        |> (\config -> buttonHelp action config)


configFromOptions : List (Option msg) -> Config msg
configFromOptions options =
    options
        |> List.foldr
            (\opt acc ->
                case opt of
                    Role role ->
                        { acc | role = role }

                    Label txt ->
                        { acc | text = Just txt }

                    Attrs attrs ->
                        { acc | attrs = attrs }
            )
            defaults


defaults : Config msg
defaults =
    { role = Plain, text = Nothing, attrs = [] }


button : msg -> Button msg
button action =
    Button action []


buttonHelp : msg -> Config msg -> Html msg
buttonHelp action conf =
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
        [ conf.text
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
