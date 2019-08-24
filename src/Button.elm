module Button exposing (btn, faBtn, primaryTxtBtn, secondaryTxtBtn, textBtn)

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
import Svg.Attributes


type Role
    = Primary
    | Secondary


type alias Options =
    { role : Role
    , icon : Maybe FontAwesome.Icon.Icon
    , text : Maybe String
    }


defaults : Options
defaults =
    { role = Primary, icon = Nothing, text = Nothing }


button : Options -> msg -> List (Attribute msg) -> Html msg
button options action attrs =
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
            ++ attrs
        )
        [ options.icon
            |> viewMaybe
                (\icon ->
                    icon
                        |> FontAwesome.Icon.viewStyled [ Svg.Attributes.class "gray" ]
                        |> H.fromUnstyled
                )
        , options.text
            |> viewMaybe
                (\txt ->
                    div
                        [ class "underline pa1"
                        , class <|
                            case options.role of
                                Primary ->
                                    "blue"

                                Secondary ->
                                    "gray"
                        ]
                        [ text txt ]
                )
        ]


btn : msg -> List (Attribute msg) -> List (Html msg) -> Html msg
btn action attrs =
    let
        btnKDDecoder msg =
            JD.lazy (\_ -> JD.oneOf [ Key.enter msg, Key.space msg ])
    in
    div
        ([ preventDefaultOn "click" <| JD.succeed ( action, True )
         , preventDefaultOn "keydown" <| btnKDDecoder ( action, True )
         , tabindex 0
         , class "dib pointer"
         ]
            ++ attrs
        )


textBtn : msg -> List (Attribute msg) -> String -> Html msg
textBtn action attrs txt =
    button { defaults | text = Just txt, role = Secondary } action attrs


primaryTxtBtn : msg -> List (Attribute msg) -> String -> Html msg
primaryTxtBtn action attrs txt =
    button { defaults | text = Just txt, role = Primary } action attrs


secondaryTxtBtn : msg -> List (Attribute msg) -> String -> Html msg
secondaryTxtBtn =
    textBtn


faBtn : msg -> FontAwesome.Icon.Icon -> List (Attribute msg) -> Html msg
faBtn action icon attrs =
    button { defaults | icon = Just icon } action attrs