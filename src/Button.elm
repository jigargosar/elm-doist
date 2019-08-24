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
import Json.Decode as JD exposing (Decoder)


type Role
    = Primary
    | Secondary


type alias Options =
    { role : Role
    , icon : Maybe FontAwesome.Icon.Icon
    }


button options msg text =
    1


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
         , class "pointer"
         ]
            ++ attrs
        )


textBtn : msg -> List (Attribute msg) -> String -> Html msg
textBtn action attrs txt =
    btn action
        (class "underline pa1" :: attrs)
        [ text txt ]


primaryTxtBtn : msg -> List (Attribute msg) -> String -> Html msg
primaryTxtBtn action attrs txt =
    textBtn action
        (class "blue" :: attrs)
        txt


secondaryTxtBtn : msg -> List (Attribute msg) -> String -> Html msg
secondaryTxtBtn action attrs txt =
    textBtn action
        (class "gray" :: attrs)
        txt


faBtn : msg -> FontAwesome.Icon.Icon -> List (Attribute msg) -> Html msg
faBtn action icon attrs =
    btn action
        ([ class "dib gray hover-dark-gray_"
         ]
            ++ attrs
        )
        [ icon
            |> FontAwesome.Icon.viewStyled [{- FontAwesome.Attributes.sm -}]
            |> H.fromUnstyled
        ]
