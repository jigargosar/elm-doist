module ModalDemo exposing (..)

import Accessibility.Styled exposing (Html, button, div, text)
import Accessibility.Styled.Modal as Modal exposing (Model)
import Html.Styled
import Html.Styled.Attributes exposing (style)
import Html.Styled.Events exposing (onClick)


view wrapMsg m =
    view_ wrapMsg m
        |> Accessibility.Styled.toUnstyled
        |> Html.Styled.fromUnstyled


view_ wrapMsg modal =
    Modal.view
        { overlayColor = "rgba(128, 0, 128, 0.7)"
        , wrapMsg = wrapMsg
        , modalAttributes =
            [ style "background-color" "white"
            , style "border-radius" "4px"
            , style "border" "2px solid purple"
            , style "margin" "40px auto"
            , style "padding" "20px"
            , style "max-width" "600px"
            , style "min-height" "40vh"
            ]
        , title = ( "Intro Modal", [] )
        , content =
            \{ onlyFocusableElement } ->
                div
                    [ style "display" "flex"
                    ]
                    [ text "Welcome to this modal! I'm so happy to have you here with me."
                    , button
                        (onClick (Modal.close |> wrapMsg) :: onlyFocusableElement)
                        [ text "Close intro modal" ]
                    ]
        }
        modal
