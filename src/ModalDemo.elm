module ModalDemo exposing (..)

import Accessibility exposing (Html, button, div, text)
import Accessibility.Modal as Modal
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


view =
    Modal.view
        { overlayColor = "rgba(128, 0, 128, 0.7)"
        , wrapMsg = identity
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
                        (onClick Modal.close :: onlyFocusableElement)
                        [ text "Close intro modal" ]
                    ]
        }
