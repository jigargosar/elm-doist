module ModalDemo exposing (..)

import Accessibility.Styled exposing (Html, button, div, text)
import Accessibility.Styled.Modal as Modal exposing (Model, Msg)
import Html.Styled.Attributes exposing (class, style)


view : (Msg -> msg) -> Model -> Html msg
view wrapMsg =
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
            \{ firstFocusableElement, lastFocusableElement } ->
                div [ class "vs3" ]
                    [ div []
                        [ text "Welcome to this modal! I'm so happy to have you here with me."
                        ]
                    , div [ class "flex hs3" ]
                        [ button firstFocusableElement [ text "fst btn" ]
                        , button
                            (Modal.closeOnClick wrapMsg)
                            [ text "Close intro modal" ]
                        , button lastFocusableElement [ text "lst btn" ]
                        ]
                    ]
        }