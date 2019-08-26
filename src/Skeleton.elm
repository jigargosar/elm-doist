module Skeleton exposing (view)

import Css exposing (bottom, fixed, height, marginLeft, marginTop, maxWidth, paddingTop, position, px, rem, top, transforms, translateX, width, zero)
import Css.Media as Media exposing (withMedia)
import Css.Transitions as Transition exposing (transition)
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes
    exposing
        ( class
        , css
        )


view :
    { title : String
    , header : Html msg
    , sidebar : Html msg
    , content : List (Html msg)
    , footer : Html msg
    }
    -> { title : String, body : List (Html msg) }
view config =
    let
        sidebarWidthNum =
            250

        headerHeight =
            rem 2

        maxContentWidth =
            px 900

        bpSmall =
            600

        sm =
            withMedia
                [ Media.all [ Media.maxWidth <| px bpSmall ] ]

        ns =
            withMedia
                [ Media.all [ Media.minWidth <| px (bpSmall + 1) ] ]
    in
    { title = config.title
    , body =
        [ div
            [ class "bg-black white w-100"
            , css [ position fixed, height headerHeight ]
            ]
            [ div
                [ class "center", css [ maxWidth maxContentWidth ] ]
                [ config.header ]
            ]
        , div [ class "center", css [ maxWidth maxContentWidth, paddingTop headerHeight ] ]
            [ div
                [ class "fixed overflow-auto ph3"
                , css
                    [ width (px sidebarWidthNum)
                    , top headerHeight
                    , bottom zero
                    , sm
                        [ transforms [ translateX <| px -sidebarWidthNum ] ]
                    , transition [ Transition.transform 150 ]
                    ]
                ]
                [ config.sidebar
                ]
            , div
                [ class "ph3"
                , css
                    [ marginLeft zero
                    , ns [ marginLeft <| px sidebarWidthNum ]
                    , transition [ Transition.marginLeft 150 ]
                    ]
                ]
                config.content
            ]
        , config.footer
        ]
    }
