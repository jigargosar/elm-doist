module MovePopup exposing (Model(..), firstFocusable, initial, view)

import BasicsExtra exposing (..)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (class, classList, tabindex)
import Html.Styled.Events exposing (onClick)
import MoveDialog
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = Closed
    | OpenFor TodoId ProjectId


initial : Model
initial =
    Closed


firstFocusable =
    "move-popup__first-focusable"


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject p =
    { id = p.id, title = p.title }


inboxDisplayProject =
    { id = ProjectId.default, title = "Inbox" }


toDisplayProjectList projectList =
    inboxDisplayProject :: List.map toDisplayProject projectList


view : msg -> List (Html msg) -> Html msg
view onOverlayClickOrEscapePressed content =
    div
        [ class "z-1 fixed absolute--fill flex items-center justify-center"
        , Key.onEscape onOverlayClickOrEscapePressed
        , tabindex -1
        ]
        [ div
            [ class "absolute absolute--fill bg-black-50"
            , onClick onOverlayClickOrEscapePressed
            ]
            []
        , div [ class "absolute" ]
            (let
                viewProjectItem idx dp =
                    TextButton.view (OpenMoveDialog todoId dp.id)
                        dp.title
                        [ class "ph3 pv2"
                        , classList [ ( "b", dp.id == projectId ) ]
                        , A.id <| ifElse (idx == 0) MoveDialog.firstFocusable ""
                        ]
             in
             viewDialog
                [ div [ class "bg-white pa3 lh-copy shadow-1" ]
                    (div [ class "b" ] [ text "Move To Project ..." ]
                        :: (projectList
                                |> toDisplayProjectList
                                |> List.indexedMap viewProjectItem
                           )
                    )
                ]
            )
        ]
