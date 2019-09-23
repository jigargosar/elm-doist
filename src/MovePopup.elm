module MovePopup exposing (ViewConfig, firstFocusable, view)

import BasicsExtra exposing (..)
import Focus
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (class, classList, tabindex)
import Html.Styled.Events exposing (on)
import Json.Decode as JD
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


firstFocusable =
    "move-popup__first-focusable"


type alias ViewConfig msg =
    { close : msg, move : ProjectId -> msg }


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject p =
    { id = p.id, title = p.title }


inboxDisplayProject =
    { id = ProjectId.default, title = "Inbox" }


toDisplayProjectList : List Project -> List DisplayProject
toDisplayProjectList projectList =
    inboxDisplayProject :: List.map toDisplayProject projectList


view : ViewConfig msg -> ProjectId -> List Project -> Html msg
view config projectId projectList =
    let
        viewProjectItem idx dp =
            TextButton.view_ (config.move dp.id)
                dp.title
                [ class "ph3 pv2"
                , classList [ ( "b", dp.id == projectId ) ]
                , A.id <| ifElse (idx == 0) firstFocusable ""
                ]
    in
    Focus.focusTracker
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , tabindex -1
        , Focus.onFocusOutside config.close
        , Key.onEscape config.close
        ]
        [ div [ class "pa3 lh-copy" ]
            (div [ class "b" ] [ text "Move To Project ..." ]
                :: (projectList
                        |> toDisplayProjectList
                        |> List.indexedMap viewProjectItem
                   )
            )
        ]
