module MovePopup exposing (Model(..), firstFocusable, initial, view)

import BasicsExtra exposing (..)
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (class, classList, tabindex)
import Html.Styled.Events exposing (on, onClick)
import HtmlExtra as HX
import Json.Decode as JD
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


type alias ViewConfig msg =
    { close : msg, move : TodoId -> ProjectId -> msg }


view : ViewConfig msg -> TodoId -> List Project -> Model -> Html msg
view config todoId projectList model =
    case model of
        OpenFor todoId_ projectId ->
            if todoId /= todoId_ then
                HX.none

            else
                viewHelp config todoId projectId projectList

        Closed ->
            HX.none


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


viewHelp : ViewConfig msg -> TodoId -> ProjectId -> List Project -> Html msg
viewHelp config todoId projectId projectList =
    let
        viewProjectItem idx dp =
            TextButton.view (config.move todoId dp.id)
                dp.title
                [ class "ph3 pv2"
                , classList [ ( "b", dp.id == projectId ) ]
                , A.id <| ifElse (idx == 0) firstFocusable ""
                ]
    in
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , tabindex -1
        , on "focusOutside" (JD.succeed config.close)
        , Key.onEscape config.close
        ]
        [ div [ class "bg-white pa3 lh-copy shadow-1" ]
            [ div [ class "bg-white pa3 lh-copy shadow-1" ]
                (div [ class "b" ] [ text "Move To Project ..." ]
                    :: (projectList
                            |> toDisplayProjectList
                            |> List.indexedMap viewProjectItem
                       )
                )
            ]
        ]
