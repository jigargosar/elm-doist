module MovePopup exposing (view)

import BasicsExtra exposing (eq_)
import Css
import Focus
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import List.Extra as LX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import UI.Key as Key
import UI.TextButton as TextButton


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject { id, title } =
    { id = id, title = title }


inboxDisplayProject : DisplayProject
inboxDisplayProject =
    { id = ProjectId.default, title = "Inbox" }


type Reason
    = LostFocus
    | Canceled
    | Selected ProjectId


type alias Config msg =
    { rootId : String
    , closed : Reason -> msg
    }


view : Config msg -> ProjectId -> ProjectList -> Html msg
view config selectedProjectId projectList =
    let
        items =
            inboxDisplayProject
                :: List.map toDisplayProject projectList

        selectedItem =
            LX.find (.id >> eq_ selectedProjectId) items
                |> Maybe.withDefault inboxDisplayProject

        firstItem =
            List.head items |> Maybe.withDefault selectedItem

        selectedItemStyle item =
            if item == selectedItem then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []

        attrsForItem item =
            [ Focus.dataAutoFocus (item == firstItem)
            , css [ selectedItemStyle item ]
            ]

        viewDisplayProject item =
            viewItem (attrsForItem item)
                (Selected item.id |> config.closed)
                item.title
    in
    Focus.focusTracker
        [ A.id config.rootId
        , class "absolute top-1 left--1 shadow-1 bg-white"
        , Key.onEscape (config.closed Canceled)
        , Focus.onFocusLost (config.closed LostFocus)
        , tabindex -1
        ]
        (List.map viewDisplayProject items)


viewItem : List (H.Attribute msg) -> msg -> String -> Html msg
viewItem attrs action title =
    TextButton.view (class "ph2 pv1" :: attrs)
        action
        title
