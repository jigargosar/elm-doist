module SelectProject exposing (Model, Msg, init, update, view)

import BasicsExtra exposing (eq_)
import Browser.Dom as Dom
import Css
import Focus
import Html.Styled as H exposing (div, text)
import Html.Styled.Attributes as A exposing (css, tabindex)
import Html.Styled.Events as E exposing (onClick)
import HtmlExtra as HX
import Json.Decode as JD
import List.Extra as LX
import Maybe.Extra as MX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = SelectOpen
    | SelectClosed


type alias Internal =
    {}


init : Model
init =
    SelectClosed


focusFirstCmd : Cmd Msg
focusFirstCmd =
    Focus.attempt Focused firstDomId



--map : (Internal -> Internal) -> Model -> Model
--map fn (Model internal) =
--    Model (fn internal)


type Msg
    = OpenMenu
    | Selected ProjectId
    | CloseMenu
    | Focused Focus.FocusResult


update : Msg -> Model -> ( Model, Cmd Msg, Maybe ProjectId )
update message model =
    case message of
        OpenMenu ->
            ( SelectOpen, focusFirstCmd, Nothing )

        CloseMenu ->
            ( SelectClosed, Cmd.none, Nothing )

        Selected projectId ->
            ( SelectClosed, Cmd.none, Just projectId )

        Focused _ ->
            ( model, Cmd.none, Nothing )


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


getDisplayProjectTitle : ProjectId -> List DisplayProject -> String
getDisplayProjectTitle projectId projectList =
    LX.find (.id >> eq_ projectId) projectList
        |> MX.unwrap "<Unknown Project>" .title


view : ProjectId -> ProjectList -> Model -> H.Html Msg
view selectedProjectId projectList model =
    let
        displayProjectList =
            inboxDisplayProject
                :: List.map toDisplayProject projectList
    in
    case model of
        SelectClosed ->
            div [ E.onClick OpenMenu ]
                [ text "project: "
                , text (getDisplayProjectTitle selectedProjectId displayProjectList)
                ]

        SelectOpen ->
            selectContainer
                (displayProjectList
                    |> List.indexedMap (viewListItem selectedProjectId)
                )


selectContainer =
    H.styled (H.node "track-focus-outside")
        []
        [ E.on "focusOutside" (JD.succeed CloseMenu)
        , Key.onEscape CloseMenu
        , tabindex -1
        ]


viewListItem : ProjectId -> Int -> DisplayProject -> H.Html Msg
viewListItem selectedProjectId idx displayProject =
    viewListItemHelp
        { isSelected = selectedProjectId == displayProject.id
        , isFirst = idx == 0
        }
        displayProject


viewListItemHelp : { isSelected : Bool, isFirst : Bool } -> DisplayProject -> H.Html Msg
viewListItemHelp { isSelected, isFirst } displayProject =
    let
        styles =
            if isSelected then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []
    in
    TextButton.view2
        [ HX.idIf isFirst (always firstDomId)
        , css [ Css.cursor Css.pointer, styles ]
        ]
        (Selected displayProject.id)
        displayProject.title


firstDomId =
    "select-project__first-dom-id"
