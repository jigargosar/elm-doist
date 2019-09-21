module SelectProject exposing (Model, Msg, init, update, view)

import BasicsExtra exposing (eq_)
import Css
import Focus
import Html.Styled as H exposing (div, text)
import Html.Styled.Attributes exposing (class, css, tabindex)
import Html.Styled.Events as E
import HtmlExtra as HX
import Json.Decode as JD
import List.Extra as LX
import Maybe.Extra as MX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Task
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = MenuOpen
    | MenuClosed


type alias Internal =
    {}


init : Model
init =
    MenuClosed


focusFirstCmd : Cmd Msg
focusFirstCmd =
    Focus.attempt Focused firstDomId


type Msg
    = OpenMenu
    | Selected ProjectId
    | CloseMenu
    | Focused Focus.FocusResult


update : { toMsg : Msg -> msg, onSelect : ProjectId -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        OpenMenu ->
            ( MenuOpen, focusFirstCmd |> Cmd.map config.toMsg )

        CloseMenu ->
            ( MenuClosed, Cmd.none )

        Selected projectId ->
            ( MenuClosed, config.onSelect projectId |> perform )

        Focused _ ->
            ( model, Cmd.none )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


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

        currentProjectTitle =
            getDisplayProjectTitle selectedProjectId displayProjectList

        open =
            case model of
                MenuClosed ->
                    False

                MenuOpen ->
                    True
    in
    div [ class "relative" ]
        [ div [ E.onClick OpenMenu ]
            [ text "project: "
            , text currentProjectTitle
            ]
        , if open then
            popupContainer
                (displayProjectList
                    |> List.indexedMap (viewItem selectedProjectId)
                )

          else
            text ""
        ]


popupContainer =
    H.styled (H.node "track-focus-outside")
        []
        [ class "absolute top-1 shadow-1 bg-white"
        , E.on "focusOutside" (JD.succeed CloseMenu)
        , Key.onEscape CloseMenu
        , tabindex -1
        ]


viewItem : ProjectId -> Int -> DisplayProject -> H.Html Msg
viewItem selectedProjectId idx displayProject =
    viewItemHelp
        { isSelected = selectedProjectId == displayProject.id
        , isFirst = idx == 0
        }
        displayProject


viewItemHelp : { isSelected : Bool, isFirst : Bool } -> DisplayProject -> H.Html Msg
viewItemHelp { isSelected, isFirst } displayProject =
    let
        styles =
            if isSelected then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []
    in
    TextButton.view
        [ HX.idIf isFirst (always firstDomId)
        , css [ styles ]
        ]
        (Selected displayProject.id)
        displayProject.title


firstDomId =
    "select-project__first-dom-id"
