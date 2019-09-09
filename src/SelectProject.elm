module SelectProject exposing (Exit(..), Model, Msg, init, update, view)

import Browser.Dom as Dom
import Css
import Focus
import Html.Styled as H exposing (div, text)
import Html.Styled.Attributes as A exposing (tabindex)
import Html.Styled.Events as E exposing (onClick)
import HtmlExtra as HX
import Json.Decode as JD
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = SelectOpen
    | SelectClosed


type alias Internal =
    {}


init : ( Model, Cmd Msg )
init =
    ( SelectClosed, Cmd.none )


focusFirstCmd : Cmd Msg
focusFirstCmd =
    Focus.attempt Focused firstDomId



--map : (Internal -> Internal) -> Model -> Model
--map fn (Model internal) =
--    Model (fn internal)


type Msg
    = Selected ProjectId
    | Cancel
    | Focused Focus.FocusResult


type Exit
    = Closed (Maybe ProjectId)
    | DomError Dom.Error


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Exit )
update message model =
    case message of
        Selected projectId ->
            ( SelectClosed, Cmd.none, Just <| Closed <| Just projectId )

        Cancel ->
            ( SelectClosed, Cmd.none, Just <| Closed Nothing )

        Focused result ->
            ( model
            , Cmd.none
            , case result of
                Ok _ ->
                    Nothing

                Err domError ->
                    Just (DomError domError)
            )


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


view : ProjectId -> ProjectList -> Model -> H.Html Msg
view selectedProjectId projectList model =
    case model of
        SelectClosed ->
            HX.none

        SelectOpen ->
            selectContainer
                (inboxDisplayProject
                    :: List.map toDisplayProject projectList
                    |> List.indexedMap (viewListItem selectedProjectId)
                )


selectContainer =
    H.styled (H.node "track-focus-outside")
        []
        [ E.on "focusOutside" (JD.succeed Cancel)
        , Key.onEscape Cancel
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
    TextButton.styled
        [ Css.cursor Css.pointer, styles ]
        (Selected displayProject.id)
        displayProject.title
        [ A.id <|
            if isFirst then
                firstDomId

            else
                ""
        ]


firstDomId =
    "select-project__first-dom-id"
