module SelectProject exposing (Exit(..), Model, Msg, init, update, view)

import Browser.Dom as Dom
import Css
import Focus
import Html.Styled as H exposing (div, text)
import Html.Styled.Attributes exposing (tabindex)
import Html.Styled.Events as E exposing (onClick)
import Json.Decode as JD
import Project exposing (ProjectList)
import ProjectId exposing (ProjectId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = Model Internal


type alias Internal =
    { projectId : ProjectId }


init : ProjectId -> ( Model, Cmd Msg )
init projectId =
    ( Model (Internal projectId), Focus.attempt Focused "" )


map : (Internal -> Internal) -> Model -> Model
map fn (Model internal) =
    Model (fn internal)


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
            ( model, Cmd.none, Just <| Closed <| Just projectId )

        Cancel ->
            ( model, Cmd.none, Just <| Closed Nothing )

        Focused result ->
            ( model
            , Cmd.none
            , case result of
                Ok _ ->
                    Nothing

                Err domError ->
                    Just (DomError domError)
            )


getProjectId (Model { projectId }) =
    projectId


view : ProjectList -> Model -> H.Html Msg
view projectList model =
    let
        initialProjectId =
            getProjectId model

        viewProjectItem { id, title } =
            viewListItem initialProjectId id title

        viewInboxItem =
            viewListItem initialProjectId ProjectId.default "Inbox"
    in
    H.styled (H.node "track-focus-outside")
        []
        [ E.on "focusOutside" (JD.succeed Cancel)
        , Key.onEscape Cancel
        , tabindex -1
        ]
        (viewInboxItem :: List.map viewProjectItem projectList)


viewListItem : ProjectId -> ProjectId -> String -> H.Html Msg
viewListItem initialProjectId projectId projectTitle =
    let
        styles =
            if projectId == initialProjectId then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []
    in
    TextButton.styled
        [ Css.cursor Css.pointer, styles ]
        (Selected projectId)
        projectTitle
        []
