module SelectProject exposing (Exit(..), Model, Msg, init, update, view)

import Css
import Html.Styled as H exposing (div, text)
import Html.Styled.Attributes exposing (tabindex)
import Html.Styled.Events as E exposing (onClick)
import Json.Decode as JD
import Project exposing (ProjectList)
import ProjectId exposing (ProjectId)
import UI.Key as Key


type Model
    = Model Internal


type alias Internal =
    { projectId : ProjectId }


init : ProjectId -> Model
init projectId =
    Model (Internal projectId)


map : (Internal -> Internal) -> Model -> Model
map fn (Model internal) =
    Model (fn internal)


type Msg
    = Selected_ ProjectId
    | Cancel_


type Exit
    = Closed (Maybe ProjectId)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Exit )
update message model =
    case message of
        Selected_ projectId ->
            ( model, Cmd.none, Just <| Closed <| Just projectId )

        Cancel_ ->
            ( model, Cmd.none, Just <| Closed Nothing )


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
        [ E.on "focusOutside" (JD.succeed Cancel_)
        , Key.onEscape Cancel_
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
    H.styled div
        [ Css.cursor Css.pointer, styles ]
        [ tabindex 0
        , onClick (Selected_ projectId)
        ]
        [ text projectTitle ]
