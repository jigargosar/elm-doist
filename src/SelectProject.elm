port module SelectProject exposing (Model, Msg, init, update, view)

import BasicsExtra exposing (eq_)
import Focus
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A
import ListZipper as LZ
import Maybe.Extra as MX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import SelectInput
import Task


port focusSelector : String -> Cmd msg


type Model
    = IsOpen Bool


type alias Internal =
    {}


init : Model
init =
    IsOpen False


focusFirstCmd : Cmd Msg
focusFirstCmd =
    Focus.attempt Focused ""


type Msg
    = OpenPopup
    | Selected ProjectId
    | ClosePopup
    | Focused Focus.FocusResult


update : { toMsg : Msg -> msg, onSelect : ProjectId -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        OpenPopup ->
            ( IsOpen True
            , Cmd.batch
                [ focusFirstCmd |> Cmd.map config.toMsg
                , focusSelector ("#" ++ selectProjectInputId ++ " [autofocus=true]")
                ]
            )

        ClosePopup ->
            ( IsOpen False, Cmd.none )

        Selected projectId ->
            ( IsOpen False, config.onSelect projectId |> perform )

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


view : ProjectId -> ProjectList -> Model -> H.Html Msg
view selectedProjectId projectList model =
    let
        displayList =
            inboxDisplayProject
                :: List.map toDisplayProject projectList

        open =
            case model of
                IsOpen bool ->
                    bool

        pivot =
            LZ.zipperFromListFocusedBy (.id >> eq_ selectedProjectId) displayList
                |> MX.unpack (\_ -> LZ.zipperFromCons inboxDisplayProject (List.drop 1 displayList))
                    identity
    in
    SelectInput.view
        { attrs = [ A.id selectProjectInputId ]
        , itemLabel = .title
        , onClose = ClosePopup
        , onOpen = OpenPopup
        , onSelect = \{ id } -> Selected id
        }
        { open = open, items = pivot }


selectProjectInputId =
    "select-project-input"
