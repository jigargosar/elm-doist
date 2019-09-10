module TodoForm exposing (Fields, Model, Msg, fromParts, fromProjectId, fromTodo, getFields, update, view)

import Accessibility.Styled exposing (text)
import Html.Styled as H exposing (div, textarea)
import Html.Styled.Attributes as A exposing (class, rows)
import Html.Styled.Events as E
import Json.Encode as JE
import Project exposing (ProjectList)
import ProjectId exposing (ProjectId)
import SelectProject
import Task
import Todo exposing (Todo)
import UI.TextButton as TextButton


type Model
    = Model Internal


type alias Internal =
    { fields : Fields
    , selectProject : SelectProject.Model
    }


type alias Fields =
    { title : String
    , dueAt : Todo.DueAt
    , projectId : ProjectId
    }


fromParts : String -> Todo.DueAt -> ProjectId -> Model
fromParts title dueAt projectId =
    Model
        { fields = Fields title dueAt projectId
        , selectProject = SelectProject.init
        }


fromProjectId : ProjectId -> Model
fromProjectId projectId =
    fromParts "" Todo.notDue projectId


fromTodo : Todo -> Model
fromTodo { title, dueAt, projectId } =
    fromParts title dueAt projectId


map : (Internal -> Internal) -> Model -> Model
map fn (Model internal) =
    Model (fn internal)


mapFields : (Fields -> Fields) -> Model -> Model
mapFields fn =
    map (\internal -> { internal | fields = fn internal.fields })


setSelectProject : SelectProject.Model -> Model -> Model
setSelectProject selectProject =
    map (\internal -> { internal | selectProject = selectProject })


getSelectProject =
    unwrap >> .selectProject


setProjectId : ProjectId -> Model -> Model
setProjectId projectId =
    mapFields (\fields -> { fields | projectId = projectId })


setTitle : String -> Model -> Model
setTitle title =
    mapFields (\fields -> { fields | title = title })


unwrap (Model internal) =
    internal


getFields : Model -> Fields
getFields =
    unwrap >> .fields


getTitle =
    getFields >> .title


getProjectId : Model -> ProjectId
getProjectId =
    getFields >> .projectId


type Msg
    = SaveClicked
    | CancelClicked
    | SelectProjectMsg SelectProject.Msg
    | SetProjectId ProjectId
    | TitleChanged String


update :
    { toMsg : Msg -> msg, onSave : Fields -> msg, onCancel : msg }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config message model =
    case message of
        TitleChanged title ->
            ( setTitle title model, Cmd.none )

        SaveClicked ->
            ( model, config.onSave (getFields model) |> perform )

        CancelClicked ->
            ( model, config.onCancel |> perform )

        SelectProjectMsg msg ->
            let
                ( newSelectProject, cmd ) =
                    SelectProject.update { toMsg = SelectProjectMsg, onSelect = SetProjectId }
                        msg
                        (getSelectProject model)
            in
            ( setSelectProject newSelectProject model
            , cmd |> Cmd.map config.toMsg
            )

        SetProjectId projectId ->
            ( setProjectId projectId model, Cmd.none )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


view : ProjectList -> Model -> H.Html Msg
view projectList model =
    div [ class "pa3" ]
        [ div [ class "flex" ]
            [ div [ class "flex-grow-1" ]
                [ H.node "auto-resize-textarea"
                    [ A.property "textAreaValue" (JE.string (getTitle model)) ]
                    [ textarea
                        [ class "pa0 lh-copy overflow-hidden w-100"
                        , rows 1
                        , E.onInput TitleChanged
                        ]
                        []
                    ]
                ]
            , div [] [ text "schedule" ]
            ]
        , SelectProject.view (getProjectId model) projectList (getSelectProject model)
            |> H.map SelectProjectMsg
        , div [ class "flex hs3 lh-copy" ]
            [ TextButton.primary SaveClicked "Save" []
            , TextButton.primary CancelClicked "Cancel" []

            --            , case config.delete of
            --                Nothing ->
            --                    HX.none
            --
            --                Just del ->
            --                    TextButton.primary del "Delete" []
            ]
        ]
