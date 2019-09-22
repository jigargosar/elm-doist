module TodoForm exposing
    ( Fields
    , Model
    , Msg
    , firstFocusable
    , fromParts
    , fromProjectId
    , fromTodo
    , getFields
    , update
    , view
    )

import Accessibility.Styled exposing (text)
import BasicsExtra exposing (eq_)
import Html.Styled as H exposing (div, textarea)
import Html.Styled.Attributes as A exposing (class, rows)
import Html.Styled.Events as E
import Json.Encode as JE
import List.Extra as LX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import SelectInput
import Task
import Todo exposing (Todo)
import UI.TextButton as TextButton


type Model
    = Model Internal


type alias Internal =
    { fields : Fields
    , selectProject : SelectInput.Model
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
        , selectProject = SelectInput.init
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


setSelectProject : SelectInput.Model -> Model -> Model
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
    | SelectProjectMsg (SelectInput.Msg DisplayProject)
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
                    SelectInput.update selectProjectConfig
                        msg
                        (getSelectProject model)
            in
            ( setSelectProject newSelectProject model
            , cmd |> Cmd.map config.toMsg
            )

        SetProjectId projectId ->
            ( setProjectId projectId model, Cmd.none )


selectProjectConfig : SelectInput.Config Msg DisplayProject
selectProjectConfig =
    { id = "project"
    , itemLabel = .title
    , toMsg = SelectProjectMsg
    , onSelect = .id >> SetProjectId
    }


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


firstFocusable : String
firstFocusable =
    "todo-form__first-focusable"


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
                        , A.id firstFocusable
                        ]
                        []
                    ]
                ]
            , div [] [ text "schedule" ]
            ]
        , viewSelectProjectInput (getProjectId model) projectList (getSelectProject model)
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


viewSelectProjectInput : ProjectId -> ProjectList -> SelectInput.Model -> H.Html Msg
viewSelectProjectInput selectedProjectId projectList =
    let
        displayList =
            inboxDisplayProject
                :: List.map toDisplayProject projectList

        selected =
            LX.find (.id >> eq_ selectedProjectId) displayList
                |> Maybe.withDefault inboxDisplayProject
    in
    SelectInput.view selectProjectConfig selected displayList
