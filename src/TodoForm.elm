module TodoForm exposing (Fields, Model, Msg, OutMsg(..), init, update, view)

import Accessibility.Styled exposing (text)
import Html.Styled as H exposing (div, textarea)
import Html.Styled.Attributes as A exposing (class, rows)
import Html.Styled.Events as E
import Json.Encode as JE
import Project exposing (ProjectList)
import ProjectId exposing (ProjectId)
import Return
import SelectProject
import Todo
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


init : String -> Todo.DueAt -> ProjectId -> Model
init title dueAt projectId =
    Model
        { fields = Fields title dueAt projectId
        , selectProject = SelectProject.init
        }


map : (Internal -> Internal) -> Model -> Model
map fn (Model internal) =
    Model (fn internal)


setFields : Fields -> Model -> Model
setFields fields =
    map (\internal -> { internal | fields = fields })


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


setMaybeProjectId : Maybe ProjectId -> Model -> Model
setMaybeProjectId maybeProjectId model =
    case maybeProjectId of
        Nothing ->
            model

        Just projectId ->
            setProjectId projectId model


unwrap (Model internal) =
    internal


getFields =
    unwrap >> .fields


getTitle =
    getFields >> .title


getProjectId : Model -> ProjectId
getProjectId =
    getFields >> .projectId


type Msg
    = FieldsChanged Fields
    | SaveClicked
    | CancelClicked
    | SelectProjectMsg SelectProject.Msg
    | TitleChanged String


type OutMsg
    = Submit Fields
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message model =
    case message of
        FieldsChanged fields ->
            ( setFields fields model, Cmd.none, Nothing )

        TitleChanged title ->
            ( setTitle title model, Cmd.none, Nothing )

        SaveClicked ->
            ( model, Cmd.none, Just <| Submit (getFields model) )

        CancelClicked ->
            ( model, Cmd.none, Nothing )

        SelectProjectMsg msg ->
            let
                ( newSelectProject, cmd, maybeProjectId ) =
                    SelectProject.update msg (getSelectProject model)
            in
            ( setSelectProject newSelectProject model
                |> setMaybeProjectId maybeProjectId
            , Cmd.map SelectProjectMsg cmd
            , Nothing
            )


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
