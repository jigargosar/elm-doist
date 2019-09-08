module TodoForm exposing (Model, Msg, init, update, view)

import Accessibility.Styled exposing (text)
import Html.Styled as H exposing (div, textarea)
import Html.Styled.Attributes as A exposing (class, rows)
import Html.Styled.Events as E
import HtmlExtra as HX
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
    , maybeEditor : Maybe Editor
    }


type Editor
    = SelectProject SelectProject.Model
    | SelectDueDate


type alias Fields =
    { title : String
    , dueAt : Todo.DueAt
    , projectId : ProjectId
    }


init : String -> Todo.DueAt -> ProjectId -> Model
init title dueAt projectId =
    Model
        { fields = Fields title dueAt projectId
        , maybeEditor = Nothing
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


setEditor : Editor -> Model -> Model
setEditor editor =
    map (\internal -> { internal | maybeEditor = Just editor })


closeEditor : Model -> Model
closeEditor =
    map (\internal -> { internal | maybeEditor = Nothing })


type Msg
    = FieldsChanged Fields
    | OpenEditor Editor
    | CloseEditor (Maybe Fields)
    | Close (Maybe Fields)
    | OnSelectProjectMsg SelectProject.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FieldsChanged fields ->
            ( setFields fields model, Cmd.none )

        OpenEditor editor ->
            ( setEditor editor model, Cmd.none )

        CloseEditor maybeFields ->
            case maybeFields of
                Just fields ->
                    ( model
                        |> setFields fields
                        |> closeEditor
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Close maybeFields ->
            ( model, Cmd.none )

        OnSelectProjectMsg msg ->
            case getEditor model of
                Just (SelectProject spm) ->
                    let
                        ( newSP, newSPCmd, spMaybeExit ) =
                            SelectProject.update msg spm
                    in
                    ( setEditor (SelectProject newSP) model
                    , Cmd.map OnSelectProjectMsg newSPCmd
                    )
                        |> Return.andThen
                            (handleSelectProjectExitMsg spMaybeExit)

                _ ->
                    ( model, Cmd.none )


handleSelectProjectExitMsg : Maybe SelectProject.Exit -> Model -> ( Model, Cmd Msg )
handleSelectProjectExitMsg maybeExit model =
    case maybeExit of
        Nothing ->
            ( model, Cmd.none )

        Just (SelectProject.Closed (Just projectId)) ->
            ( closeEditor model
                |> mapFields (\fields -> { fields | projectId = projectId })
            , Cmd.none
            )

        Just (SelectProject.Closed Nothing) ->
            ( closeEditor model
            , Cmd.none
            )


unwrap (Model internal) =
    internal


getFields =
    unwrap >> .fields


getTitle =
    getFields >> .title


getEditor : Model -> Maybe Editor
getEditor =
    unwrap >> .maybeEditor


getProjectId : Model -> ProjectId
getProjectId =
    getFields >> .projectId


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

                        --                        , E.onInput titleChangedMsg
                        ]
                        []
                    ]
                ]
            , div [] [ text "schedule" ]
            ]
        , div
            [ E.onClick
                (OpenEditor
                    (SelectProject <| SelectProject.init (getProjectId model))
                )
            ]
            [ text "select project" ]
        , case getEditor model of
            Nothing ->
                HX.none

            Just SelectDueDate ->
                HX.none

            Just (SelectProject spm) ->
                SelectProject.view projectList spm
                    |> H.map OnSelectProjectMsg
        , div [ class "flex hs3 lh-copy" ]
            [ TextButton.primary (Close (Just <| getFields model)) "Save" []
            , TextButton.primary (Close Nothing) "Cancel" []

            --            , case config.delete of
            --                Nothing ->
            --                    HX.none
            --
            --                Just del ->
            --                    TextButton.primary del "Delete" []
            ]
        ]
