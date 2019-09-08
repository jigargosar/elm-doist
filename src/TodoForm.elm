module TodoForm exposing (Model, init)

import ProjectId exposing (ProjectId)
import Todo


type Model
    = Model Internal


type alias Internal =
    { fields : Fields
    , maybeEditor : Maybe Editor
    }


type Editor
    = SelectProject
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
