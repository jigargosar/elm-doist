module TodoFormFields exposing (Model, getProjectId, getTitle, init, setProjectId, setTitle)

import ProjectId exposing (ProjectId)
import Todo


type Model
    = Model Internal


type alias Internal =
    { title : String
    , dueAt : Todo.DueAt
    , projectId : ProjectId
    }


init : String -> Todo.DueAt -> ProjectId -> Model
init title dueAt projectId =
    Model (Internal title dueAt projectId)


map : (Internal -> Internal) -> Model -> Model
map fn (Model internal) =
    Model (fn internal)


setProjectId : ProjectId -> Model -> Model
setProjectId projectId =
    map (\fields -> { fields | projectId = projectId })


setTitle : String -> Model -> Model
setTitle title =
    map (\fields -> { fields | title = title })


unwrap (Model internal) =
    internal


getTitle : Model -> String
getTitle =
    unwrap >> .title


getProjectId : Model -> ProjectId
getProjectId =
    unwrap >> .projectId
