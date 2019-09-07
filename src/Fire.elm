module Fire exposing
    ( addProject
    , addTodo
    , cleanupTodoList
    , deleteProject
    , deleteTodo
    , patchTodo
    , queryProjectList
    , queryTodoList
    )

import Dict exposing (Dict)
import Dict.Extra
import Json.Encode as JE
import Maybe.Extra as MX
import Ports
import Project exposing (Project)
import ProjectId
import Time
import Todo exposing (Todo)
import TodoId exposing (TodoId)


queryTodoList : Cmd msg
queryTodoList =
    Ports.queryFirestore
        { id = "todoList"
        , userCollectionName = "todos"
        , whereClause = []
        }


queryProjectList : Cmd msg
queryProjectList =
    Ports.queryFirestore
        { id = "projectList"
        , userCollectionName = "projects"
        , whereClause = []
        }


deleteTodo : TodoId -> Cmd msg
deleteTodo todoId =
    Ports.deleteFirestoreDoc
        { userDocPath = "todos/" ++ TodoId.toString todoId }


deleteProject : ProjectId.ProjectId -> Cmd msg
deleteProject projectId =
    Ports.updateFirestoreDoc
        { userDocPath =
            "projects/" ++ ProjectId.toString projectId
        , data =
            JE.object
                [ ( "deleted", JE.bool True )
                ]
        }


updateTodo : TodoId -> List ( String, JE.Value ) -> Cmd msg
updateTodo todoId kvPairs =
    Ports.updateFirestoreDoc
        { userDocPath = "todos/" ++ TodoId.toString todoId
        , data = JE.object kvPairs
        }


patchTodo : Time.Posix -> TodoId -> List Todo.Msg -> Cmd msg
patchTodo now todoId todoMsgList =
    if List.isEmpty todoMsgList then
        Cmd.none

    else
        updateTodo todoId (Todo.patch todoMsgList now)


addTodo : JE.Value -> Cmd msg
addTodo encoded =
    Ports.addFirestoreDoc
        { userCollectionName = "todos"
        , data = encoded
        }


addProject : JE.Value -> Cmd msg
addProject encoded =
    Ports.addFirestoreDoc
        { userCollectionName = "projects"
        , data = encoded
        }


cleanupTodoList :
    { a
        | todoList : List Todo
        , projectList : List Project
    }
    -> Cmd msg
cleanupTodoList { todoList, projectList } =
    let
        todoByPid : Dict String (List Todo)
        todoByPid =
            Dict.Extra.groupBy
                (.projectId >> ProjectId.toString)
                todoList

        deleteProjectsCmd =
            projectList
                |> List.filter .deleted
                |> List.filter
                    (\p ->
                        Dict.get
                            (ProjectId.toString p.id)
                            todoByPid
                            |> MX.unwrap True List.isEmpty
                    )
                |> List.map
                    (.id
                        >> (\projectId ->
                                Ports.deleteFirestoreDoc
                                    { userDocPath =
                                        "projects/"
                                            ++ ProjectId.toString
                                                projectId
                                    }
                           )
                    )
                |> Cmd.batch

        deleteTodosCmd : Cmd msg
        deleteTodosCmd =
            projectList
                |> List.filter .deleted
                |> List.filterMap
                    (\p ->
                        Dict.get
                            (ProjectId.toString p.id)
                            todoByPid
                    )
                |> List.concat
                |> List.map
                    (.id
                        >> (\todoId ->
                                Ports.deleteFirestoreDoc
                                    { userDocPath =
                                        "todos/"
                                            ++ TodoId.toString todoId
                                    }
                           )
                    )
                |> Cmd.batch
    in
    Cmd.batch [ deleteTodosCmd, deleteProjectsCmd ]
