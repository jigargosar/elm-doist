module Fire exposing (addProject, addTodo, deleteProject, deleteTodo, updateTodo)

import Json.Encode as JE
import Ports
import ProjectId
import TodoId exposing (TodoId)


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
