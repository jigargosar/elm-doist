module Fire exposing (deleteTodo)

import Ports
import TodoId exposing (TodoId)


deleteTodo : TodoId -> Cmd msg
deleteTodo todoId =
    Ports.deleteFirestoreDoc
        { userDocPath = "todos/" ++ TodoId.toString todoId }
