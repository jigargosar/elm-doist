module Now exposing (Millis, perform)

import Task
import Time


type alias Millis =
    Int


perform : (Millis -> msg) -> Cmd msg
perform f =
    Time.now |> Task.map Time.posixToMillis |> Task.perform f
