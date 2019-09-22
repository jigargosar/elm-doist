module TodoContextMenu exposing (Model, Msg)

import Focus
import TodoId exposing (TodoId)


type Model
    = Opened TodoId
    | Closed


init : Model
init =
    Closed


open : TodoId -> Msg
open todoId =
    Open todoId


type Msg
    = Open TodoId
    | Close


update : { toMsg : Msg -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        Open todoId ->
            ( Opened todoId, focusFirstCmd )

        Close ->
            ( Closed, Cmd.none )


rootDomId =
    "todo-context-menu"


focusFirstCmd =
    Focus.autoFocusWithinId rootDomId
