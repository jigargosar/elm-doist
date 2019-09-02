module IET exposing (Model, update)

import Json.Encode exposing (Value)
import Todo
import TodoId exposing (TodoId)


type Editable a
    = Editable a a


type Model
    = Editing
        { todoId : TodoId
        , dueAt : Editable Todo.DueAt
        , title : Editable String
        , schedulePopupOpened : Bool
        }
    | Closed


type Msg
    = StartEditing TodoId { title : String, dueAt : Todo.DueAt }
    | Cancel
    | Save
    | OpenSchedulePopup
    | CloseSchedulePopup
    | TitleChanged String
    | DueAtChanged Todo.DueAt


type alias Config msg =
    { onOverwrite : TodoId -> { title : Maybe String, dueAt : Maybe Todo.DueAt } -> msg
    , onSave : TodoId -> { title : Maybe String, dueAt : Maybe Todo.DueAt } -> msg
    , onFocusError : String -> msg
    , onChanged : Value -> msg
    }


update :
    Config msg
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update config msg model =
    let
        nop =
            ( model, Cmd.none )
    in
    case model of
        Editing rec ->
            case msg of
                StartEditing todoId record ->
                    nop

                Cancel ->
                    nop

                Save ->
                    nop

                OpenSchedulePopup ->
                    nop

                CloseSchedulePopup ->
                    nop

                TitleChanged newTitle ->
                    nop

                DueAtChanged dueAt ->
                    nop

        Closed ->
            case msg of
                StartEditing todoId record ->
                    nop

                Cancel ->
                    nop

                Save ->
                    nop

                OpenSchedulePopup ->
                    nop

                CloseSchedulePopup ->
                    nop

                TitleChanged string ->
                    nop

                DueAtChanged dueAt ->
                    nop
