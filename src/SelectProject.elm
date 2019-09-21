module SelectProject exposing (Config, Model, Msg, init, update, view)

import BasicsExtra exposing (eq_)
import Focus
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A
import ListZipper as LZ
import Maybe.Extra as MX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import SelectInput
import Task


type Model
    = IsOpen Bool


type alias Internal =
    {}


init : Model
init =
    IsOpen False


type Msg item
    = OpenPopup
    | Selected item
    | ClosePopup


type alias Config msg item =
    { id : String
    , toMsg : Msg item -> msg
    , onSelect : item -> msg
    , itemLabel : item -> String
    }


update : Config msg item -> Msg item -> Model -> ( Model, Cmd msg )
update config message _ =
    case message of
        OpenPopup ->
            ( IsOpen True
            , Focus.autoFocusWithinId (selectProjectInputId config.id)
            )

        ClosePopup ->
            ( IsOpen False, Cmd.none )

        Selected projectId ->
            ( IsOpen False, config.onSelect projectId |> perform )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


view : Config msg item -> LZ.ListZipper item -> Model -> H.Html msg
view config items model =
    let
        open =
            case model of
                IsOpen bool ->
                    bool
    in
    SelectInput.view
        { id = selectProjectInputId config.id
        , itemLabel = config.itemLabel
        , onClose = ClosePopup
        , onOpen = OpenPopup
        , onSelect = Selected
        }
        { open = open, items = items }
        |> H.map config.toMsg


selectProjectInputId uid =
    "select-input__" ++ uid
