module MovePopup exposing (Config, Model, Msg, Reason(..), init, update, view)

import BasicsExtra exposing (eq_)
import Css
import Focus
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import List.Extra as LX
import Menu
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = Model Menu.Model


init : Model
init =
    Model Menu.init


type Msg
    = MenuMsg (Menu.Msg DisplayProject)


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message ((Model menu) as model) =
    case message of
        MenuMsg msg ->
            Menu.update (menuConfig config) msg menu
                |> Tuple.mapBoth Model (Cmd.map identity)


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject { id, title } =
    { id = id, title = title }


inboxDisplayProject : DisplayProject
inboxDisplayProject =
    { id = ProjectId.default, title = "Inbox" }


type Reason
    = LostFocus
    | Canceled
    | Selected ProjectId


type alias Config msg =
    { id : String
    , closed : Reason -> msg
    , toMsg : Msg -> msg
    }


menuConfig : Config msg -> Menu.Config DisplayProject msg
menuConfig config =
    { toMsg = config.toMsg << MenuMsg
    , id = config.id
    , title = .title
    , selected = config.closed << Selected << .id
    , closed =
        \reason ->
            case reason of
                Menu.LostFocus ->
                    config.closed LostFocus

                Menu.Canceled ->
                    config.closed Canceled
    }


view : Config msg -> ProjectId -> ProjectList -> Model -> Html msg
view config selectedProjectId projectList (Model menu) =
    let
        items =
            inboxDisplayProject
                :: List.map toDisplayProject projectList

        maybeSelected =
            LX.find (.id >> eq_ selectedProjectId) items
    in
    Menu.view (menuConfig config) maybeSelected items menu
