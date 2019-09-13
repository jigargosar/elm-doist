module DevMain exposing (..)

import AuthState exposing (AuthState)
import Basics.Extra exposing (flip)
import BasicsExtra exposing (..)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import BrowserSize exposing (BrowserSize)
import Calendar
import Css exposing (none, outline)
import Errors exposing (Errors)
import Fire
import FontAwesome.Attributes as FAA
import FontAwesome.Brands as FABrands
import FontAwesome.Solid as FAS
import FontAwesome.Styles
import FunctionalCss as FCss
import HasErrors
import Html
import Html.Styled as H exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes as A exposing (class, css, disabled, href, tabindex)
import Html.Styled.Keyed as HK
import HtmlExtra as HX
import InlineTodoForm
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode exposing (Value)
import List.Extra
import Main
import Maybe.Extra as MX
import Millis exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Return
import Route exposing (Route)
import Skeleton
import Svg.Attributes as SA
import Task
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoForm
import TodoId exposing (TodoId)
import TodoItem
import UI.Button as Button
import UI.FAIcon as FAIcon
import UI.IconButton as IconButton
import UI.TextButton as TextButton
import Url exposing (Url)


type alias Model =
    { mainModel : Main.Model }


type Msg
    = MainMsg Main.Msg


type alias Return =
    Return.Return Msg Model


init : Value -> Url -> Nav.Key -> Return
init f u k =
    Main.appConfig.init f u k
        |> Tuple.mapBoth (\mainModel -> { mainModel = mainModel }) (Cmd.map MainMsg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Main.appConfig.subscriptions model.mainModel |> Sub.map MainMsg
        ]


update : Msg -> Model -> Return
update message model =
    case message of
        MainMsg msg ->
            updateMain msg model


updateMain : Main.Msg -> Model -> Return
updateMain message model =
    Main.appConfig.update message model.mainModel
        |> Tuple.mapBoth (\mainModel -> { model | mainModel = mainModel }) (Cmd.map MainMsg)


view : Model -> Browser.Document Msg
view model =
    Main.appConfig.view model.mainModel
        |> Html.map MainMsg


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Main.appConfig.onUrlRequest >> MainMsg
        , onUrlChange = Main.appConfig.onUrlChange >> MainMsg
        }
