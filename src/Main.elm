module Main exposing (main)

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
import Html.Styled as H exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes as A exposing (class, css, disabled, href, tabindex)
import Html.Styled.Keyed as HK
import HtmlExtra as HX
import InlineTodoForm
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode exposing (Value)
import List.Extra
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



-- TodoForm MODULE
-- Flags


type alias Flags =
    { cachedTodoList : TodoList
    , cachedProjectList : ProjectList
    , cachedAuthState : AuthState
    , browserSize : BrowserSize
    , now : Millis
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    let
        cachedField name decoder valueIfNull =
            JDP.required name
                (JD.oneOf [ decoder, JD.null valueIfNull ])
    in
    JD.succeed Flags
        |> cachedField "cachedTodoList" Todo.listDecoder []
        |> cachedField "cachedProjectList" Project.listDecoder []
        |> cachedField "cachedAuthState" AuthState.decoder AuthState.initial
        |> JDP.required "browserSize" BrowserSize.decoder
        |> JDP.required "now" JD.int



-- MODEL


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , inlineTodoForm : InlineTodoForm.Model
    , authState : AuthState
    , errors : Errors
    , key : Nav.Key
    , route : Route
    , today : Calendar.Date
    , here : Time.Zone
    , browserSize : BrowserSize
    }



-- MODEL QUERIES


findActiveProjectById pid model =
    model.projectList
        |> Project.filterActive
        |> List.Extra.find (.id >> (==) pid)



-- MODEL SETTERS


setTodoList : TodoList -> Model -> Model
setTodoList todoList model =
    { model | todoList = todoList }


setProjectList : ProjectList -> Model -> Model
setProjectList projectList model =
    { model | projectList = projectList }


setAuthState : AuthState -> Model -> Model
setAuthState authState model =
    { model | authState = authState }


setInlineTodoForm : InlineTodoForm.Model -> Model -> Model
setInlineTodoForm inlineTodoForm model =
    { model | inlineTodoForm = inlineTodoForm }


setBrowserSize : BrowserSize -> Model -> Model
setBrowserSize browserSize model =
    { model | browserSize = browserSize }


setTodayFromMillis : Int -> Model -> Model
setTodayFromMillis millis model =
    { model | today = dateFromMillis millis }



-- INIT


type alias Return =
    Return.Return Msg Model


init : Value -> Url -> Nav.Key -> Return
init encodedFlags url key =
    let
        route =
            Route.fromUrl url

        now =
            0

        model : Model
        model =
            { todoList = []
            , projectList = []
            , inlineTodoForm = InlineTodoForm.init
            , authState = AuthState.initial
            , errors = Errors.fromStrings []
            , key = key
            , route = route
            , today = dateFromMillis now
            , here = Time.utc
            , browserSize = BrowserSize.initial
            }
    in
    ( case JD.decodeValue flagsDecoder encodedFlags of
        Ok flags ->
            setTodoList flags.cachedTodoList model
                |> setProjectList flags.cachedProjectList
                |> setAuthState flags.cachedAuthState
                |> setBrowserSize flags.browserSize
                |> setTodayFromMillis flags.now

        Err err ->
            HasErrors.addDecodeError err model
    , Millis.hereCmd GotHere
    )


type NowContinuation
    = AddTodo_ String DueAt ProjectId
    | AddProject
    | PatchTodo_ TodoId (List Todo.Msg)


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotHere Time.Zone
    | BrowserSizeChanged BrowserSize
    | Focused (Result Dom.Error ())
    | ScrolledToTop ()
      -- Time.now Continuation
    | ContinueWithNow NowContinuation Time.Posix
      -- Fire
    | AuthStateChanged Value
    | GotFirestoreQueryResponse FirestoreQueryResponse
    | SignInClicked
    | SignOutClicked
      -- TodoOps
    | PatchTodo TodoId (List Todo.Msg)
    | AddTodo TodoForm.Fields
    | AddTodoWithDueTodayClicked
    | InlineTodoFormMsg InlineTodoForm.Msg
      -- ProjectOps
    | DeleteProjectClicked ProjectId
    | AddProjectClicked


addTodoClicked : InlineTodoForm.AddAt -> ProjectId -> Msg
addTodoClicked addAt projectId =
    InlineTodoFormMsg (InlineTodoForm.add addAt projectId)


addTodoWithDueAtClicked =
    InlineTodoFormMsg (InlineTodoForm.add InlineTodoForm.End ProjectId.default)


editTodoClicked : Todo -> Msg
editTodoClicked todo =
    InlineTodoFormMsg (InlineTodoForm.edit todo)


todoDoneChecked : TodoId -> Bool -> Msg
todoDoneChecked todoId isChecked =
    PatchTodo todoId [ Todo.SetCompleted isChecked ]



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged AuthStateChanged
        , Ports.onFirestoreQueryResponse GotFirestoreQueryResponse
        , BrowserSize.onBrowserResize BrowserSizeChanged
        ]



-- UPDATE


update : Msg -> Model -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , if Route.fromUrl url == model.route then
                        Nav.replaceUrl model.key (Url.toString url)

                      else
                        Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                newRoute =
                    Route.fromUrl url
            in
            if model.route /= newRoute then
                ( { model
                    | route = newRoute
                    , inlineTodoForm = InlineTodoForm.init
                  }
                , Dom.setViewport 0 0 |> Task.perform ScrolledToTop
                )

            else
                ( model, Cmd.none )

        ScrolledToTop _ ->
            ( model, Cmd.none )

        GotHere here ->
            ( { model | here = here }, Cmd.none )

        BrowserSizeChanged size ->
            setBrowserSize size model |> Return.singleton

        Focused domResult ->
            case domResult of
                Ok () ->
                    ( model, Cmd.none )

                Err error ->
                    ( HasErrors.addDomFocusError error model, Cmd.none )

        ContinueWithNow msg now ->
            case msg of
                AddTodo_ title dueAt projectId ->
                    ( model, Fire.addTodo (Todo.new now title dueAt projectId) )

                AddProject ->
                    ( model, Fire.addProject (Project.new now) )

                PatchTodo_ todoId todoMsgList ->
                    ( model
                    , Todo.patchTodo now todoId todoMsgList
                    )

        AuthStateChanged encodedValue ->
            case JD.decodeValue AuthState.decoder encodedValue of
                Ok authState ->
                    onAuthStateChanged authState model

                Err error ->
                    ( HasErrors.addDecodeError error model, Cmd.none )

        SignInClicked ->
            ( model, Ports.signIn () )

        SignOutClicked ->
            ( model
            , Cmd.batch
                [ Ports.signOut ()
                , Ports.disposeFirestoreQuery "todoList"
                , Ports.disposeFirestoreQuery "projectList"
                ]
            )

        GotFirestoreQueryResponse qs ->
            onFirestoreQueryResponse qs model

        AddTodo { title, dueAt, projectId } ->
            ( model, continueWithNow (AddTodo_ title dueAt projectId) )

        PatchTodo todoId todoMsgList ->
            ( model, continueWithNow (PatchTodo_ todoId todoMsgList) )

        AddTodoWithDueTodayClicked ->
            ( model
            , Time.now
                |> Task.map
                    (\now ->
                        ContinueWithNow
                            (AddTodo_ "" (Todo.dueAtFromPosix now) ProjectId.default)
                            now
                    )
                |> Task.perform identity
            )

        AddProjectClicked ->
            ( model, continueWithNow AddProject )

        DeleteProjectClicked projectId ->
            ( model
            , Fire.deleteProject projectId
            )

        InlineTodoFormMsg msg ->
            updateInlineTodoForm
                msg
                model.inlineTodoForm
                |> Tuple.mapFirst (flip setInlineTodoForm model)



-- Update TodoForm


updateInlineTodoForm : InlineTodoForm.Msg -> InlineTodoForm.Model -> ( InlineTodoForm.Model, Cmd Msg )
updateInlineTodoForm =
    InlineTodoForm.update
        { toMsg = InlineTodoFormMsg
        , onAdded = AddTodo
        , onEdited = PatchTodo
        }



-- Update Helpers


continueWithNow msg =
    Time.now |> Task.perform (ContinueWithNow msg)



-- Update Fire Helpers


onFirestoreQueryResponse : FirestoreQueryResponse -> Model -> Return
onFirestoreQueryResponse qs model =
    case qs.id of
        "todoList" ->
            case JD.decodeValue Todo.listDecoder qs.docDataList of
                Ok todoList ->
                    ( setTodoList todoList model
                    , Ports.localStorageSetJsonItem
                        ( "cachedTodoList", Todo.listEncoder todoList )
                    )

                Err error ->
                    ( HasErrors.addDecodeError error model, Cmd.none )

        "projectList" ->
            case JD.decodeValue Project.listDecoder qs.docDataList of
                Ok projectList ->
                    ( setProjectList projectList model
                    , Ports.localStorageSetJsonItem
                        ( "cachedProjectList"
                        , Project.listEncoder projectList
                        )
                    )

                Err error ->
                    ( HasErrors.addDecodeError error model, Cmd.none )

        _ ->
            HasErrors.add ("Invalid QueryId" ++ qs.id) model
                |> Return.singleton


onAuthStateChanged : AuthState -> Model -> Return
onAuthStateChanged authState model =
    ( setAuthState authState model
    , Cmd.batch
        [ case authState of
            AuthState.Unknown ->
                Cmd.none

            AuthState.SignedIn _ ->
                Cmd.batch [ Fire.queryTodoList, Fire.queryProjectList ]

            AuthState.NotSignedIn ->
                Nav.replaceUrl model.key Route.topUrl
        , Ports.localStorageSetJsonItem
            ( "cachedAuthState", AuthState.encoder authState )
        ]
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    viewRoute model.route model
        |> (\{ title, body } ->
                { title = title
                , body =
                    [ FontAwesome.Styles.css
                    , H.toUnstyled <|
                        div
                            [ A.id "root"
                            , css [ outline none ]
                            ]
                            body
                    ]
                }
           )


type alias StyledDocument msg =
    { title : String, body : List (Html msg) }


viewRoute : Route -> Model -> StyledDocument Msg
viewRoute route model =
    let
        viewInboxPage =
            viewProjectTodoListPage ProjectId.default "Inbox"

        { title, content } =
            case route of
                Route.Inbox ->
                    viewInboxPage model

                Route.Project pid ->
                    case
                        findActiveProjectById pid model
                    of
                        Just project ->
                            viewProjectTodoListPage project.id project.title model

                        Nothing ->
                            viewInboxPage model

                Route.Today ->
                    viewTodayPage model

                Route.NotFound _ ->
                    viewInboxPage model
    in
    Skeleton.view
        { title = title
        , header = viewHeader model
        , sidebar = viewSidebar model
        , content = [ HasErrors.detailView model, content ]
        , footer = viewFooter model
        }



-- LAYOUT HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "vs3" ]
        [ div [ class "flex ph3 h2 items-center" ]
            [ div [ class "f4 tracked flex-grow-1" ] [ text "ElmDOist" ]
            , case model.authState of
                AuthState.Unknown ->
                    viewHeaderBtn
                        SignInClicked
                        "SignIn"
                        [ disabled True ]

                AuthState.SignedIn user ->
                    div [ class "flex items-center hs3 " ]
                        [ div [] [ text user.displayName ]
                        , viewHeaderBtn SignOutClicked "SignOut" []
                        ]

                AuthState.NotSignedIn ->
                    viewHeaderBtn SignInClicked "SignIn" []
            ]
        ]


viewHeaderBtn : msg -> String -> List (Attribute msg) -> Html msg
viewHeaderBtn =
    TextButton.styled_ [ FCss.underline, FCss.white ]



-- LAYOUT SIDEBAR


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "lh-copy" ]
        [ viewNavLink Route.inboxUrl "Inbox"
        , viewNavLink Route.todayUrl "Today"
        , div [ class "pv1 flex hs3" ]
            [ div [ class "ttu tracked flex-grow-1" ] [ text "Projects:" ]
            , IconButton.view AddProjectClicked [] FAS.plus []
            ]
        , div []
            (Project.filterActive model.projectList
                |> List.map viewNavProject
            )
        ]


viewNavLink link title =
    a [ class "pv1 b db no-underline truncate flex-grow-1", href link ] [ text title ]


viewNavProject : Project -> Html Msg
viewNavProject project =
    div [ class "pv1 flex hs3" ]
        [ a
            [ class "b db no-underline truncate flex-grow-1"
            , href (Route.projectUrl project.id)
            ]
            [ text project.title ]
        , IconButton.view (DeleteProjectClicked project.id) [] FAS.trash []
        ]



-- LAYOUT FOOTER & DIALOG


viewFooter : Model -> Html Msg
viewFooter model =
    if model.authState == AuthState.NotSignedIn then
        viewSignInDialog

    else
        HX.none


viewDialogWrapper : List (Html msg) -> Html msg
viewDialogWrapper content =
    div
        [ class "z-1 fixed absolute--fill flex items-center justify-center"
        , tabindex -1
        ]
        [ div
            [ class "absolute absolute--fill bg-black-50"
            ]
            []
        , div [ class "absolute" ] content
        , H.node "style" [] [ text "body { overflow: hidden; }" ]
        ]


viewSignInDialog : Html Msg
viewSignInDialog =
    viewDialogWrapper
        [ div
            [ class "vs3 pa4 lh-copy tc bg-white shadow-1"
            , class " ba br1 b--transparent "
            ]
            [ div [ class "b" ] [ text "SignIn/SignUp using" ]
            , Button.styled []
                SignInClicked
                [ class "ph2 pv1"
                , class "flex inline-flex items-center justify-center"
                , class "ba br2 white bg-blue shadow-1"
                ]
                [ FAIcon.styled
                    [ FAA.fa2x, SA.class "white ph2 pv1" ]
                    FABrands.google
                , div [ class "dib white f4" ] [ text "Google" ]
                ]
            ]
        ]



-- TODAY PAGE CONTENT


dateFromMillis : Int -> Calendar.Date
dateFromMillis =
    Time.millisToPosix >> Calendar.fromPosix


viewTodayPage model =
    { title = "Today", content = todayContent model }


todayContent :
    { a
        | today : Calendar.Date
        , todoList : List Todo
        , here : Zone
        , projectList : ProjectList
        , inlineTodoForm : InlineTodoForm.Model
    }
    -> Html Msg
todayContent model =
    let
        today =
            model.today

        todayList =
            List.filter (Todo.dueDateEq today)
                model.todoList
                |> List.filter (.isDone >> not)

        overDueList =
            List.filter
                (Todo.compareDueDate today
                    >> MX.unwrap False (eq_ GT)
                )
                model.todoList
                |> List.filter (.isDone >> not)

        viewItemList =
            viewKeyedTodoItemList model.here
    in
    InlineTodoForm.view
        InlineTodoFormMsg
        { closed =
            \_ ->
                viewTodayContentHelp (viewItemList overDueList) (viewItemList todayList)
        , add =
            \_ formHtml ->
                viewTodayContentHelp (viewItemList overDueList)
                    (viewItemList todayList ++ [ viewKeyedTodoForm formHtml ])
        , edit =
            \todoId formHtml ->
                let
                    viewItem todo =
                        if todo.id == todoId then
                            viewKeyedTodoForm formHtml

                        else
                            viewKeyedTodoItem model.here todo

                    viewItemList_ =
                        List.map viewItem
                in
                viewTodayContentHelp (viewItemList_ overDueList) (viewItemList_ todayList)
        }
        model.projectList
        model.inlineTodoForm


viewTodayContentHelp overDueKeyedHtmlItems todayKeyedHtmlItems =
    div [ class "pv2 vs3" ]
        [ overDueKeyedHtmlItems
            |> HX.viewIfListNotEmpty
                (\_ ->
                    div [ class "vs3" ]
                        [ div [ class "pv2 flex items-center hs3" ]
                            [ div [ class "lh-copy b flex-grow-1" ]
                                [ text "Overdue" ]
                            ]
                        , HK.node "div" [ class "" ] overDueKeyedHtmlItems
                        ]
                )
        , div [ class "vs3" ]
            [ div [ class "pv2 flex items-center hs3" ]
                [ div [ class "lh-copy b flex-grow-1" ] [ text "Today" ]
                , TextButton.primary addTodoWithDueAtClicked "add task" []
                ]
            , HK.node "div" [ class "" ] todayKeyedHtmlItems
            ]
        ]



-- ProjectTodoList


sortedInProject : ProjectId -> TodoList -> TodoList
sortedInProject pid todoList =
    Todo.filterSort
        (Todo.BelongsToProject pid)
        [ Todo.ByIdx
        , Todo.ByRecentlyModifiedProjectId
        , Todo.ByRecentlyCreated
        ]
        todoList


viewProjectTodoListPage : ProjectId -> String -> Model -> { title : String, content : Html Msg }
viewProjectTodoListPage projectId projectName model =
    let
        displayTodoList =
            sortedInProject projectId model.todoList

        title =
            projectName
    in
    { title = title
    , content =
        div [ class "pv2 vs3" ]
            [ div [ class "pv2 flex items-center hs3" ]
                [ div [ class "b flex-grow-1" ] [ text title ]
                , TextButton.primary (addTodoClicked InlineTodoForm.Start projectId) "add task" []
                ]
            , HK.node "div" [ class "" ] (viewKeyedTodoItems model displayTodoList)
            , div [ class "lh-copy" ]
                [ TextButton.primary
                    (addTodoClicked InlineTodoForm.End projectId)
                    "add task"
                    []
                ]
            ]
    }



-- TodoItem


todoItemConfig : TodoItem.Config Msg
todoItemConfig =
    { noOp = NoOp
    , doneChanged = todoDoneChecked
    , editClicked = editTodoClicked
    }


viewKeyedTodoItem : Time.Zone -> Todo -> ( String, Html Msg )
viewKeyedTodoItem =
    TodoItem.viewKeyed todoItemConfig


viewKeyedTodoItemList : Time.Zone -> List Todo -> List ( String, Html Msg )
viewKeyedTodoItemList zone =
    TodoItem.viewList todoItemConfig zone


viewKeyedTodoForm : Html msg -> ( String, Html msg )
viewKeyedTodoForm =
    Tuple.pair "todo-item-inline-form-key"


viewKeyedTodoItems :
    Model
    -> List Todo
    -> List ( String, Html Msg )
viewKeyedTodoItems { here, projectList, inlineTodoForm } todoList =
    let
        viewBaseList : List Todo -> List ( String, Html Msg )
        viewBaseList =
            viewKeyedTodoItemList here
    in
    InlineTodoForm.view
        InlineTodoFormMsg
        { closed = \_ -> viewBaseList todoList
        , add =
            \addAt formHtml ->
                let
                    keyedForm =
                        viewKeyedTodoForm formHtml

                    keyedList =
                        viewBaseList todoList
                in
                case addAt of
                    InlineTodoForm.Start ->
                        keyedForm :: keyedList

                    InlineTodoForm.End ->
                        keyedList ++ [ keyedForm ]
        , edit =
            \todoId formHtml ->
                let
                    keyedForm =
                        viewKeyedTodoForm formHtml

                    hasEditingTodo =
                        List.any (.id >> eq_ todoId) todoList
                in
                if hasEditingTodo then
                    todoList
                        |> List.map
                            (\todo ->
                                if todoId == todo.id then
                                    keyedForm

                                else
                                    viewKeyedTodoItem here todo
                            )

                else
                    keyedForm :: viewBaseList todoList
        }
        projectList
        inlineTodoForm



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
