module Main exposing (main)

import AuthState exposing (AuthState)
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
import FontAwesome.Regular as FAR
import FontAwesome.Solid as FAS
import FontAwesome.Styles
import FunctionalCss as FCss
import HasErrors
import Html.Styled as H exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes as A exposing (class, css, disabled, href, tabindex)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as HK
import HtmlExtra as HX
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
import String.Extra as SX
import Svg.Attributes as SA
import Task
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import TodoPopup
import UI.Button as Button
import UI.FAIcon as FAIcon
import UI.IconButton as IconButton
import UI.TextButton as TextButton
import Url exposing (Url)



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



-- TODO_ FORM


type alias TodoFormFields =
    { title : String, dueAt : Todo.DueAt }


initTodoFormFields : Todo -> TodoFormFields
initTodoFormFields todo =
    { title = todo.title, dueAt = todo.dueAt }


type AddAt
    = Start
    | End
    | After TodoId


type TodoForm
    = Edit TodoId TodoFormFields TodoFormFields
    | Add AddAt TodoFormFields



-- MODEL


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , maybeTodoForm : Maybe TodoForm
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
            , maybeTodoForm = Nothing
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


findById : a -> List { b | id : a } -> Maybe { b | id : a }
findById todoId =
    List.filter (.id >> eq_ todoId)
        >> List.head



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotHere Time.Zone
    | BrowserSizeChanged BrowserSize
    | Focused (Result Dom.Error ())
    | AuthStateChanged Value
    | GotFirestoreQueryResponse FirestoreQueryResponse
    | SignInClicked
    | SignOutClicked
      -- NewTodoOperations
    | AddTodoWithProjectIdClicked ProjectId
    | AddTodo DueAt ProjectId Time.Posix
    | AddTodoWithDueTodayClicked
      -- ExistingTodoOperations
    | DeleteTodoClicked TodoId
    | PatchTodo TodoId (List Todo.Msg)
    | PatchTodoWithNow TodoId (List Todo.Msg) Time.Posix
      -- TodoListItem Messages
    | EditTodoClicked TodoId
    | CancelTodoFormClicked
      -- Project
    | DeleteProjectClicked ProjectId
    | AddProjectClicked
    | AddProject Time.Posix



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged AuthStateChanged
        , Ports.onFirestoreQueryResponse GotFirestoreQueryResponse
        , BrowserSize.onBrowserResize BrowserSizeChanged
        ]



-- UPDATE


todoDoneCheckedMsg : TodoId -> Bool -> Msg
todoDoneCheckedMsg todoId isChecked =
    PatchTodo todoId [ Todo.SetCompleted isChecked ]


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
            ( { model | route = Route.fromUrl url }, Cmd.none )

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

        AddTodo dueAt projectId now ->
            ( model, Fire.addTodo (Todo.new now dueAt projectId) )

        PatchTodo todoId todoMsgList ->
            ( model, getNow (PatchTodoWithNow todoId todoMsgList) )

        PatchTodoWithNow todoId todoMsgList now ->
            ( model
            , Fire.updateTodo todoId (Todo.patch todoMsgList now)
            )

        EditTodoClicked todoId ->
            case
                findById todoId model.todoList
            of
                Nothing ->
                    ( model, Cmd.none )

                Just todo ->
                    case model.maybeTodoForm of
                        Nothing ->
                            ( { model
                                | maybeTodoForm =
                                    Edit todoId (initTodoFormFields todo) (initTodoFormFields todo)
                                        |> Just
                              }
                            , Cmd.none
                            )

                        Just (Add _ _) ->
                            ( model, Cmd.none )

                        Just (Edit _ _ _) ->
                            ( model, Cmd.none )

        CancelTodoFormClicked ->
            ( { model | maybeTodoForm = Nothing }, Cmd.none )

        AddProject now ->
            ( model, Fire.addProject (Project.new now) )

        DeleteTodoClicked todoId ->
            ( model, Fire.deleteTodo todoId )

        AddTodoWithProjectIdClicked projectId ->
            ( model, getNow (AddTodo Todo.notDue projectId) )

        AddTodoWithDueTodayClicked ->
            ( model
            , Time.now
                |> Task.map (\now -> AddTodo (Todo.dueAtPosix now) ProjectId.default now)
                |> Task.perform identity
            )

        AddProjectClicked ->
            ( model, getNow AddProject )

        DeleteProjectClicked projectId ->
            ( model
            , Fire.deleteProject projectId
            )


getNow : (Time.Posix -> msg) -> Cmd msg
getNow msg =
    Task.perform msg Time.now


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
    case route of
        Route.Inbox ->
            let
                displayTodoList =
                    sortedInProject ProjectId.default model.todoList

                title =
                    "Inbox"
            in
            masterLayout title
                (pendingForProjectContent ProjectId.default
                    title
                    model
                    displayTodoList
                )
                model

        Route.Project pid ->
            case
                findActiveProjectById pid model
            of
                Just project ->
                    let
                        displayTodoList =
                            sortedInProject project.id model.todoList

                        title =
                            project.title
                    in
                    masterLayout title
                        (pendingForProjectContent project.id
                            title
                            model
                            displayTodoList
                        )
                        model

                Nothing ->
                    viewRoute Route.Inbox model

        Route.Today ->
            let
                title =
                    "Today"
            in
            masterLayout title (todayContent model) model

        Route.NotFound _ ->
            viewRoute Route.Inbox model


sortedInProject : ProjectId -> TodoList -> TodoList
sortedInProject pid todoList =
    Todo.filterSort
        (Todo.BelongsToProject pid)
        [ Todo.ByIdx
        , Todo.ByRecentlyModifiedProjectId
        , Todo.ByRecentlyCreated
        ]
        todoList



-- MASTER LAYOUT


masterLayout : String -> Html Msg -> Model -> StyledDocument Msg
masterLayout title content model =
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
    TextButton.styled [ FCss.underline, FCss.white ]



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


todayContent : Model -> Html Msg
todayContent model =
    let
        nowDate =
            model.today

        displayTodoList =
            List.filter (Todo.dueDateEq nowDate)
                model.todoList
                |> List.filter (.isDone >> not)

        overDueList =
            List.filter
                (Todo.compareDueDate nowDate
                    >> MX.unwrap False (eq_ GT)
                )
                model.todoList
                |> List.filter (.isDone >> not)
    in
    div [ class "pv2 vs3" ]
        [ overDueList
            |> HX.viewIfListNotEmpty
                (\_ ->
                    div [ class "vs3" ]
                        [ div [ class "pv2 flex items-center hs3" ]
                            [ div [ class "lh-copy b flex-grow-1" ]
                                [ text "Overdue" ]
                            ]
                        , HK.node "div" [ class "" ] (viewKeyedTodoItems model overDueList)
                        ]
                )
        , div [ class "vs3" ]
            [ div [ class "pv2 flex items-center hs3" ]
                [ div [ class "lh-copy b flex-grow-1" ] [ text "Today" ]
                , TextButton.primary AddTodoWithDueTodayClicked "add task" []
                ]
            , HK.node "div" [ class "" ] (viewKeyedTodoItems model displayTodoList)
            ]
        ]



-- TodoListPageContent


pendingForProjectContent :
    ProjectId
    -> String
    -> Model
    -> TodoList
    -> Html Msg
pendingForProjectContent pid title model displayTodoList =
    div [ class "pv2 vs3" ]
        [ div [ class "pv2 flex items-center hs3" ]
            [ div [ class "b flex-grow-1" ] [ text title ]
            , TextButton.primary (AddTodoWithProjectIdClicked pid) "add task" []
            ]
        , HK.node "div" [ class "" ] (viewKeyedTodoItems model displayTodoList)
        ]



-- TodoItem


viewKeyedTodoItems : Model -> List Todo -> List ( String, Html Msg )
viewKeyedTodoItems model todoList =
    case model.maybeTodoForm of
        Nothing ->
            todoList
                |> List.map
                    (\todo ->
                        ( TodoId.toString todo.id
                        , viewTodoItemBase model.here todo
                        )
                    )

        Just (Edit todoId _ currentFields) ->
            todoList
                |> List.map
                    (\todo ->
                        if todo.id == todoId then
                            ( TodoId.toString todo.id
                            , viewEditTodoItem currentFields
                            )

                        else
                            ( TodoId.toString todo.id
                            , viewTodoItemBase model.here todo
                            )
                    )

        Just (Add _ _) ->
            todoList |> List.map (\todo -> ( TodoId.toString todo.id, viewTodoItemBase model.here todo ))


viewEditTodoItem : TodoFormFields -> Html Msg
viewEditTodoItem fields =
    div [ class "flex pa3" ] [ text "TODO_ EDIT FORM" ]


viewTodoItemBase : Zone -> Todo -> Html Msg
viewTodoItemBase zone todo =
    div
        [ class "flex hide-child"
        ]
        [ viewTodoItemDoneCheckbox todo.isDone (todoDoneCheckedMsg todo.id)
        , viewTodoItemTitle (EditTodoClicked todo.id) todo.title
        , viewTodoItemDueDate todo zone
        , div [ class "relative flex" ]
            [ IconButton.view NoOp
                [ A.id <| TodoPopup.triggerElDomId todo.id
                , class "pa2 tc child"
                ]
                FAS.ellipsisH
                []
            ]
        ]


viewTodoItemDueDate : Todo -> Zone -> Html Msg
viewTodoItemDueDate todo here =
    div [ class "flex-shrink-0 relative flex" ]
        [ case Todo.dueMilli todo of
            Nothing ->
                IconButton.view NoOp
                    [ class "pa2 child" ]
                    FAR.calendarPlus
                    []

            Just dueMillis ->
                TextButton.view NoOp
                    (Millis.formatDate "MMM dd" here dueMillis)
                    [ class "pa2 flex-shrink-0 f7 lh-copy" ]
        ]


viewTodoItemDoneCheckbox : Bool -> (Bool -> msg) -> Html msg
viewTodoItemDoneCheckbox isChecked setCheckedMsg =
    let
        faCheckBtn action icon =
            IconButton.view action
                [ class "pa2 " ]
                icon
                [ FAA.lg ]
    in
    ifElse isChecked
        (faCheckBtn (setCheckedMsg False) FAR.checkCircle)
        (faCheckBtn (setCheckedMsg True) FAR.circle)


viewTodoItemTitle : Msg -> String -> Html Msg
viewTodoItemTitle clickMsg title_ =
    let
        ( title, titleClass ) =
            ifElse (SX.isBlank title_)
                ( "<no title>", "i black-70" )
                ( title_, "" )

        viewTitle =
            div [ class "", onClick NoOp ]
                [ text title ]
    in
    div
        [ class titleClass
        , class "pa2 flex-grow-1 hover-bg-light-yellow"
        , class " lh-title"
        , onClick clickMsg
        ]
        [ viewTitle ]



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
