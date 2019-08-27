module Main exposing (main)

import Accessibility.Styled.Key as Key
import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith, eq_, ifElse)
import Browser
import Browser.Dom as Dom exposing (focus)
import Browser.Navigation as Nav
import BrowserSize exposing (BrowserSize)
import Calendar
import Css exposing (minWidth, none, outline, px, resize)
import Dialog
import Dict exposing (Dict)
import Dict.Extra
import Errors exposing (Errors)
import Focus
import FontAwesome.Attributes as FAA
import FontAwesome.Brands as FABrands
import FontAwesome.Regular as FAR
import FontAwesome.Solid as FAS
import FontAwesome.Styles
import FunctionalCss as FCss
import HasErrors
import Html.Styled as H exposing (Attribute, Html, a, div, text, textarea)
import Html.Styled.Attributes as A exposing (checked, class, classList, css, disabled, href, style, tabindex, value)
import Html.Styled.Events exposing (onClick, onInput, preventDefaultOn)
import HtmlStyledExtra as HX
import InlineEditTodo
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Maybe.Extra as MX
import Millis exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Result.Extra as RX
import Return
import Route exposing (Route)
import Skeleton
import String.Extra as SX
import Svg.Attributes as SA
import Task
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import TodoMenu
import UI.Button as Button
import UI.FAIcon as FAIcon
import UI.IconButton as IconButton
import UI.TextButton as TextButton
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)



-- Flags


type alias Flags =
    { cachedTodoList : TodoList
    , cachedProjectList : ProjectList
    , cachedAuthState : AuthState
    , browserSize : BrowserSize
    , now : Millis
    , cache : Cache
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "cachedTodoList" (JD.oneOf [ Todo.listDecoder, JD.null [] ])
        |> JDP.required "cachedProjectList" (JD.oneOf [ Project.listDecoder, JD.null [] ])
        |> JDP.required "cachedAuthState"
            (JD.oneOf [ AuthState.decoder, JD.null AuthState.initial ])
        |> JDP.required "browserSize" BrowserSize.decoder
        |> JDP.required "now" JD.int
        |> JDP.required "cache" cacheDecoder



-- Cache


type alias Cache =
    { dialog : Dialog.Model
    , inlineEditTodo : Maybe InlineEditTodo.Model
    }


cacheDecoder : Decoder Cache
cacheDecoder =
    JD.succeed Cache
        |> JDP.optional "dialog" Dialog.decoder Dialog.Closed
        |> JDP.optional "inlineEditTodo" (JD.maybe InlineEditTodo.decoder) Nothing


cacheEncoder : Cache -> Value
cacheEncoder { dialog, inlineEditTodo } =
    JE.object
        [ ( "dialog", Dialog.encoder dialog )
        , ( "inlineEditTodo", InlineEditTodo.maybeEncoder inlineEditTodo )
        ]



-- MODEL


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , inlineEditTodo : Maybe InlineEditTodo.Model
    , taHeight : Maybe Float
    , todoMenu : TodoMenu.Model
    , dialog : Dialog.Model
    , authState : AuthState
    , errors : Errors
    , key : Nav.Key
    , route : Route
    , today : Calendar.Date
    , here : Time.Zone
    , browserSize : BrowserSize
    }


setModelFromCache : Cache -> Model -> Model
setModelFromCache { dialog, inlineEditTodo } model =
    { model
        | dialog = dialog
        , inlineEditTodo = inlineEditTodo
    }


cacheFromModel : Model -> Cache
cacheFromModel { dialog, inlineEditTodo } =
    { dialog = dialog
    , inlineEditTodo = inlineEditTodo
    }


findTodoById todoId model =
    model.todoList
        |> List.Extra.find (.id >> (==) todoId)


findActiveProjectById pid model =
    model.projectList
        |> Project.filterActive
        |> List.Extra.find (.id >> (==) pid)



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
            , inlineEditTodo = Nothing
            , taHeight = Nothing
            , todoMenu = TodoMenu.init
            , dialog = Dialog.Closed
            , authState = AuthState.initial
            , errors = Errors.fromStrings []
            , key = key
            , route = route
            , today = dateFromMillis now
            , here = Time.utc
            , browserSize = BrowserSize.initial
            }
    in
    model
        |> pure
        |> andThen (updateFromEncodedFlags encodedFlags)
        |> command (Millis.hereCmd OnHere)



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnHere Time.Zone
    | OnBrowserResize BrowserSize
    | Focus String
    | Focused (Result Dom.Error ())
    | GotTAElement (Result Dom.Error Dom.Viewport)
    | OnAuthStateChanged Value
    | OnFirestoreQueryResponse FirestoreQueryResponse
    | OnSignInClicked
    | OnSignOutClicked
    | OnChecked TodoId Bool
    | OnDelete TodoId
    | OnDeleteProject ProjectId
    | PatchTodo TodoId (List Todo.Msg) Millis
    | OnAddTodoStart ProjectId
    | AddTodo ProjectId Millis
    | OnAddTodoTodayStart
    | AddTodoToday Millis
    | OnAddProjectStart
    | AddProject Millis
    | OnMoveStart TodoId
    | OnEditDueStart TodoId
    | OnTodoMenuTriggered TodoId
    | CloseTodoMenu TodoId Bool
    | OnSetDue TodoId DueAt
    | OnSetTitle TodoId String
    | OnMoveToProject TodoId ProjectId
    | OnDialogOverlayClicked
    | OnStartInlineEditTodo TodoId
    | OnEditCancel
    | OnEditSave



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged OnAuthStateChanged
        , Ports.onFirestoreQueryResponse OnFirestoreQueryResponse
        , BrowserSize.onBrowserResize OnBrowserResize
        ]



-- UPDATE


decodeValueAndUnpack : ( Decoder a, a -> b, JD.Error -> b ) -> JD.Value -> b
decodeValueAndUnpack ( decoder, onOk, onErr ) enc =
    enc
        |> JD.decodeValue decoder
        |> RX.unpack onErr onOk


update : Msg -> Model -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ifElse (Route.fromUrl url == model.route)
                        ( model, Nav.replaceUrl model.key (Url.toString url) )
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url
            in
            ( { model | route = route }, Cmd.none )

        OnHere here ->
            pure { model | here = here }

        OnBrowserResize size ->
            setBrowserSize size model |> pure

        Focus domId ->
            ( model, focus domId |> Task.attempt Focused )

        Focused _ ->
            pure model

        GotTAElement res ->
            Debug.log "res" res
                |> RX.unpack (\_ -> pure model)
                    (\{ scene } ->
                        pure { model | taHeight = Just scene.height }
                    )

        OnAuthStateChanged encodedValue ->
            model
                |> decodeValueAndUnpack
                    ( AuthState.decoder, onAuthStateChanged, onDecodeError )
                    encodedValue

        OnSignInClicked ->
            ( model, Ports.signIn () )

        OnSignOutClicked ->
            ( model
            , Cmd.batch
                [ Ports.signOut ()
                , Ports.disposeFirestoreQuery "todoList"
                , Ports.disposeFirestoreQuery "projectList"
                ]
            )

        OnFirestoreQueryResponse qs ->
            case qs.id of
                "todoList" ->
                    model
                        |> decodeValueAndUnpack
                            ( Todo.listDecoder, updateTodoListFromFirestore, onDecodeError )
                            qs.docDataList

                "projectList" ->
                    model
                        |> decodeValueAndUnpack
                            ( Project.listDecoder
                            , updateProjectListAndCleanupFromFirestore
                            , onDecodeError
                            )
                            qs.docDataList

                _ ->
                    HasErrors.prependString ("Invalid QueryId" ++ qs.id) model
                        |> pure

        OnChecked todoId checked ->
            ( model, patchTodoCmd todoId [ Todo.SetCompleted checked ] )

        OnDelete todoId ->
            ( model, Ports.deleteFirestoreDoc { userDocPath = "todos/" ++ TodoId.toString todoId } )

        OnDeleteProject projectId ->
            ( model
            , Ports.updateFirestoreDoc
                { userDocPath = "projects/" ++ ProjectId.toString projectId
                , data =
                    JE.object
                        [ ( "deleted", JE.bool True )
                        ]
                }
            )

        PatchTodo todoId todoMsgList now ->
            ( model
            , Ports.updateFirestoreDoc
                { userDocPath = "todos/" ++ TodoId.toString todoId
                , data = JE.object (Todo.patch todoMsgList now)
                }
            )

        OnAddTodoStart pid ->
            ( model, Millis.nowCmd (AddTodo pid) )

        AddTodo pid now ->
            ( model
            , Ports.addFirestoreDoc
                { userCollectionName = "todos"
                , data = Todo.newForProject now pid
                }
            )

        OnAddTodoTodayStart ->
            ( model, Millis.nowCmd AddTodoToday )

        AddTodoToday now ->
            ( model
            , Ports.addFirestoreDoc
                { userCollectionName = "todos"
                , data = Todo.newToday now now
                }
            )

        OnAddProjectStart ->
            ( model, Millis.nowCmd AddProject )

        AddProject now ->
            ( model
            , Ports.addFirestoreDoc
                { userCollectionName = "projects"
                , data = Project.new now
                }
            )

        OnStartInlineEditTodo todoId ->
            startEditingTodoId todoId model
                |> Maybe.withDefault (pure model)

        OnEditCancel ->
            resetInlineEditTodoAndCache model

        OnEditSave ->
            persistInlineEditTodo model
                |> andThen resetInlineEditTodoAndCache

        OnMoveStart todoId ->
            findTodoById todoId model
                |> MX.unwrap pure startMoving
                |> callWith model

        OnEditDueStart todoId ->
            startEditingDue todoId model

        OnTodoMenuTriggered todoId ->
            pure { model | todoMenu = TodoMenu.openFor todoId }
                |> command (focusTodoMenuCmd todoId)

        CloseTodoMenu todoId restoreFocus ->
            ifElse (TodoMenu.isOpenFor todoId model.todoMenu)
                ( { model | todoMenu = TodoMenu.init }
                , ifElse restoreFocus
                    (focusDomIdCmd <| todoMenuTriggerDomId todoId)
                    Cmd.none
                )
                (pure model)

        OnMoveToProject todoId pid ->
            updateDialogAndCache Dialog.Closed model
                |> command
                    (patchTodoCmd
                        todoId
                        [ Todo.SetProjectId pid ]
                    )

        OnSetDue todoId dueAt ->
            ifElse (model.dialog == Dialog.DueDialog todoId)
                (model.inlineEditTodo
                    |> MX.filter (InlineEditTodo.idEq todoId)
                    |> MX.unpack
                        (\_ ->
                            updateDialogAndCache Dialog.Closed model
                                |> command
                                    (patchTodoCmd
                                        todoId
                                        [ Todo.SetDueAt dueAt ]
                                    )
                        )
                        (\_ ->
                            model
                                |> mapInlineEditTodo (InlineEditTodo.setDueAt dueAt)
                                |> updateDialogAndCache Dialog.Closed
                        )
                )
                (pure model)

        OnSetTitle todoId title ->
            model.inlineEditTodo
                |> MX.filter (InlineEditTodo.idEq todoId)
                |> MX.unpack
                    (\_ -> pure model)
                    (\_ ->
                        ( model, Ports.resizeTextArea () )
                            |> Tuple.mapFirst (mapInlineEditTodo (InlineEditTodo.setTitle title))
                            |> andThen (updateDialogAndCache Dialog.Closed)
                    )

        OnDialogOverlayClicked ->
            case model.dialog of
                Dialog.Closed ->
                    pure model

                Dialog.MoveToProjectDialog _ _ ->
                    updateDialogAndCache Dialog.Closed model

                Dialog.DueDialog _ ->
                    updateDialogAndCache Dialog.Closed model


todoMenuDomId todoId =
    "todo-menu-dom-id--" ++ TodoId.toString todoId


todoMenuTriggerDomId todoId =
    "todo-menu-trigger-dom-id--" ++ TodoId.toString todoId


todoMenuFirstFocusableDomId todoId =
    "todo-menu--first-focusable--dom-id--" ++ TodoId.toString todoId


focusTodoMenuCmd todoId =
    let
        domId =
            todoMenuFirstFocusableDomId todoId
    in
    focus domId |> Task.attempt Focused


focusDomIdCmd : String -> Cmd Msg
focusDomIdCmd domId =
    focus domId |> Task.attempt Focused


persistInlineEditTodo : Model -> Return
persistInlineEditTodo model =
    model.inlineEditTodo
        |> Maybe.andThen InlineEditTodo.toUpdateMessages
        |> MX.unpack (\_ -> pure model)
            (\( todo, todoUpdateMsgList ) ->
                ( model
                , patchTodoCmd todo.id todoUpdateMsgList
                )
            )


startEditingTodoId : TodoId -> Model -> Maybe Return
startEditingTodoId todoId model =
    findTodoById todoId model
        |> Maybe.map
            (\todo ->
                model
                    |> persistInlineEditTodo
                    |> andThen (setInlineEditTodoAndCache todo)
            )


setInlineEditTodoAndCache : Todo -> Model -> Return
setInlineEditTodoAndCache todo model =
    { model | taHeight = Nothing }
        |> setInlineEditTodo (InlineEditTodo.fromTodo todo)
        |> pure
        |> effect cacheEffect
        |> command (focusDomIdCmd todoTADomId)
        |> command (Dom.getViewportOf todoTADomId |> Task.attempt GotTAElement)


resetInlineEditTodoAndCache : Model -> Return
resetInlineEditTodoAndCache model =
    resetInlineEditTodo model
        |> pure
        |> effect cacheEffect


setInlineEditTodo_ : Maybe InlineEditTodo.Model -> Model -> Model
setInlineEditTodo_ inlineEditTodo model =
    { model | inlineEditTodo = inlineEditTodo }


setInlineEditTodo : InlineEditTodo.Model -> Model -> Model
setInlineEditTodo inlineEditTodo model =
    { model | inlineEditTodo = Just inlineEditTodo }


resetInlineEditTodo : Model -> Model
resetInlineEditTodo model =
    { model | inlineEditTodo = Nothing }


mapInlineEditTodo : (InlineEditTodo.Model -> InlineEditTodo.Model) -> Model -> Model
mapInlineEditTodo mfn model =
    setInlineEditTodo_ (Maybe.map mfn model.inlineEditTodo) model


startMoving : Todo -> Model -> Return
startMoving todo =
    updateDialogAndCache (Dialog.MoveToProjectDialog todo.id todo.projectId)


startEditingDue : TodoId -> Model -> Return
startEditingDue todoId =
    updateDialogAndCache (Dialog.DueDialog todoId)


updateDialogAndCache : Dialog.Model -> Model -> Return
updateDialogAndCache dialog model =
    pure { model | dialog = dialog }
        |> effect cacheEffect


cacheEffect : Model -> Cmd msg
cacheEffect model =
    Ports.setCache (cacheEncoder (cacheFromModel model))


patchTodoCmd : TodoId -> List Todo.Msg -> Cmd Msg
patchTodoCmd todoId todoMsgList =
    PatchTodo todoId todoMsgList |> Millis.nowCmd


queryTodoListCmd =
    Ports.queryFirestore
        { id = "todoList"
        , userCollectionName = "todos"
        , whereClause = []
        }


queryProjectListCmd =
    Ports.queryFirestore
        { id = "projectList"
        , userCollectionName = "projects"
        , whereClause = []
        }


updateFromEncodedFlags : Value -> Model -> Return
updateFromEncodedFlags encodedFlags model =
    model
        |> decodeValueAndUnpack
            ( flagsDecoder, updateFromFlags, onDecodeError )
            encodedFlags


updateFromFlags : Flags -> Model -> Return
updateFromFlags flags model =
    setTodoList flags.cachedTodoList model
        |> setProjectList flags.cachedProjectList
        |> setAuthState flags.cachedAuthState
        |> setModelFromCache flags.cache
        |> setBrowserSize flags.browserSize
        |> setTodayFromNow flags.now
        |> pure


setTodoList : TodoList -> Model -> Model
setTodoList todoList model =
    { model | todoList = todoList }


updateTodoListFromFirestore : TodoList -> Model -> Return
updateTodoListFromFirestore todoList model =
    setTodoList todoList model
        |> pure
        |> command
            (Ports.localStorageSetJsonItem
                ( "cachedTodoList", Todo.listEncoder todoList )
            )


cleanupTodoList : Model -> Return
cleanupTodoList model =
    let
        todoByPid : Dict String (List Todo)
        todoByPid =
            Dict.Extra.groupBy (.projectId >> ProjectId.toString) model.todoList

        deleteProjectsCmd =
            model.projectList
                |> List.filter .deleted
                |> List.filter
                    (\p ->
                        Dict.get (ProjectId.toString p.id) todoByPid |> MX.unwrap True List.isEmpty
                    )
                |> List.map
                    (.id
                        >> (\projectId ->
                                Ports.deleteFirestoreDoc { userDocPath = "projects/" ++ ProjectId.toString projectId }
                           )
                    )
                |> Cmd.batch

        deleteTodosCmd : Cmd msg
        deleteTodosCmd =
            model.projectList
                |> List.filter .deleted
                |> List.filterMap (\p -> Dict.get (ProjectId.toString p.id) todoByPid)
                |> List.concat
                |> List.map
                    (.id
                        >> (\todoId ->
                                Ports.deleteFirestoreDoc
                                    { userDocPath = "todos/" ++ TodoId.toString todoId }
                           )
                    )
                |> Cmd.batch
    in
    ( model, Cmd.batch [ deleteTodosCmd, deleteProjectsCmd ] )


setProjectList : ProjectList -> Model -> Model
setProjectList projectList model =
    { model | projectList = projectList }


updateProjectListAndCleanupFromFirestore : ProjectList -> Model -> Return
updateProjectListAndCleanupFromFirestore projectList model =
    setProjectList projectList model
        |> pure
        |> command
            (Ports.localStorageSetJsonItem
                ( "cachedProjectList", Project.listEncoder projectList )
            )
        |> andThen cleanupTodoList


setAuthState : AuthState -> Model -> Model
setAuthState authState model =
    { model | authState = authState }


setBrowserSize browserSize model =
    { model | browserSize = browserSize }


setTodayFromNow : Int -> { b | today : Calendar.Date } -> { b | today : Calendar.Date }
setTodayFromNow millis model =
    { model | today = dateFromMillis millis }


onAuthStateChanged : AuthState -> Model -> Return
onAuthStateChanged authState model =
    let
        cmd =
            case authState of
                AuthState.Unknown ->
                    Cmd.none

                AuthState.SignedIn _ ->
                    Cmd.batch [ queryTodoListCmd, queryProjectListCmd ]

                AuthState.NotSignedIn ->
                    Nav.replaceUrl model.key Route.topUrl
    in
    setAuthState authState model
        |> pure
        |> command cmd
        |> command
            (Ports.localStorageSetJsonItem
                ( "cachedAuthState", AuthState.encoder authState )
            )


onDecodeError : JD.Error -> Model -> Return
onDecodeError error model =
    HasErrors.prependDecodeError error model
        |> pure



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
                            , class "h-100 overflow-y-scroll"
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
        , content = [ content, viewDebugContent model ]
        , footer = viewFooter model
        }


viewDebugContent model =
    div [ class "pa3 vs3" ]
        [ HasErrors.detailView model
        , div [ class " flex hs3" ]
            [ div [ class "ttu tracked" ] [ text "AuthState:" ]
            , AuthState.view model.authState
            ]
        ]



-- LAYOUT HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "vs3" ]
        [ div [ class "flex ph3 h2 items-center" ]
            [ div [ class "f4 tracked flex-grow-1" ] [ text "ElmDOist" ]
            , case model.authState of
                AuthState.Unknown ->
                    viewHeaderBtn NoOp "SignIn" [ disabled True ]

                AuthState.SignedIn user ->
                    div [ class "flex items-center hs3 " ]
                        [ div [] [ text user.displayName ]
                        , viewHeaderBtn OnSignOutClicked "SignOut" []
                        ]

                AuthState.NotSignedIn ->
                    viewHeaderBtn OnSignInClicked "SignIn" []
            ]
        ]


viewHeaderBtn : msg -> String -> List (Attribute msg) -> Html msg
viewHeaderBtn action label =
    TextButton.styled [ FCss.underline, FCss.white ]
        action
        label



-- LAYOUT SIDEBAR


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "lh-title" ]
        [ viewNavLink Route.inboxUrl "Inbox"
        , viewNavLink Route.todayUrl "Today"
        , div [ class "pv2 flex hs3" ]
            [ div [ class "ttu tracked flex-grow-1" ] [ text "Projects:" ]
            , IconButton.view OnAddProjectStart [] FAS.plus []
            ]
        , viewNavProjects (Project.filterActive model.projectList)
        ]


viewNavLink link title =
    div [ class "pv2 " ]
        [ div [ class "flex hs3" ]
            [ a
                [ class "no-underline"
                , href link
                , class "b"
                ]
                [ text title ]
            ]
        ]


viewNavProjects : ProjectList -> Html Msg
viewNavProjects projectList =
    div [ class "b " ] (List.map viewNavProject projectList)


viewNavProject : Project -> Html Msg
viewNavProject project =
    div [ class "pv2 flex hs3" ]
        [ a
            [ class "no-underline truncate flex-grow-1"
            , href (Route.projectUrl project.id)
            ]
            [ text project.title ]
        , IconButton.view (OnDeleteProject project.id) [] FAS.trash []
        ]



-- LAYOUT FOOTER & DIALOG


viewFooter : Model -> Html Msg
viewFooter model =
    if model.authState == AuthState.NotSignedIn then
        viewSignInDialog

    else
        div []
            [ case model.dialog of
                Dialog.Closed ->
                    HX.empty

                Dialog.MoveToProjectDialog todoId projectId ->
                    viewMoveDialog todoId projectId (Project.filterActive model.projectList)

                Dialog.DueDialog todoId ->
                    viewDueDialog model.here model.today todoId
            ]


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject p =
    { id = p.id, title = p.title }


inboxDisplayProject =
    { id = ProjectId.default, title = "Inbox" }


toDisplayProjectList projectList =
    inboxDisplayProject :: List.map toDisplayProject projectList


viewSignInDialog =
    Dialog.view NoOp
        [ div [ class "vs3 bg-white pa4 lh-copy shadow-1 ba br1 b--transparent tc" ]
            [ div [ class "b" ] [ text "SignIn/SignUp using" ]
            , Button.styled []
                OnSignInClicked
                [ class "ph2 pv1 flex inline-flex items-center justify-center "
                , class "ba br2 white bg-blue shadow-1"
                ]
                [ FAIcon.styled [ FAA.fa2x, SA.class "white ph2 pv1" ] FABrands.google
                , div [ class "dib white f4" ] [ text "Google" ]
                ]
            ]
        ]


viewMoveDialog : TodoId -> ProjectId -> List Project -> Html Msg
viewMoveDialog todoId projectId projectList =
    let
        viewPLI dp =
            TextButton.view (OnMoveToProject todoId dp.id)
                dp.title
                [ class "ph3 pv2"
                , classList [ ( "b", dp.id == projectId ) ]
                ]
    in
    viewDialog
        [ div [ class "bg-white pa3 lh-copy shadow-1" ]
            (div [ class "b" ] [ text "Move To Project ..." ]
                :: (projectList
                        |> toDisplayProjectList
                        |> List.map viewPLI
                   )
            )
        ]


viewDueDialog : Time.Zone -> Calendar.Date -> TodoId -> Html Msg
viewDueDialog zone today todoId =
    let
        todayFmt =
            Millis.formatDate "ddd MMM yyyy" zone (Calendar.toMillis today)

        yesterday =
            Calendar.decrementDay today

        yesterdayFmt =
            Millis.formatDate "ddd MMM yyyy" zone (yesterday |> Calendar.toMillis)

        setDueMsg =
            OnSetDue todoId

        viewSetDueButton action label =
            TextButton.view (setDueMsg <| action) label [ class "ph3 pv2" ]
    in
    viewDialog
        [ div [ class "bg-white pa3 lh-copy shadow-1" ]
            [ div [ class " b  " ] [ text "Due Date" ]
            , viewSetDueButton (Todo.DueAt <| Calendar.toMillis today)
                ("Today: " ++ todayFmt)
            , viewSetDueButton (Todo.DueAt <| Calendar.toMillis yesterday)
                ("Yesterday: " ++ yesterdayFmt)
            , viewSetDueButton Todo.NoDue "No Due Date"
            ]
        ]


viewDialog : List (Html Msg) -> Html Msg
viewDialog =
    Dialog.view OnDialogOverlayClicked



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
            |> HX.viewNotEmpty
                (\_ ->
                    div [ class "vs3" ]
                        [ div [ class "pv2 flex items-center hs3" ]
                            [ div [ class "lh-copy b flex-grow-1" ] [ text "Overdue" ]
                            ]
                        , div [ class "" ]
                            (List.map
                                (viewTodoItem model)
                                overDueList
                            )
                        ]
                )
        , div [ class "vs3" ]
            [ div [ class "pv2 flex items-center hs3" ]
                [ div [ class "lh-copy b flex-grow-1" ] [ text "Today" ]
                , TextButton.primary OnAddTodoTodayStart "add task" []
                ]
            , div [ class "" ]
                (List.map
                    (viewTodoItem model)
                    displayTodoList
                )
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
            , TextButton.primary (OnAddTodoStart pid) "add task" []
            ]
        , div [ class "" ] (List.map (viewTodoItem model) displayTodoList)
        ]



-- TodoItem


todoTADomId =
    "todo-inline-edit-dom-id"


viewTodoItem :
    { a
        | inlineEditTodo : Maybe InlineEditTodo.Model
        , here : Zone
        , todoMenu : TodoMenu.Model
        , taHeight : Maybe Float
    }
    -> Todo
    -> Html Msg
viewTodoItem model todo =
    let
        { inlineEditTodo, here } =
            model
    in
    inlineEditTodo
        |> MX.filter (InlineEditTodo.idEq todo.id)
        |> MX.unpack (\_ -> viewTodoItemBase model todo)
            (viewEditTodoItem here model.taHeight)


viewEditTodoItem : Time.Zone -> Maybe Float -> InlineEditTodo.Model -> Html Msg
viewEditTodoItem here taHeight edt =
    let
        titleValue =
            InlineEditTodo.titleOrDefault edt

        dueAtValue =
            InlineEditTodo.dueAtOrDefault edt
                |> Todo.dueAtToMillis

        todoId =
            InlineEditTodo.todoId edt

        viewIP =
            div [ class "flex-grow-1 flex ba b--moon-gray" ]
                [ textarea
                    [ A.id todoTADomId
                    , class "pa2 flex-grow-1 lh-copy bn"
                    , value titleValue
                    , onInput (OnSetTitle todoId)
                    , css [ resize none ]
                    , class "overflow-hidden"

                    --                    , taHeight
                    --                        |> MX.unwrap (style "height" "auto")
                    --                            (\ht -> style "height" (String.fromFloat ht ++ "px"))
                    ]
                    []
                ]

        ( txt, cls ) =
            dueAtValue
                |> MX.unpack
                    (\_ -> ( "Schedule", "gray" ))
                    (\mi ->
                        ( Millis.formatDate "MMM dd" here <| mi, "near-black" )
                    )

        viewDue =
            TextButton.secondary (OnEditDueStart <| todoId)
                txt
                [ class "pa3 ba b--moon-gray"
                , class cls
                , css [ minWidth <| px 100 ]
                ]
    in
    div
        [ class "pv3 ph2"
        , tabindex 0
        ]
        [ div [ class "flex" ]
            [ viewIP
            , viewDue
            ]
        , div [ class "flex hs3 lh-copy" ]
            [ TextButton.primary OnEditSave "Save" [ class "pa2" ]
            , TextButton.secondary OnEditCancel "Cancel" [ class "pa2" ]
            ]
        ]


viewTodoItemBase :
    { a
        | here : Zone
        , todoMenu : TodoMenu.Model
    }
    -> Todo
    -> Html Msg
viewTodoItemBase model todo =
    div
        [ class "flex hide-child"
        ]
        [ viewCheck todo.isDone (OnChecked todo.id)
        , viewTodoItemTitle todo
        , viewDueAt model.here todo
        , div [ class "relative flex" ]
            [ IconButton.view (OnTodoMenuTriggered todo.id)
                [ A.id <| todoMenuTriggerDomId todo.id
                , class "pa2 tc child"
                ]
                FAS.ellipsisH
                []
            , HX.viewIf (TodoMenu.isOpenFor todo.id model.todoMenu)
                (viewTodoMenu todo)
            ]
        ]


viewTodoMenu : { a | id : TodoId } -> Html Msg
viewTodoMenu todo =
    let
        menuItemModelList =
            [ ( OnStartInlineEditTodo, "Edit" )
            , ( OnMoveStart, "Move to Project" )
            , ( OnEditDueStart, "Schedule" )
            , ( OnDelete, "Delete" )
            ]

        viewMenuItem : number -> ( TodoId -> msg, String ) -> Html msg
        viewMenuItem idx ( todoAction, label ) =
            TextButton.view (todoAction todo.id)
                label
                [ A.id <|
                    ifElse (idx == 0)
                        (todoMenuFirstFocusableDomId todo.id)
                        ""
                , class "pa2"
                ]

        menuDomId =
            todoMenuDomId todo.id

        closeMsg : Bool -> Msg
        closeMsg restoreFocus =
            CloseTodoMenu todo.id restoreFocus
    in
    div
        [ A.id menuDomId
        , class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , Focus.onFocusOutsideDomId menuDomId (closeMsg False)
        , preventDefaultOn "keydown" (Key.escape ( closeMsg True, True ))
        ]
        (menuItemModelList |> List.indexedMap viewMenuItem)


viewDueAt : Zone -> Todo -> Html Msg
viewDueAt here todo =
    todo
        |> Todo.dueMilli
        |> MX.unpack
            (\_ ->
                IconButton.view (OnEditDueStart todo.id)
                    [ class "pa2 child" ]
                    FAR.calendarPlus
                    []
            )
            (\dueMillis ->
                TextButton.view (OnEditDueStart todo.id)
                    (Millis.formatDate "MMM dd" here dueMillis)
                    [ class "pa2 flex-shrink-0 f7 lh-copy" ]
            )


viewCheck : Bool -> (Bool -> msg) -> Html msg
viewCheck isChecked setCheckedMsg =
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


viewTodoItemTitle : Todo -> Html Msg
viewTodoItemTitle todo =
    let
        ( title, titleClass ) =
            ifElse (SX.isBlank todo.title)
                ( "<no title>", "i black-70" )
                ( todo.title, "" )

        viewTitle =
            div [ class "", onClick (OnStartInlineEditTodo todo.id) ] [ text title ]
    in
    div
        [ class titleClass
        , class "pa2 flex-grow-1 hover-bg-light-yellow"
        , class " lh-title"
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
