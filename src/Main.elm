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
import Dialog
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
import Html.Styled.Attributes as A exposing (checked, class, classList, css, disabled, href, tabindex)
import Html.Styled.Events exposing (on, onClick)
import HtmlExtra as HX
import InlineEditTodo
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode exposing (Value)
import List.Extra
import Maybe.Extra as MX
import Millis exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Result.Extra as RX
import Return
import Route exposing (Route)
import SchedulePopup
import Skeleton
import String.Extra as SX
import Svg.Attributes as SA
import Task
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UI.Button as Button
import UI.FAIcon as FAIcon
import UI.IconButton as IconButton
import UI.Key as Key
import UI.TextButton as TextButton
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)



-- Flags


type alias Flags =
    { cachedTodoList : TodoList
    , cachedProjectList : ProjectList
    , cachedAuthState : AuthState
    , cachedDialog : Dialog.Model
    , cachedInlineEditTodo : Maybe InlineEditTodo.Model
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
        |> cachedField "cachedAuthState"
            AuthState.decoder
            AuthState.initial
        |> cachedField "cachedDialog" Dialog.decoder Dialog.init
        |> cachedField "cachedInlineEditTodo"
            (JD.nullable InlineEditTodo.decoder)
            Nothing
        |> JDP.required "browserSize" BrowserSize.decoder
        |> JDP.required "now" JD.int



-- POPUP MODEL


type Popup a
    = PopupOpened a
    | PopupClosed


initPopup : Popup a
initPopup =
    PopupClosed


openPopup : String -> a -> ( Popup a, Cmd Msg )
openPopup firstFocusableDomId a =
    ( PopupOpened a, focus firstFocusableDomId )


closePopup : ( Popup a, Cmd msg )
closePopup =
    ( PopupClosed, Cmd.none )


isPopupOpenFor : a -> Popup a -> Bool
isPopupOpenFor a popup =
    popup == PopupOpened a



-- TodoPopupModel


type alias TodoPopupModel =
    Popup TodoId


todoPopupFirstFocusableDomId : String
todoPopupFirstFocusableDomId =
    "todo-popup--first-focusable"



-- SchedulePopupModel


type SchedulePopupLocation
    = InTodoPopupMenu
    | InTodoItem


type alias SchedulePopupModel =
    Popup ( SchedulePopupLocation, TodoId )


schedulePopupFirstFocusableDomId : String
schedulePopupFirstFocusableDomId =
    "schedule-popup__first-focusable"



-- MODEL


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , inlineEditTodo : Maybe InlineEditTodo.Model
    , todoPopup : TodoPopupModel
    , schedulePopup : SchedulePopupModel
    , dialog : Dialog.Model
    , authState : AuthState
    , errors : Errors
    , key : Nav.Key
    , route : Route
    , today : Calendar.Date
    , here : Time.Zone
    , browserSize : BrowserSize
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
            , todoPopup = initPopup
            , schedulePopup = initPopup
            , dialog = Dialog.init
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


type InlineEditTodoMsg
    = IETTitleChanged String
    | IETOpenSchedulePopup
    | IETCloseSchedulePopup
    | IETDueAtSelected DueAt
    | IETCancel
    | IETSave


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnHere Time.Zone
    | OnBrowserResize BrowserSize
    | Focused (Result Dom.Error ())
    | OnAuthStateChanged Value
    | OnFirestoreQueryResponse FirestoreQueryResponse
    | OnSignInClicked
    | OnSignOutClicked
      -- ExistingTodoOperations
    | OnChecked TodoId Bool
    | OnDelete TodoId
    | PatchTodo TodoId (List Todo.Msg) Millis
    | OnMoveClicked TodoId
    | OpenTodoPopupClicked TodoId
    | CloseTodoPopup
    | OpenSchedulePopupClicked SchedulePopupLocation TodoId
    | CloseSchedulePopup
    | SchedulePopupDueAtSelected Todo.DueAt
    | OnMoveToProject TodoId ProjectId
    | OnDialogOverlayClickedOrEscapePressed
    | OnEditClicked TodoId
      -- TodoEditorMsg
    | OnInlineEditTodoMsg InlineEditTodoMsg
      -- NewTodoOperations
    | OnAddTodoStart ProjectId
    | AddTodo ProjectId Millis
    | OnAddTodoTodayStart
    | AddTodoToday Millis
      -- Project
    | OnDeleteProject ProjectId
    | OnAddProjectStart
    | AddProject Millis



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged OnAuthStateChanged
        , Ports.onFirestoreQueryResponse OnFirestoreQueryResponse
        , BrowserSize.onBrowserResize OnBrowserResize
        ]



-- UPDATE


decodeValueAndUnpack :
    ( Decoder a, a -> b, JD.Error -> b )
    -> JD.Value
    -> b
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
                        ( model
                        , Nav.replaceUrl model.key (Url.toString url)
                        )
                        ( model
                        , Nav.pushUrl model.key (Url.toString url)
                        )

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

        Focused res ->
            res
                |> RX.unpack
                    HasErrors.addDomFocusError
                    (always identity)
                |> callWith model
                |> pure

        OnAuthStateChanged encodedValue ->
            model
                |> decodeValueAndUnpack
                    ( AuthState.decoder
                    , onAuthStateChanged
                    , onDecodeError
                    )
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
            handleFirestoreQueryResponse qs model

        OnChecked todoId checked ->
            ( model, patchTodoCmd todoId [ Todo.SetCompleted checked ] )

        OnDelete todoId ->
            ( model
            , Fire.deleteTodo todoId
            )

        OnDeleteProject projectId ->
            ( model
            , Fire.deleteProject projectId
            )

        PatchTodo todoId todoMsgList now ->
            ( model
            , Fire.updateTodo todoId (Todo.patch todoMsgList now)
            )

        OnAddTodoStart pid ->
            ( model, Millis.nowCmd (AddTodo pid) )

        AddTodo pid now ->
            ( model
            , Fire.addTodo (Todo.newForProject now pid)
            )

        OnAddTodoTodayStart ->
            ( model, Millis.nowCmd AddTodoToday )

        AddTodoToday now ->
            ( model
            , Fire.addTodo (Todo.newToday now now)
            )

        OnAddProjectStart ->
            ( model, Millis.nowCmd AddProject )

        AddProject now ->
            ( model
            , Fire.addProject (Project.new now)
            )

        OnEditClicked todoId ->
            startEditingTodoId todoId model
                |> Maybe.withDefault (pure model)

        OnInlineEditTodoMsg msg ->
            case model.inlineEditTodo of
                Nothing ->
                    pure model

                Just edt ->
                    case msg of
                        IETCancel ->
                            resetInlineEditTodoAndCache model

                        IETSave ->
                            persistInlineEditTodo model
                                |> andThen resetInlineEditTodoAndCache

                        IETTitleChanged title ->
                            model
                                |> updateInlineEditTodoAndCache
                                    (InlineEditTodo.setTitle title)

                        IETOpenSchedulePopup ->
                            model
                                |> updateInlineEditTodoAndCache
                                    (InlineEditTodo.setIsScheduling True)
                                |> command (focus schedulePopupFirstFocusableDomId)

                        IETCloseSchedulePopup ->
                            model
                                |> updateInlineEditTodoAndCache
                                    (InlineEditTodo.setIsScheduling False)

                        IETDueAtSelected dueAt ->
                            model
                                |> updateInlineEditTodoAndCache
                                    (InlineEditTodo.setDueAt dueAt
                                        >> InlineEditTodo.setIsScheduling False
                                    )

        OnMoveClicked todoId ->
            findTodoById todoId model
                |> MX.unwrap pure startMoving
                |> callWith model

        OpenSchedulePopupClicked loc todoId ->
            openPopup schedulePopupFirstFocusableDomId ( loc, todoId )
                |> Tuple.mapFirst (flip setSchedulePopup model)

        SchedulePopupDueAtSelected dueAt ->
            case model.schedulePopup of
                PopupOpened ( _, todoId ) ->
                    closePopup
                        |> Tuple.mapFirst (flip setSchedulePopup model)
                        |> command
                            (patchTodoCmd
                                todoId
                                [ Todo.SetDueAt dueAt ]
                            )

                PopupClosed ->
                    pure model

        CloseSchedulePopup ->
            closePopup
                |> Tuple.mapFirst (flip setSchedulePopup model)

        OpenTodoPopupClicked todoId ->
            openPopup todoPopupFirstFocusableDomId todoId
                |> Tuple.mapFirst (flip setTodoPopup model)

        CloseTodoPopup ->
            closePopup
                |> Tuple.mapFirst (flip setTodoPopup model)

        OnMoveToProject todoId pid ->
            updateDialogAndCache Dialog.Closed model
                |> command
                    (patchTodoCmd
                        todoId
                        [ Todo.SetProjectId pid ]
                    )

        OnDialogOverlayClickedOrEscapePressed ->
            case model.dialog of
                Dialog.Closed ->
                    pure model

                Dialog.MoveToProjectDialog _ _ ->
                    updateDialogAndCache Dialog.Closed model


handleFirestoreQueryResponse :
    FirestoreQueryResponse
    -> Model
    -> Return
handleFirestoreQueryResponse qs model =
    let
        decodeAndUpdate decoder successFn =
            model
                |> decodeValueAndUnpack
                    ( decoder
                    , successFn
                    , onDecodeError
                    )
                    qs.docDataList
    in
    case qs.id of
        "todoList" ->
            decodeAndUpdate
                Todo.listDecoder
                updateTodoListFromFirestore

        "projectList" ->
            decodeAndUpdate
                Project.listDecoder
                (\projectList ->
                    updateProjectListFromFirestore projectList
                 -- >> effect Fire.cleanupTodoList
                )

        _ ->
            HasErrors.add ("Invalid QueryId" ++ qs.id) model
                |> pure


focus : String -> Cmd Msg
focus domId =
    Dom.focus domId |> Task.attempt Focused


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
    model
        |> setInlineEditTodo (InlineEditTodo.fromTodo todo)
        |> pure
        |> effect cacheInlineEditTodoEffect
        |> command (focus InlineEditTodo.firstFocusableDomId)


cacheInlineEditTodoEffect model =
    Ports.localStorageSetJsonItem
        ( "cachedInlineEditTodo"
        , InlineEditTodo.maybeEncoder model.inlineEditTodo
        )


resetInlineEditTodoAndCache : Model -> Return
resetInlineEditTodoAndCache model =
    resetInlineEditTodo model
        |> pure
        |> effect cacheInlineEditTodoEffect


setInlineEditTodo_ : Maybe InlineEditTodo.Model -> Model -> Model
setInlineEditTodo_ inlineEditTodo model =
    { model | inlineEditTodo = inlineEditTodo }


setInlineEditTodo : InlineEditTodo.Model -> Model -> Model
setInlineEditTodo inlineEditTodo model =
    { model | inlineEditTodo = Just inlineEditTodo }


resetInlineEditTodo : Model -> Model
resetInlineEditTodo model =
    { model | inlineEditTodo = Nothing }


updateInlineEditTodoAndCache :
    (InlineEditTodo.Model -> InlineEditTodo.Model)
    -> Model
    -> Return
updateInlineEditTodoAndCache mfn model =
    setInlineEditTodo_ (Maybe.map mfn model.inlineEditTodo) model
        |> pure
        |> effect cacheInlineEditTodoEffect


setSchedulePopup schedulePopup model =
    { model | schedulePopup = schedulePopup }


setTodoPopup todoPopup model =
    { model | todoPopup = todoPopup }


startMoving : Todo -> Model -> Return
startMoving todo =
    updateDialogAndCache
        (Dialog.MoveToProjectDialog todo.id todo.projectId)
        >> command (focus Dialog.firstFocusable)


setDialog : Dialog.Model -> Model -> Model
setDialog dialog model =
    { model | dialog = dialog }


updateDialogAndCache : Dialog.Model -> Model -> Return
updateDialogAndCache dialog model =
    setDialog dialog model
        |> pure
        |> command
            (Ports.localStorageSetJsonItem
                ( "cachedDialog", Dialog.encoder dialog )
            )


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
        |> setDialog flags.cachedDialog
        |> setInlineEditTodo_ flags.cachedInlineEditTodo
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


setProjectList : ProjectList -> Model -> Model
setProjectList projectList model =
    { model | projectList = projectList }


updateProjectListFromFirestore : ProjectList -> Model -> Return
updateProjectListFromFirestore projectList model =
    ( setProjectList projectList model
    , Ports.localStorageSetJsonItem
        ( "cachedProjectList"
        , Project.listEncoder projectList
        )
    )


setAuthState : AuthState -> Model -> Model
setAuthState authState model =
    { model | authState = authState }


setBrowserSize browserSize model =
    { model | browserSize = browserSize }


setTodayFromNow :
    Int
    -> { b | today : Calendar.Date }
    -> { b | today : Calendar.Date }
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
    HasErrors.addDecodeError error model
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
        , content = [ viewDebugContent model, content ]
        , footer = viewFooter model
        }


viewDebugContent model =
    --    div [ class "pa3 vs3" ]
    --        [ HasErrors.detailView model
    --        , div [ class " flex hs3" ]
    --            [ div [ class "ttu tracked" ] [ text "AuthState:" ]
    --            , AuthState.view model.authState
    --            ]
    --        ]
    HasErrors.detailView model



-- LAYOUT HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "vs3" ]
        [ div [ class "flex ph3 h2 items-center" ]
            [ div [ class "f4 tracked flex-grow-1" ] [ text "ElmDOist" ]
            , case model.authState of
                AuthState.Unknown ->
                    viewHeaderBtn
                        OnSignInClicked
                        "SignIn"
                        [ disabled True ]

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
        case model.dialog of
            Dialog.Closed ->
                HX.empty

            Dialog.MoveToProjectDialog todoId projectId ->
                viewMoveDialog
                    todoId
                    projectId
                    (Project.filterActive model.projectList)


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
        [ div
            [ class "vs3 pa4 lh-copy tc bg-white shadow-1"
            , class " ba br1 b--transparent "
            ]
            [ div [ class "b" ] [ text "SignIn/SignUp using" ]
            , Button.styled []
                OnSignInClicked
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


viewMoveDialog : TodoId -> ProjectId -> List Project -> Html Msg
viewMoveDialog todoId projectId projectList =
    let
        viewProjectItem : Int -> DisplayProject -> Html Msg
        viewProjectItem idx dp =
            TextButton.view (OnMoveToProject todoId dp.id)
                dp.title
                [ class "ph3 pv2"
                , classList [ ( "b", dp.id == projectId ) ]
                , A.id <| ifElse (idx == 0) Dialog.firstFocusable ""
                ]
    in
    viewDialog
        [ div [ class "bg-white pa3 lh-copy shadow-1" ]
            (div [ class "b" ] [ text "Move To Project ..." ]
                :: (projectList
                        |> toDisplayProjectList
                        |> List.indexedMap viewProjectItem
                   )
            )
        ]


viewDialog : List (Html Msg) -> Html Msg
viewDialog =
    Dialog.view OnDialogOverlayClickedOrEscapePressed



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


inlineEditTodoTitleDomId todoId =
    TodoId.toString todoId ++ "inline-edit-todo-title-dom-id"


viewTodoItem :
    Model
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
            (InlineEditTodo.view
                inlineEditTodoViewConfig
                model.here
                model.today
                >> H.map OnInlineEditTodoMsg
            )


inlineEditTodoViewConfig : InlineEditTodo.ViewConfig InlineEditTodoMsg
inlineEditTodoViewConfig =
    { openSchedulePopup = IETOpenSchedulePopup
    , titleChanged = IETTitleChanged
    , cancel = IETCancel
    , save = IETSave
    , schedulePopupConfig =
        { close = IETCloseSchedulePopup
        , dueAtSelected = IETDueAtSelected
        , firstFocusableDomId = schedulePopupFirstFocusableDomId
        }
    }


schedulePopupConfig : SchedulePopup.ViewConfig Msg
schedulePopupConfig =
    { close = CloseSchedulePopup
    , dueAtSelected = SchedulePopupDueAtSelected
    , firstFocusableDomId = schedulePopupFirstFocusableDomId
    }


viewSchedulePopup : SchedulePopupLocation -> TodoId -> Model -> Html Msg
viewSchedulePopup loc todoId model =
    HX.viewIf (isPopupOpenFor ( loc, todoId ) model.schedulePopup)
        (\_ ->
            SchedulePopup.view schedulePopupConfig model.here model.today
        )


viewTodoItemBase :
    Model
    -> Todo
    -> Html Msg
viewTodoItemBase model todo =
    div
        [ class "flex hide-child"
        ]
        [ viewCheck todo.isDone (OnChecked todo.id)
        , viewTodoItemTitle todo
        , div [ class "flex-shrink-0 relative flex" ]
            [ viewDueAt model.here todo
            , viewSchedulePopup InTodoItem todo.id model
            ]
        , div [ class "relative flex" ]
            [ IconButton.view (OpenTodoPopupClicked todo.id)
                [ class "pa2 tc child" ]
                FAS.ellipsisH
                []
            , viewTodoPopup todo.id model
            ]
        ]


viewTodoPopup : TodoId -> Model -> Html Msg
viewTodoPopup todoId model =
    HX.viewIf (isPopupOpenFor todoId model.todoPopup)
        (\_ ->
            viewTodoPopupContainer
                (viewTodoPopupItems todoPopupFirstFocusableDomId
                    todoId
                    model
                )
        )


viewTodoPopupContainer =
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , on "focusOutside" (JD.succeed CloseTodoPopup)
        , Key.onEscape CloseTodoPopup
        , tabindex -1
        ]


viewTodoPopupItems : String -> TodoId -> Model -> List (Html Msg)
viewTodoPopupItems firstFocusable todoId model =
    let
        containerDiv =
            div [ class "relative" ]
    in
    [ containerDiv
        [ TextButton.view (OnEditClicked todoId)
            "Edit"
            [ class "pa2", A.id firstFocusable ]
        ]
    , containerDiv
        [ TextButton.view (OnMoveClicked todoId)
            "Move to Project"
            [ class "pa2" ]
        ]
    , containerDiv
        [ TextButton.view (OpenSchedulePopupClicked InTodoPopupMenu todoId)
            "Schedule"
            [ class "pa2" ]
        , viewSchedulePopup InTodoPopupMenu todoId model
        ]
    , containerDiv
        [ TextButton.view (OnDelete todoId)
            "Delete"
            [ class "pa2" ]
        ]
    ]


viewDueAt : Zone -> Todo -> Html Msg
viewDueAt here todo =
    let
        action =
            OpenSchedulePopupClicked InTodoItem todo.id
    in
    todo
        |> Todo.dueMilli
        |> MX.unpack
            (\_ ->
                IconButton.view action
                    [ class "pa2 child" ]
                    FAR.calendarPlus
                    []
            )
            (\dueMillis ->
                TextButton.view action
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
            div [ class "", onClick (OnEditClicked todo.id) ]
                [ text title ]
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
