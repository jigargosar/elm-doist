module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import Calendar
import Css exposing (auto, bottom, calc, height, left, marginLeft, maxWidth, minus, position, px, rem, sticky, top, transforms, translateX, width, zero)
import Css.Media as Media exposing (only, screen, withMedia)
import Css.Transitions as Transition exposing (transition)
import Dict exposing (Dict)
import Dict.Extra
import Errors exposing (Errors)
import HasErrors
import Html.Styled exposing (Html, a, button, div, input, text)
import Html.Styled.Attributes exposing (checked, class, classList, css, disabled, href, tabindex, type_, value)
import Html.Styled.Events exposing (onCheck, onClick)
import HtmlStyledEvent exposing (onDomIdClicked)
import HtmlStyledExtra
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Maybe.Extra
import Millis exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Result.Extra
import Return
import Route exposing (Route)
import Time
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)



-- MODEL


type alias InlineEditTodo =
    { todo : Todo, title : Maybe String }


type Dialog
    = NoDialog
    | MoveToProjectDialog Todo
    | DueDialog Todo


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , inlineEditTodo : Maybe InlineEditTodo
    , dialog : Dialog
    , authState : AuthState
    , errors : Errors
    , key : Nav.Key
    , route : Route
    , now : Millis
    , here : Time.Zone
    }


type alias Cache =
    { dialog : Dialog
    }


type alias Flags =
    { cachedTodoList : TodoList
    , cachedProjectList : ProjectList
    , cachedAuthState : AuthState
    , cache : Cache
    }


dialogDecoder : Decoder Dialog
dialogDecoder =
    JD.field "tag" JD.string
        |> JD.andThen dialogDecoderForTag


dialogEncoder : Dialog -> Value
dialogEncoder dialog =
    case dialog of
        NoDialog ->
            JE.object [ ( "tag", JE.string "NoDialog" ) ]

        MoveToProjectDialog todo ->
            JE.object
                [ ( "tag", JE.string "MoveToProjectDialog" )
                , ( "todo", Todo.encoder todo )
                ]

        DueDialog todo ->
            JE.object
                [ ( "tag", JE.string "DueDialog" )
                , ( "todo", Todo.encoder todo )
                ]


dialogDecoderForTag : String -> Decoder Dialog
dialogDecoderForTag tag =
    case tag of
        "NoDialog" ->
            JD.succeed NoDialog

        "MoveToProjectDialog" ->
            JD.field "todo" Todo.decoder
                |> JD.map MoveToProjectDialog

        "DueDialog" ->
            JD.field "todo" Todo.decoder
                |> JD.map DueDialog

        _ ->
            JD.fail ("Invalid Dialog Tag:" ++ tag)


cacheDecoder : Decoder Cache
cacheDecoder =
    JD.succeed Cache
        |> JDP.optional "dialog" dialogDecoder NoDialog


cacheEncoder : Cache -> Value
cacheEncoder { dialog } =
    JE.object
        [ ( "dialog", dialogEncoder dialog )
        ]


setModelFromCache : Cache -> Model -> Model
setModelFromCache { dialog } model =
    { model | dialog = dialog }


cacheFromModel : Model -> Cache
cacheFromModel model =
    { dialog = model.dialog }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "cachedTodoList" (JD.oneOf [ Todo.listDecoder, JD.null [] ])
        |> JDP.required "cachedProjectList" (JD.oneOf [ Project.listDecoder, JD.null [] ])
        |> JDP.required "cachedAuthState"
            (JD.oneOf [ AuthState.decoder, JD.null AuthState.initial ])
        |> JDP.required "cache" cacheDecoder



-- INIT


type alias Return =
    Return.Return Msg Model


init : Value -> Url -> Nav.Key -> Return
init encodedFlags url key =
    let
        route =
            Route.fromUrl url

        model : Model
        model =
            { todoList = []
            , projectList = []
            , inlineEditTodo = Nothing
            , dialog = NoDialog
            , authState = AuthState.initial
            , errors = Errors.fromStrings [ "Testing Error View" ]
            , key = key
            , route = route
            , now = 0
            , here = Time.utc
            }
    in
    model
        |> pure
        |> andThen (updateFromEncodedFlags encodedFlags)
        |> command (Millis.nowCmd OnNow)
        |> command (Millis.hereCmd OnHere)



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnNow Millis
    | OnHere Time.Zone
    | OnAuthStateChanged Value
    | OnTodoListChanged Value
    | OnFirestoreQueryResponse FirestoreQueryResponse
    | OnSignInClicked
    | OnSignOutClicked
    | OnChangeTitleRequested TodoId
    | OnChecked TodoId Bool
    | OnDelete TodoId
    | OnDeleteProject ProjectId
    | PatchTodo TodoId Todo.Msg Millis
    | OnAddTodoStart ProjectId
    | OnAddTodoTodayStart
    | AddTodoToday Millis
    | AddTodo ProjectId Millis
    | OnAddProjectStart
    | AddProject Millis
    | OnMoveStart TodoId
    | OnEditDueStart TodoId
    | OnSetDue Millis
    | OnMoveToProject ProjectId
    | OnDialogOverlayClicked
    | OnEdit TodoId
    | OnEditCancel
    | OnEditSave



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged OnAuthStateChanged
        , Ports.onFirestoreQueryResponse OnFirestoreQueryResponse
        , Time.every 1000 (Time.posixToMillis >> OnNow)
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
                    if Route.fromUrl url == model.route then
                        ( model, Nav.replaceUrl model.key (Url.toString url) )

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url
            in
            ( { model | route = route }, {- queryTodoListForRouteCmd route -} Cmd.none )

        OnNow now ->
            pure { model | now = now }

        OnHere here ->
            pure { model | here = here }

        OnAuthStateChanged encodedValue ->
            JD.decodeValue AuthState.decoder encodedValue
                |> Result.Extra.unpack onDecodeError onAuthStateChanged
                |> callWith model

        OnTodoListChanged encodedValue ->
            JD.decodeValue Todo.listDecoder encodedValue
                |> Result.Extra.unpack onDecodeError updateTodoListFromFirestore
                |> callWith model

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

        OnChangeTitleRequested todoId ->
            ( model, Ports.changeTodoTitle todoId )

        OnFirestoreQueryResponse qs ->
            case qs.id of
                "todoList" ->
                    qs.docDataList
                        |> List.map (JD.decodeValue Todo.decoder)
                        |> Result.Extra.combine
                        |> Result.Extra.unpack onDecodeError updateTodoListFromFirestore
                        |> callWith model

                "projectList" ->
                    qs.docDataList
                        |> List.map (JD.decodeValue Project.decoder)
                        |> Result.Extra.combine
                        |> Result.Extra.unpack onDecodeError updateProjectListAndCleanupFromFirestore
                        |> callWith model

                _ ->
                    HasErrors.prependString ("Invalid QueryId" ++ qs.id) model
                        |> pure

        OnChecked todoId checked ->
            ( model, patchTodoCmd todoId (Todo.SetCompleted checked) )

        OnDelete todoId ->
            ( model, Ports.deleteFirestoreDoc { userDocPath = "todos/" ++ todoId } )

        OnDeleteProject projectId ->
            ( model
            , Ports.updateFirestoreDoc
                { userDocPath = "projects/" ++ projectId
                , data =
                    JE.object
                        [ ( "deleted", JE.bool True )
                        ]
                }
            )

        PatchTodo todoId todoMsg now ->
            ( model
            , Ports.updateFirestoreDoc
                { userDocPath = "todos/" ++ todoId
                , data = JE.object (Todo.modifyPatch todoMsg now)
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
                , data = Todo.newToday now model.now
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

        OnEdit todoId ->
            model.todoList
                |> List.Extra.find (.id >> (==) todoId)
                |> Maybe.Extra.unwrap pure startEditing
                |> callWith model

        OnMoveStart todoId ->
            model.todoList
                |> List.Extra.find (.id >> (==) todoId)
                |> Maybe.Extra.unwrap pure startMoving
                |> callWith model

        OnEditDueStart todoId ->
            model.todoList
                |> List.Extra.find (.id >> (==) todoId)
                |> Maybe.Extra.unwrap pure startEditingDue
                |> callWith model

        OnMoveToProject pid ->
            case model.dialog of
                NoDialog ->
                    pure model

                MoveToProjectDialog todo ->
                    updateDialog NoDialog model
                        |> command
                            (patchTodoCmd
                                todo.id
                                (Todo.SetProjectId pid)
                            )

                DueDialog _ ->
                    pure model

        OnSetDue dueAt ->
            case model.dialog of
                NoDialog ->
                    pure model

                MoveToProjectDialog _ ->
                    pure model

                DueDialog todo ->
                    updateDialog NoDialog model
                        |> command
                            (patchTodoCmd
                                todo.id
                                (Todo.SetDueAt dueAt)
                            )

        OnDialogOverlayClicked ->
            case model.dialog of
                NoDialog ->
                    pure model

                MoveToProjectDialog _ ->
                    updateDialog NoDialog model

                DueDialog _ ->
                    updateDialog NoDialog model

        OnEditCancel ->
            pure { model | inlineEditTodo = Nothing }

        OnEditSave ->
            pure { model | inlineEditTodo = Nothing }


startEditing : Todo -> Model -> Return
startEditing todo model =
    pure { model | inlineEditTodo = Just { todo = todo, title = Nothing } }


startMoving : Todo -> Model -> Return
startMoving todo =
    updateDialog (MoveToProjectDialog todo)


startEditingDue : Todo -> Model -> Return
startEditingDue todo =
    updateDialog (DueDialog todo)


updateDialog : Dialog -> Model -> Return
updateDialog dialog model =
    pure { model | dialog = dialog }
        |> effect cacheEffect


cacheEffect : Model -> Cmd msg
cacheEffect model =
    Ports.setCache (cacheEncoder (cacheFromModel model))


patchTodoCmd : TodoId -> Todo.Msg -> Cmd Msg
patchTodoCmd todoId todoMsg =
    PatchTodo todoId todoMsg |> Millis.nowCmd


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
    JD.decodeValue flagsDecoder encodedFlags
        |> Result.Extra.unpack onDecodeError updateFromFlags
        |> callWith model


updateFromFlags : Flags -> Model -> Return
updateFromFlags flags model =
    setTodoList flags.cachedTodoList model
        |> setProjectList flags.cachedProjectList
        |> setAuthState flags.cachedAuthState
        |> setModelFromCache flags.cache
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
        todoByPid : Dict ProjectId (List Todo)
        todoByPid =
            Dict.Extra.groupBy .projectId model.todoList

        deleteProjectsCmd =
            model.projectList
                |> List.filter .deleted
                |> List.filter
                    (\p ->
                        Dict.get p.id todoByPid |> Maybe.Extra.unwrap True List.isEmpty
                    )
                |> List.map
                    (.id
                        >> (\projectId ->
                                Ports.deleteFirestoreDoc { userDocPath = "projects/" ++ projectId }
                           )
                    )
                |> Cmd.batch

        deleteTodosCmd : Cmd msg
        deleteTodosCmd =
            model.projectList
                |> List.filter .deleted
                |> List.filterMap (\p -> Dict.get p.id todoByPid)
                |> List.concat
                |> List.map
                    (.id
                        >> (\todoId ->
                                Ports.deleteFirestoreDoc
                                    { userDocPath = "todos/" ++ todoId }
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
                    Cmd.none
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
        |> toUnStyledDocument


type alias StyledDocument msg =
    { title : String, body : List (Html msg) }


toUnStyledDocument : StyledDocument msg -> Browser.Document msg
toUnStyledDocument { title, body } =
    { title = title, body = body |> List.map Html.Styled.toUnstyled }


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
                    model.inlineEditTodo
                    model.here
                    displayTodoList
                )
                model

        Route.Project pid ->
            case
                model.projectList
                    |> Project.filterActive
                    |> List.Extra.find (.id >> (==) pid)
            of
                Just project ->
                    let
                        displayTodoList =
                            sortedInProject pid model.todoList

                        title =
                            project.title
                    in
                    masterLayout title
                        (pendingForProjectContent project.id
                            title
                            model.inlineEditTodo
                            model.here
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


sortedInProject pid todoList =
    Todo.filterSort
        (Todo.BelongsToProject pid)
        [ Todo.ByIdx
        , Todo.ByRecentlyModifiedProjectId
        , Todo.ByRecentlyCreated
        ]
        todoList



{- sortedPendingInProject pid todoList =
   Todo.filterSort
       (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))
       [ Todo.ByIdx
       , Todo.ByRecentlyModifiedProjectId
       , Todo.ByRecentlyCreated
       ]
       todoList
-}
-- MASTER LAYOUT


masterLayout : String -> Html Msg -> Model -> StyledDocument Msg
masterLayout title content model =
    let
        sidebarWidth =
            px 250

        negativeSidebarWidth =
            px -250

        headerHeight =
            rem 2

        maxContentWidth =
            px 1024

        viewDebugContent =
            div [ class "pa3 vs3" ]
                [ HasErrors.detailView model
                , div [ class " flex hs3" ]
                    [ div [ class "ttu tracked" ] [ text "AuthState:" ]
                    , AuthState.view model.authState
                    ]
                ]

        bpSmall =
            600

        sm =
            withMedia
                [ Media.all [ Media.maxWidth <| px bpSmall ] ]

        ns =
            withMedia
                [ Media.all [ Media.minWidth <| px (bpSmall + 1) ] ]
    in
    { title = title
    , body =
        [ div
            [ class "bg-black white"
            , css [ position sticky, top zero, height headerHeight ]
            ]
            [ div [ class "center", css [ maxWidth maxContentWidth ] ]
                [ viewHeader model ]
            ]
        , div [ class "center", css [ maxWidth maxContentWidth ] ]
            [ div
                [ class "fixed overflow-auto ph3"
                , css
                    [ width sidebarWidth
                    , top headerHeight
                    , bottom zero
                    , sm
                        [ transforms [ translateX <| negativeSidebarWidth ] ]
                    , transition [ Transition.transform 1000 ]
                    ]
                ]
                [ viewSidebar model
                ]
            , div
                [ class "ph3"
                , css
                    [ marginLeft zero
                    , ns
                        [ marginLeft sidebarWidth ]
                    , transition [ Transition.marginLeft 1000 ]
                    ]
                ]
                [ content
                , viewDebugContent
                ]
            ]
        , viewFooter model
        ]
    }



-- LAYOUT SIDEBAR


viewSidebar model =
    div []
        [ viewNav model
        ]



-- LAYOUT HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "vs3" ]
        [ div [ class "flex ph3 h2 items-center" ]
            [ div [ class "f4 tracked flex-grow-1" ] [ text "ElmDOist" ]
            , case model.authState of
                AuthState.Unknown ->
                    button [ disabled True ] [ text "SignIn" ]

                AuthState.SignedIn user ->
                    div [ class "flex items-center hs3 " ]
                        [ div [] [ text user.displayName ]
                        , button [ onClick OnSignOutClicked ] [ text "SignOut" ]
                        ]

                AuthState.NotSignedIn ->
                    button [ onClick OnSignInClicked ] [ text "SignIn" ]
            ]
        ]


viewNav : Model -> Html Msg
viewNav model =
    let
        navItem link title =
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
    in
    div [ class "lh-title" ]
        [ navItem Route.inboxUrl "Inbox"
        , navItem Route.todayUrl "Today"
        , div [ class "pv2 flex hs3" ]
            [ div [ class "ttu tracked flex-grow-1" ] [ text "Projects:" ]
            , viewCharBtn OnAddProjectStart '+'
            ]
        , viewNavProjects (Project.filterActive model.projectList)
        ]


viewNavProjects : ProjectList -> Html Msg
viewNavProjects projectList =
    div [ class "b " ] (List.map viewProjectNavItem projectList)


viewProjectNavItem : Project -> Html Msg
viewProjectNavItem project =
    div [ class "pv2 flex hs3" ]
        [ a
            [ class "no-underline truncate flex-grow-1"
            , href (Route.projectUrl project.id)
            ]
            [ text project.title ]
        , viewCharBtn (OnDeleteProject project.id) 'X'
        ]



-- LAYOUT FOOTER & DIALOG


viewFooter : Model -> Html Msg
viewFooter model =
    div []
        [ case model.dialog of
            NoDialog ->
                HtmlStyledExtra.empty

            MoveToProjectDialog todo ->
                viewMoveDialog todo (Project.filterActive model.projectList)

            DueDialog todo ->
                viewDueDialog model.here model.now todo
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


viewMoveDialog : Todo -> ProjectList -> Html Msg
viewMoveDialog todo projectList =
    let
        viewPLI dp =
            div
                [ tabindex 0
                , class "lh-copy pa2 pointer"
                , classList [ ( "b", dp.id == todo.projectId ) ]
                , onClick (OnMoveToProject dp.id)
                ]
                [ div [] [ text dp.title ] ]
    in
    viewDialogOverlay
        [ div [ class "bg-white vs3 pa3" ]
            [ div [ class "b" ] [ text "Move To Project ..." ]
            , div [ class "vs1" ]
                (projectList
                    |> toDisplayProjectList
                    |> List.map viewPLI
                )
            ]
        ]


viewDueDialog : Time.Zone -> Millis -> Todo -> Html Msg
viewDueDialog zone now _ =
    let
        today : Calendar.Date
        today =
            dateFromMillis now

        todayFmt =
            Millis.formatDate "ddd MMM yyyy" zone now

        yesterday =
            Calendar.decrementDay today

        yesterdayFmt =
            Millis.formatDate "ddd MMM yyyy" zone (yesterday |> Calendar.toMillis)
    in
    viewDialogOverlay
        [ div [ class "bg-white vs3 pa3" ]
            [ div [ class "b" ] [ text "Set Due Date.." ]
            , div
                [ class "pa3 b pointer"
                , onClick (OnSetDue <| Calendar.toMillis today)
                ]
                [ text <| "Today: " ++ todayFmt ]
            , div
                [ class "pa3 b pointer"
                , onClick (OnSetDue <| Calendar.toMillis yesterday)
                ]
                [ text <| "Yesterday: " ++ yesterdayFmt ]
            ]
        ]


viewDialogOverlay : List (Html Msg) -> Html Msg
viewDialogOverlay =
    div
        [ class "fixed absolute--fill bg-black-50"
        , class "flex items-center justify-center "
        , Html.Styled.Attributes.id "overlay"
        , onDomIdClicked "overlay" OnDialogOverlayClicked
        ]



-- TODAY PAGE CONTENT


dateFromMillis : Int -> Calendar.Date
dateFromMillis =
    Time.millisToPosix >> Calendar.fromPosix


eqByDay m1 m2 =
    dateFromMillis m1 == dateFromMillis m2


compareDate m1 m2 =
    Calendar.compare (dateFromMillis m1) (dateFromMillis m2)


todayContent : Model -> Html Msg
todayContent model =
    let
        now =
            model.now

        displayTodoList =
            List.filter (.dueAt >> Maybe.Extra.unwrap False (eqByDay now))
                model.todoList
                |> List.filter (.isDone >> not)

        overDueList =
            List.filter
                (.dueAt
                    >> Maybe.Extra.unwrap False
                        (\dueAt -> compareDate dueAt now == LT)
                )
                model.todoList
                |> List.filter (.isDone >> not)
    in
    div [ class "vs3" ]
        [ HtmlStyledExtra.viewUnless (overDueList |> List.isEmpty) <|
            div [ class "ph3 vs3" ]
                [ div [ class "flex items-center hs3" ]
                    [ div [ class "b flex-grow-1" ] [ text "Overdue" ]
                    ]
                , div [ class "vs1" ]
                    (List.map
                        (viewTodoItem
                            model.inlineEditTodo
                            model.here
                        )
                        overDueList
                    )
                ]
        , div [ class "ph3 vs3" ]
            [ div [ class "flex items-center hs3" ]
                [ div [ class "b flex-grow-1" ] [ text "Today" ]
                , button [ onClick OnAddTodoTodayStart ] [ text "add task" ]
                ]
            , div [ class "vs1" ]
                (List.map
                    (viewTodoItem
                        model.inlineEditTodo
                        model.here
                    )
                    displayTodoList
                )
            ]
        ]



-- TodoListPageContent


pendingForProjectContent :
    ProjectId
    -> String
    -> Maybe InlineEditTodo
    -> Time.Zone
    -> TodoList
    -> Html Msg
pendingForProjectContent pid title edit here displayTodoList =
    div [ class "vs3" ]
        [ div [ class "pv2 flex items-center hs3" ]
            [ div [ class "b flex-grow-1" ] [ text title ]
            , button [ onClick (OnAddTodoStart pid) ] [ text "add task" ]
            ]
        , div [ class "vs1" ] (List.map (viewTodoItem edit here) displayTodoList)
        ]


viewTodoItem : Maybe InlineEditTodo -> Time.Zone -> Todo -> Html Msg
viewTodoItem edit here todo =
    case edit of
        Nothing ->
            viewTodoItemHelp here todo

        Just edt ->
            if edt.todo.id == todo.id then
                viewEditTodoItem edt

            else
                viewTodoItemHelp here todo


viewEditTodoItem : InlineEditTodo -> Html Msg
viewEditTodoItem edt =
    let
        titleValue =
            edt.title
                |> Maybe.withDefault edt.todo.title
    in
    div
        [ class ""
        , tabindex 0
        ]
        [ div []
            [ input [ type_ "text", value titleValue ] []
            ]
        , div [ class "flex hs3" ]
            [ button [ onClick OnEditCancel ] [ text "Cancel" ]
            , button [ onClick OnEditSave ] [ text "Save" ]
            ]
        ]


viewTodoItemHelp here todo =
    div
        [ class "flex items-center hs1 lh-copy db "
        , tabindex 0
        ]
        [ viewCheck todo.isDone (OnChecked todo.id)
        , viewDueAt here todo
        , viewTodoTitle todo
        , viewCharBtn (OnDelete todo.id) 'X'
        , viewCharBtn (OnMoveStart todo.id) 'M'
        , viewCharBtn (OnEditDueStart todo.id) 'D'
        ]


viewDueAt here todo =
    let
        viewDueAt_ dueAt =
            div [ class "truncate flex-shrink-0 f7 code" ]
                [ text <| Millis.formatDate "ddd MMM" here dueAt
                ]
    in
    todo.dueAt
        |> Maybe.Extra.unwrap
            HtmlStyledExtra.empty
            viewDueAt_


viewCharBtn : msg -> Char -> Html msg
viewCharBtn clickHandler chr =
    div [ class "flex items-center" ]
        [ button [ onClick clickHandler, class "code" ] [ text <| String.fromChar chr ] ]


viewCheck isChecked onCheckMsg =
    div [ class "hover-bg-light-yellow flex flex-column pa2" ]
        [ input
            [ class "pointer db flex-grow-1"
            , type_ "checkbox"
            , checked isChecked
            , onCheck onCheckMsg
            ]
            []
        ]


viewTodoTitle todo =
    let
        ( title, titleClass ) =
            if String.trim todo.title |> String.isEmpty then
                ( "<no title>", "i black-70" )

            else
                ( todo.title, "" )
    in
    div
        [ class
            (titleClass
                ++ " "
                ++ "flex-grow-1 pointer hover-bg-light-yellow lh-solid pa2"
            )
        , onClick (OnEdit todo.id)
        ]
        [ text title ]


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
