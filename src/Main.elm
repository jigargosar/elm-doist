module Main exposing (main)

import Accessibility.Styled.Key as Key
import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith, eq_, ifElse)
import Browser
import Browser.Dom as Dom exposing (focus)
import Browser.Navigation as Nav
import Calendar
import Css exposing (bottom, height, marginLeft, maxWidth, minHeight, minWidth, none, outline, position, px, rem, sticky, top, transforms, translateX, vh, vw, width, zero)
import Css.Media as Media exposing (withMedia)
import Css.Transitions as Transition exposing (transition)
import Dict exposing (Dict)
import Dict.Extra
import Errors exposing (Errors)
import Focus
import FontAwesome.Attributes
import FontAwesome.Icon as FAIcon
import FontAwesome.Solid
import FontAwesome.Styles
import HasErrors
import Html.Styled as H exposing (Attribute, Html, a, button, div, input, text)
import Html.Styled.Attributes as A
    exposing
        ( checked
        , class
        , classList
        , css
        , disabled
        , href
        , tabindex
        , type_
        , value
        )
import Html.Styled.Events exposing (onCheck, onClick, preventDefaultOn)
import HtmlStyledEvent exposing (onDomIdClicked)
import HtmlStyledExtra as HX exposing (viewMaybe)
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
import Size exposing (Size)
import String.Extra as SX
import Task
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)



-- MODEL


type Dialog
    = NoDialog
    | MoveToProjectDialog Todo
    | DueDialog Todo


type alias TodoMenu =
    { todoId : TodoId }


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , inlineEditTodo : Maybe InlineEditTodo.Model
    , todoMenu : Maybe TodoMenu
    , dialog : Dialog
    , authState : AuthState
    , errors : Errors
    , key : Nav.Key
    , route : Route
    , now : Millis
    , today : Calendar.Date
    , here : Time.Zone
    , browserSize : Size
    }


type alias Cache =
    { dialog : Dialog
    , inlineEditTodo : Maybe InlineEditTodo.Model
    }


type alias Flags =
    { cachedTodoList : TodoList
    , cachedProjectList : ProjectList
    , cachedAuthState : AuthState
    , browserSize : Size
    , now : Millis
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
        |> JDP.optional "inlineEditTodo" (JD.maybe InlineEditTodo.decoder) Nothing


cacheEncoder : Cache -> Value
cacheEncoder { dialog, inlineEditTodo } =
    JE.object
        [ ( "dialog", dialogEncoder dialog )
        , ( "inlineEditTodo", InlineEditTodo.maybeEncoder inlineEditTodo )
        ]


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


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "cachedTodoList" (JD.oneOf [ Todo.listDecoder, JD.null [] ])
        |> JDP.required "cachedProjectList" (JD.oneOf [ Project.listDecoder, JD.null [] ])
        |> JDP.required "cachedAuthState"
            (JD.oneOf [ AuthState.decoder, JD.null AuthState.initial ])
        |> JDP.required "browserSize" Size.decoder
        |> JDP.required "now" JD.int
        |> JDP.required "cache" cacheDecoder



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
            , todoMenu = Nothing
            , dialog = NoDialog
            , authState = AuthState.initial
            , errors = Errors.fromStrings []
            , key = key
            , route = route
            , now = now
            , today = dateFromMillis now
            , here = Time.utc
            , browserSize = Size.initial
            }
    in
    model
        |> pure
        |> andThen (updateFromEncodedFlags encodedFlags)
        |> command (Millis.nowCmd OnNow)
        |> command (Millis.hereCmd OnHere)
        |> command (Dom.getViewport |> Task.perform GotViewport)



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnNow Millis
    | OnHere Time.Zone
    | GotViewport Dom.Viewport
    | OnBrowserResize Size
    | Focus String
    | Focused (Result Dom.Error ())
    | OnAuthStateChanged Value
    | OnTodoListChanged Value
    | OnFirestoreQueryResponse FirestoreQueryResponse
    | OnSignInClicked
    | OnSignOutClicked
    | OnChangeTitleRequested TodoId
    | OnChecked TodoId Bool
    | OnDelete TodoId
    | OnDeleteProject ProjectId
    | PatchTodo TodoId (List Todo.Msg) Millis
    | OnAddTodoStart ProjectId
    | OnAddTodoTodayStart
    | AddTodoToday Millis
    | AddTodo ProjectId Millis
    | OnAddProjectStart
    | AddProject Millis
    | OnMoveStart TodoId
    | OnEditDueStart TodoId
    | OnTodoMenuClicked TodoId
    | CloseTodoMenu TodoId Bool
    | OnSetDue DueAt
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
        , Size.onBrowserResize OnBrowserResize
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
            ( { model | route = route }, {- queryTodoListForRouteCmd route -} Cmd.none )

        OnNow now ->
            pure { model | now = now }

        OnHere here ->
            pure { model | here = here }

        GotViewport domVP ->
            pure { model | browserSize = Size.fromViewport domVP.viewport }

        OnBrowserResize size ->
            setBrowserSize size model |> pure

        Focus domId ->
            ( model, focus domId |> Task.attempt Focused )

        Focused _ ->
            pure model

        OnAuthStateChanged encodedValue ->
            JD.decodeValue AuthState.decoder encodedValue
                |> RX.unpack onDecodeError onAuthStateChanged
                |> callWith model

        OnTodoListChanged encodedValue ->
            JD.decodeValue Todo.listDecoder encodedValue
                |> RX.unpack onDecodeError updateTodoListFromFirestore
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
                        |> RX.combine
                        |> RX.unpack onDecodeError updateTodoListFromFirestore
                        |> callWith model

                "projectList" ->
                    qs.docDataList
                        |> List.map (JD.decodeValue Project.decoder)
                        |> RX.combine
                        |> RX.unpack onDecodeError updateProjectListAndCleanupFromFirestore
                        |> callWith model

                _ ->
                    HasErrors.prependString ("Invalid QueryId" ++ qs.id) model
                        |> pure

        OnChecked todoId checked ->
            ( model, patchTodoCmd todoId [ Todo.SetCompleted checked ] )

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

        PatchTodo todoId todoMsgList now ->
            ( model
            , Ports.updateFirestoreDoc
                { userDocPath = "todos/" ++ todoId
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
                |> MX.unwrap pure startEditing
                |> callWith model

        OnMoveStart todoId ->
            model.todoList
                |> List.Extra.find (.id >> (==) todoId)
                |> MX.unwrap pure startMoving
                |> callWith model

        OnEditDueStart todoId ->
            model.todoList
                |> List.Extra.find (.id >> (==) todoId)
                |> MX.unwrap pure startEditingDue
                |> callWith model

        OnTodoMenuClicked todoId ->
            let
                tm =
                    { todoId = todoId }
            in
            pure { model | todoMenu = Just tm }
                |> command (focusTodoMenuCmd todoId)

        CloseTodoMenu todoId restoreFocus ->
            model.todoMenu
                |> MX.filter (.todoId >> eq_ todoId)
                |> MX.unpack (\_ -> pure model)
                    (\_ ->
                        ( { model | todoMenu = Nothing }
                        , ifElse restoreFocus
                            (focusDomIdCmd <| todoMenuTriggerDomId todoId)
                            Cmd.none
                        )
                    )

        OnMoveToProject pid ->
            case model.dialog of
                NoDialog ->
                    pure model

                MoveToProjectDialog todo ->
                    updateDialogAndCache NoDialog model
                        |> command
                            (patchTodoCmd
                                todo.id
                                [ Todo.SetProjectId pid ]
                            )

                DueDialog _ ->
                    pure model

        OnSetDue dueAt ->
            case ( model.inlineEditTodo, model.dialog ) of
                ( Nothing, DueDialog todo ) ->
                    updateDialogAndCache NoDialog model
                        |> command
                            (patchTodoCmd
                                todo.id
                                [ Todo.SetDueAt dueAt ]
                            )

                ( Just _, DueDialog _ ) ->
                    model
                        |> mapInlineEditTodo
                            (InlineEditTodo.setDueAt dueAt)
                        |> updateDialogAndCache NoDialog

                _ ->
                    pure model

        OnDialogOverlayClicked ->
            case model.dialog of
                NoDialog ->
                    pure model

                MoveToProjectDialog _ ->
                    updateDialogAndCache NoDialog model

                DueDialog _ ->
                    updateDialogAndCache NoDialog model

        OnEditCancel ->
            updateInlineEditTodoAndCache Nothing model

        OnEditSave ->
            model.inlineEditTodo
                |> Maybe.andThen InlineEditTodo.toUpdateMessages
                |> MX.unpack (\_ -> pure model)
                    (\( todo, todoUpdateMsgList ) ->
                        model
                            |> updateInlineEditTodoAndCache Nothing
                            |> command (patchTodoCmd todo.id todoUpdateMsgList)
                    )


todoMenuDomId todoId =
    "todo-menu-dom-id--" ++ todoId


todoMenuTriggerDomId todoId =
    "todo-menu-trigger-dom-id--" ++ todoId


todoFirstFocusableDomId todoId =
    "todo-menu--first-focusable--dom-id--" ++ todoId


focusTodoMenuCmd todoId =
    let
        domId =
            todoFirstFocusableDomId todoId
    in
    focus domId |> Task.attempt Focused


focusDomIdCmd : String -> Cmd Msg
focusDomIdCmd domId =
    focus domId |> Task.attempt Focused


updateInlineEditTodoAndCache : Maybe InlineEditTodo.Model -> Model -> Return
updateInlineEditTodoAndCache inlineEditTodo model =
    setInlineEditTodo inlineEditTodo model
        |> pure
        |> effect cacheEffect


setInlineEditTodo : Maybe InlineEditTodo.Model -> Model -> Model
setInlineEditTodo inlineEditTodo model =
    { model | inlineEditTodo = inlineEditTodo }


mapInlineEditTodo : (InlineEditTodo.Model -> InlineEditTodo.Model) -> Model -> Model
mapInlineEditTodo mfn model =
    setInlineEditTodo (Maybe.map mfn model.inlineEditTodo) model


startEditing : Todo -> Model -> Return
startEditing todo =
    updateInlineEditTodoAndCache <| Just <| InlineEditTodo.fromTodo todo


startMoving : Todo -> Model -> Return
startMoving todo =
    updateDialogAndCache (MoveToProjectDialog todo)


startEditingDue : Todo -> Model -> Return
startEditingDue todo =
    updateDialogAndCache (DueDialog todo)


updateDialogAndCache : Dialog -> Model -> Return
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
    JD.decodeValue flagsDecoder encodedFlags
        |> RX.unpack onDecodeError updateFromFlags
        |> callWith model


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
        todoByPid : Dict ProjectId (List Todo)
        todoByPid =
            Dict.Extra.groupBy .projectId model.todoList

        deleteProjectsCmd =
            model.projectList
                |> List.filter .deleted
                |> List.filter
                    (\p ->
                        Dict.get p.id todoByPid |> MX.unwrap True List.isEmpty
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
        |> (\{ title, body } ->
                { title = title
                , body =
                    [ H.toUnstyled <|
                        div
                            [ A.id "root"
                            , css
                                [ minHeight <| vh 100
                                , minWidth <| vw 100
                                , outline none
                                ]
                            ]
                            body
                    ]
                }
                    |> prependFontAwesomeCss
           )


prependFontAwesomeCss : Browser.Document Msg -> Browser.Document Msg
prependFontAwesomeCss doc =
    { doc | body = FontAwesome.Styles.css :: doc.body }


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
    let
        sidebarWidthNum =
            250

        maxContentWidthNum =
            800

        headerHeight =
            rem 2

        maxContentWidth =
            px maxContentWidthNum

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
            [ div
                [ class "center", css [ maxWidth maxContentWidth ] ]
                [ viewHeader model ]
            ]
        , div [ class "center", css [ maxWidth maxContentWidth ] ]
            [ div
                [ class "fixed overflow-auto ph3"
                , css
                    [ width (px sidebarWidthNum)
                    , top headerHeight
                    , bottom zero
                    , sm
                        [ transforms [ translateX <| px -sidebarWidthNum ] ]
                    , transition [ Transition.transform 1000 ]
                    ]
                ]
                [ viewSidebar model
                ]
            , div
                [ class "ph3"
                , css
                    [ marginLeft zero
                    , ns [ marginLeft <| px sidebarWidthNum ]
                    , transition [ Transition.marginLeft 1000 ]
                    ]
                ]
                [ content
                , viewDebugContent model
                ]
            ]
        , viewFooter model
        ]
    }


viewDebugContent model =
    div [ class "pa3 vs3" ]
        [ HasErrors.detailView model
        , div [ class " flex hs3" ]
            [ div [ class "ttu tracked" ] [ text "AuthState:" ]
            , AuthState.view model.authState
            ]
        ]



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
            , faBtn OnAddProjectStart FontAwesome.Solid.plus []
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
        , faBtn (OnDeleteProject project.id) FontAwesome.Solid.trash []
        ]


faBtn : msg -> FAIcon.Icon -> List (Attribute msg) -> Html msg
faBtn action icon attrs =
    btn action
        ([ class "gray hover-dark-gray pointer"
         ]
            ++ attrs
        )
        [ icon
            |> FAIcon.viewStyled [ FontAwesome.Attributes.lg ]
            |> H.fromUnstyled
        ]


btn : msg -> List (Attribute msg) -> List (Html msg) -> Html msg
btn action attrs =
    let
        msg =
            ( action, True )
    in
    div
        ([ onClick action
         , preventDefaultOn "keydown" <|
            JD.oneOf [ Key.enter msg, Key.space msg ]
         , tabindex 0
         , A.attribute "role" "button"
         ]
            ++ attrs
        )



-- LAYOUT FOOTER & DIALOG


viewFooter : Model -> Html Msg
viewFooter model =
    div []
        [ case model.dialog of
            NoDialog ->
                HX.empty

            MoveToProjectDialog todo ->
                viewMoveDialog todo (Project.filterActive model.projectList)

            DueDialog todo ->
                viewDueDialog model.here model.now todo
        ]



--isOutsideElementWithIdDecoder : String -> Decoder Bool
--isOutsideElementWithIdDecoder dropdownId =
--    JD.oneOf
--        [ JD.field "id" JD.string
--            |> JD.andThen
--                (\id ->
--                    if dropdownId == id then
--                        -- found match by id
--                        JD.succeed False
--
--                    else
--                        -- try next decoder
--                        JD.fail "continue"
--                )
--        , JD.lazy (\_ -> isOutsideElementWithIdDecoder dropdownId |> JD.field "parentNode")
--
--        -- fallback if all previous decoders failed
--        , JD.succeed True
--        ]
--
--
--isRelatedTargetOutsideOfElWithId elId =
--    JD.oneOf
--        [ JD.field "relatedTarget" (JD.null False)
--        , JD.field "relatedTarget" (isOutsideElementWithIdDecoder elId)
--        ]
--


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
                , onClick (OnSetDue <| Todo.NoDue)
                ]
                [ text <| "No Due Date" ]
            , div
                [ class "pa3 b pointer"
                , onClick (OnSetDue <| Todo.DueAt <| Calendar.toMillis today)
                ]
                [ text <| "Today: " ++ todayFmt ]
            , div
                [ class "pa3 b pointer"
                , onClick (OnSetDue <| Todo.DueAt <| Calendar.toMillis yesterday)
                ]
                [ text <| "Yesterday: " ++ yesterdayFmt ]
            ]
        ]


viewDialogOverlay : List (Html Msg) -> Html Msg
viewDialogOverlay =
    div
        [ class "fixed absolute--fill bg-black-50"
        , class "flex items-center justify-center "
        , A.id "overlay"
        , onDomIdClicked "overlay" OnDialogOverlayClicked
        ]



-- TODAY PAGE CONTENT


dateFromMillis : Int -> Calendar.Date
dateFromMillis =
    Time.millisToPosix >> Calendar.fromPosix


todayContent : Model -> Html Msg
todayContent model =
    let
        now =
            model.now

        nowDate =
            dateFromMillis now

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
    div [ class "vs3" ]
        [ overDueList
            |> HX.viewNotEmpty
                (\_ ->
                    div [ class "ph3 vs3" ]
                        [ div [ class "flex items-center hs3" ]
                            [ div [ class "b flex-grow-1" ] [ text "Overdue" ]
                            ]
                        , div [ class "" ]
                            (List.map
                                (viewTodoItem model)
                                overDueList
                            )
                        ]
                )
        , div [ class "ph3 vs3" ]
            [ div [ class "flex items-center hs3" ]
                [ div [ class "b flex-grow-1" ] [ text "Today" ]
                , button [ onClick OnAddTodoTodayStart ] [ text "add task" ]
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
    div [ class "vs3" ]
        [ div [ class "pv2 flex items-center hs3" ]
            [ div [ class "b flex-grow-1" ] [ text title ]
            , button [ onClick (OnAddTodoStart pid) ] [ text "add task" ]
            ]
        , div [ class "" ] (List.map (viewTodoItem model) displayTodoList)
        ]


viewTodoItem :
    { a
        | inlineEditTodo : Maybe InlineEditTodo.Model
        , here : Zone
        , todoMenu : Maybe TodoMenu
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
            (viewEditTodoItem here)


viewEditTodoItem : Time.Zone -> InlineEditTodo.Model -> Html Msg
viewEditTodoItem here edt =
    let
        titleValue =
            InlineEditTodo.titleOrDefault edt

        dueAtValue =
            InlineEditTodo.dueAtOrDefault edt
                |> Todo.dueAtToMillis
    in
    div
        [ class "pa3 vs3"
        , tabindex 0
        ]
        [ div [ class "flex" ]
            [ input
                [ class "pa1 flex-grow-1 lh-copy"
                , type_ "text"
                , value titleValue
                ]
                []
            ]
        , div [ class "flex items-center" ]
            [ div
                [ class "flex items-center hs3"
                , class "pointer"
                , onClick <| OnEditDueStart <| InlineEditTodo.todoId edt
                ]
                [ dueAtValue
                    |> MX.unpack
                        (\_ -> div [] [ text "No Due Date" ])
                        (\mi ->
                            div [ class "" ]
                                [ text "Due: "
                                , text <| Millis.formatDate "ddd MMM" here <| mi
                                ]
                        )
                , div [ class "pointer underline blue" ] [ text "edit" ]
                ]
            , div [ class "flex-grow-1" ] []
            , div [ class "flex flex-row-reverse justify-start" ]
                [ button [ class "ml3", onClick OnEditSave ] [ text "Save" ]
                , button [ class "ml3", onClick OnEditCancel ] [ text "Cancel" ]
                ]
            ]
        ]


viewTodoItemBase :
    { a
        | here : Zone
        , todoMenu : Maybe { b | todoId : TodoId }
    }
    -> Todo
    -> Html Msg
viewTodoItemBase { here, todoMenu } todo =
    div
        [ class "pa2 flex items-center hs1 lh-copy db "
        ]
        [ viewCheck todo.isDone (OnChecked todo.id)
        , viewDueAt here todo
        , viewTodoItemContent here todo
        , div [ class "relative" ]
            [ faBtn (OnTodoMenuClicked todo.id)
                FontAwesome.Solid.ellipsisV
                [ A.id <| todoMenuTriggerDomId todo.id ]
            , todoMenu
                |> MX.filter (.todoId >> eq_ todo.id)
                |> HX.viewMaybe
                    (\_ -> viewTodoMenu todo)
            ]
        ]


viewTodoMenu : { a | id : TodoId } -> Html Msg
viewTodoMenu todo =
    let
        miModel =
            [ ( OnDelete, "Delete" )
            , ( OnMoveStart, "Move to..." )
            , ( OnEditDueStart, "Change Due At..." )
            ]

        menuItemsViewList =
            miModel
                |> List.map (Tuple.mapFirst <| callWith todo.id)
                |> List.indexedMap
                    (\idx ( msg, label ) ->
                        btn msg
                            [ A.id <|
                                ifElse (idx == 0)
                                    (todoFirstFocusableDomId todo.id)
                                    ""
                            ]
                            [ text label ]
                    )

        menuDomId =
            todoMenuDomId todo.id

        closeMsg : Bool -> Msg
        closeMsg =
            CloseTodoMenu todo.id
    in
    div
        [ A.id menuDomId
        , class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , Focus.onFocusOutsideDomId menuDomId
            (\{ hasRelatedTarget } -> closeMsg (not hasRelatedTarget))
        , preventDefaultOn "keydown" (Key.escape ( closeMsg True, True ))
        ]
        menuItemsViewList


viewDueAt : Zone -> Todo -> Html msg
viewDueAt here todo =
    todo
        |> Todo.dueMilli
        |> HX.viewMaybe
            (\dueMillis ->
                div [ class "dn truncate flex-shrink-0 f7 code" ]
                    [ text <| Millis.formatDate "ddd MMM" here dueMillis
                    ]
            )


viewCheck isChecked onCheckMsg =
    div
        [ class "flex-shrink-0 hover-bg-light-yellow "
        , css [ width <| px 32, height <| px 32 ]
        ]
        [ input
            [ class "pointer w-100 h-100"
            , type_ "checkbox"
            , checked isChecked
            , onCheck onCheckMsg
            ]
            []
        ]


viewTodoItemContent : Time.Zone -> Todo -> Html Msg
viewTodoItemContent here todo =
    let
        ( title, titleClass ) =
            ifElse (SX.isBlank todo.title)
                ( "<no title>", "i black-70" )
                ( todo.title, "" )

        dueAtText =
            todo |> Todo.dueMilli |> Maybe.map (Millis.formatDate "dd MMM" here)

        viewDueAtInline : String -> Html Msg
        viewDueAtInline txt =
            div [ class "di mr2 pointer ", onClick <| OnEditDueStart todo.id ]
                [ div
                    [ class "ttu f7 lh-solid dib code br-pill ba ph2 pv1"
                    , class "bg-red white"
                    ]
                    [ text txt ]
                ]

        viewTitle =
            div [ class "di viewDueAtInline", onClick (OnEdit todo.id) ] [ text title ]
    in
    div
        [ class
            (titleClass
                ++ " "
                ++ "flex-grow-1 pointer hover-bg-light-yellow lh-solid pa2"
            )
        ]
        [ viewMaybe viewDueAtInline dueAtText, viewTitle ]



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
