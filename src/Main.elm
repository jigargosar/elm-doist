module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Dict.Extra
import HasErrors
import Html.Styled exposing (Html, a, button, div, input, text)
import Html.Styled.Attributes
    exposing
        ( checked
        , class
        , classList
        , disabled
        , href
        , tabindex
        , type_
        , value
        )
import Html.Styled.Events exposing (onCheck, onClick)
import HtmlStyledEvent exposing (onDomIdClicked)
import HtmlStyledExtra
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Maybe.Extra
import Now exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Result.Extra
import Return
import Route exposing (Route)
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)


type alias Error =
    String


type alias InlineEditTodo =
    { todo : Todo, title : Maybe String }


type Dialog
    = NoDialog
    | MoveToProjectDialog Todo


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , inlineEditTodo : Maybe InlineEditTodo
    , dialog : Dialog
    , authState : AuthState
    , errors : List Error
    , key : Nav.Key
    , route : Route
    }


type alias Return =
    Return.Return Msg Model


type alias Cache =
    { dialog : Dialog
    }


type alias Flags =
    { cachedTodoList : TodoList
    , cachedProjectList : ProjectList
    , cachedAuthState : AuthState
    , cache : Cache
    }


dialogDecoderForTag : String -> Decoder Dialog
dialogDecoderForTag tag =
    case tag of
        "NoDialog" ->
            JD.succeed NoDialog

        "MoveToProjectDialog" ->
            JD.field "todo" Todo.decoder
                |> JD.map MoveToProjectDialog

        _ ->
            JD.fail ("Invalid Dialog Tag:" ++ tag)


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
            , errors = []
            , key = key
            , route = route
            }
    in
    model
        |> pure
        |> andThen (updateFromEncodedFlags encodedFlags)


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
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
    | AddTodo ProjectId Millis
    | OnAddProjectStart
    | AddProject Millis
    | OnMoveStart TodoId
    | OnMoveToProject ProjectId
    | OnOverlayClicked
    | OnEdit TodoId
    | OnEditCancel
    | OnEditSave



--


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
                        |> Result.Extra.unpack onDecodeError setAndCacheProjectList
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
            ( model, Now.perform (AddTodo pid) )

        AddTodo pid now ->
            ( model
            , Ports.addFirestoreDoc
                { userCollectionName = "todos"
                , data = Todo.new now pid
                }
            )

        OnAddProjectStart ->
            ( model, Now.perform AddProject )

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

        OnOverlayClicked ->
            case model.dialog of
                NoDialog ->
                    pure model

                MoveToProjectDialog _ ->
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


updateDialog : Dialog -> Model -> Return
updateDialog dialog model =
    pure { model | dialog = dialog }
        |> effect cacheEffect


cacheEffect : Model -> Cmd msg
cacheEffect model =
    Ports.setCache (cacheEncoder (cacheFromModel model))


patchTodoCmd : TodoId -> Todo.Msg -> Cmd Msg
patchTodoCmd todoId todoMsg =
    PatchTodo todoId todoMsg |> Now.perform


queryTodoListCmd =
    Ports.queryFirestore
        { id = "todoList"
        , userCollectionName = "todos"
        , whereClause = []
        }



--queryPendingTodoListCmd =
--    Ports.queryFirestore
--        { id = "todoList"
--        , userCollectionName = "todos"
--        , whereClause = [ ( "isDone", "==", JE.bool False ) ]
--        }
--queryTodoListForRouteCmd : Route -> Cmd msg
--queryTodoListForRouteCmd route =
--    let
--        getPid r =
--            case r of
--                Route.Inbox ->
--                    ""
--
--                Route.Project pid ->
--                    pid
--
--                Route.NotFound _ ->
--                    getPid Route.Inbox
--    in
--    Ports.queryFirestore
--        { id = "todoList"
--        , userCollectionName = "todos"
--        , whereClause =
--            [ ( "projectId", "==", getPid route |> ProjectId.encoder )
--            , ( "isDone", "==", JE.bool False )
--            ]
--        }


queryProjectListCmd =
    Ports.queryFirestore
        { id = "projectList"
        , userCollectionName = "projects"
        , whereClause =
            [{- ( "deleted", "==", JE.bool False ) -}]
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
        |> andThen cleanupTodoList


cleanupTodoList : Model -> Return
cleanupTodoList model =
    let
        todoByPid : Dict ProjectId (List Todo)
        todoByPid =
            Dict.Extra.groupBy .projectId model.todoList

        deleteTodosCmd : Cmd msg
        deleteTodosCmd =
            model.projectList
                |> List.filter .deleted
                |> List.filterMap (\p -> Dict.get p.id todoByPid)
                |> List.concat
                |> List.map
                    (.id
                        >> (\todoId ->
                                Ports.deleteFirestoreDoc { userDocPath = "todos/" ++ todoId }
                           )
                    )
                |> Cmd.batch
    in
    ( model, deleteTodosCmd )


setProjectList : ProjectList -> Model -> Model
setProjectList projectList model =
    { model | projectList = projectList }


setAndCacheProjectList : ProjectList -> Model -> Return
setAndCacheProjectList projectList model =
    setProjectList projectList model
        |> pure
        |> command
            (Ports.localStorageSetJsonItem
                ( "cachedProjectList", Project.listEncoder projectList )
            )


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged OnAuthStateChanged
        , Ports.onFirestoreQueryResponse OnFirestoreQueryResponse
        ]


view : Model -> Browser.Document Msg
view model =
    viewRoute model.route model
        |> toUnStyledDocument


type alias StyledDocument msg =
    { title : String, body : List (Html msg) }


toUnStyledDocument : StyledDocument msg -> Browser.Document msg
toUnStyledDocument { title, body } =
    { title = title, body = body |> List.map Html.Styled.toUnstyled }


viewFooter model =
    div []
        [ case model.dialog of
            NoDialog ->
                HtmlStyledExtra.empty

            MoveToProjectDialog todo ->
                viewMoveDialog todo (Project.filterActive model.projectList)
        ]


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject p =
    { id = p.id, title = p.title }


toDisplayProjectList projectList =
    { id = "", title = "Inbox" } :: List.map toDisplayProject projectList


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
    div
        [ class "absolute absolute--fill bg-black-50"
        , class "flex items-center justify-center "
        , Html.Styled.Attributes.id "overlay"
        , onDomIdClicked "overlay" OnOverlayClicked
        ]
        [ div [ class "bg-white vs3 pa3" ]
            [ div [ class "b" ] [ text "Move To Project ..." ]
            , div [ class "vs1" ]
                (projectList
                    |> toDisplayProjectList
                    |> List.map viewPLI
                )
            ]
        ]


masterLayout : String -> Html Msg -> Model -> StyledDocument Msg
masterLayout title content model =
    { title = title
    , body =
        [ viewHeader model
        , content
        , viewFooter model
        ]
    }


sortedPendingInProject pid todoList =
    Todo.filterSort
        (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))
        [ Todo.ByIdx
        , Todo.ByRecentlyModifiedProjectId
        , Todo.ByRecentlyCreated
        ]
        todoList



--inboxDisplayProject =
--    { id = ProjectId.default, title = "Inbox" }


viewRoute : Route -> Model -> StyledDocument Msg
viewRoute route model =
    case route of
        Route.Inbox ->
            let
                displayTodoList =
                    sortedPendingInProject ProjectId.default model.todoList

                title =
                    "Inbox"
            in
            masterLayout title
                (pendingForProjectContent ProjectId.default
                    title
                    model.inlineEditTodo
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
                            sortedPendingInProject pid model.todoList

                        title =
                            project.title
                    in
                    masterLayout title
                        (pendingForProjectContent project.id
                            title
                            model.inlineEditTodo
                            displayTodoList
                        )
                        model

                Nothing ->
                    viewRoute Route.Inbox model

        Route.NotFound _ ->
            viewRoute Route.Inbox model



-- TodoListPageContent


pendingForProjectContent :
    ProjectId
    -> String
    -> Maybe InlineEditTodo
    -> TodoList
    -> Html Msg
pendingForProjectContent pid title edit displayTodoList =
    div [ class "pa3 vs3" ]
        [ div [ class "flex items-center hs3" ]
            [ div [ class "b flex-grow-1" ] [ text title ]
            , button [ onClick (OnAddTodoStart pid) ] [ text "add task" ]
            ]
        , div [ class "vs1" ] (List.map (viewTodoItem edit) displayTodoList)
        ]


viewTodoItem : Maybe InlineEditTodo -> Todo -> Html Msg
viewTodoItem edit todo =
    case edit of
        Nothing ->
            viewTodoItemHelp todo

        Just edt ->
            if edt.todo.id == todo.id then
                viewEditTodoItem edt

            else
                viewTodoItemHelp todo


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


viewTodoItemHelp todo =
    div
        [ class "flex hs1 lh-copy db "
        , tabindex 0
        ]
        [ viewTodoCheck todo
        , viewTodoTitle todo
        , viewMoveTodoBtn todo
        , viewDeleteTodoBtn todo
        ]


viewTodoCheck todo =
    div [ class "hover-bg-light-yellow flex flex-column pa2" ]
        [ input
            [ class "pointer db flex-grow-1"
            , type_ "checkbox"
            , checked todo.isDone
            , onCheck (OnChecked todo.id)
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


viewMoveTodoBtn todo =
    div [ class "flex items-center" ]
        [ button [ onClick (OnMoveStart todo.id), class "code" ] [ text "M" ] ]


viewDeleteTodoBtn todo =
    div [ class "flex items-center" ]
        [ button [ onClick (OnDelete todo.id), class "code" ] [ text "X" ] ]



-- HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "vs3" ]
        [ div [ class "pa3 f4 tracked" ] [ text "ElmDOist" ]
        , HtmlStyledExtra.viewUnless (model.errors |> List.isEmpty) <|
            div [ class "ph3 flex hs3" ]
                [ div [ class "ttu tracked" ] [ text "Errors:" ]
                , HasErrors.view model.errors
                ]
        , div [ class "ph3 flex hs3" ]
            [ div [ class "ttu tracked" ] [ text "AuthState:" ]
            , AuthState.view model.authState
            ]
        , div [ class "ph3 flex items-center hs3" ]
            [ div [ class "ttu tracked" ] [ text "User:" ]
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
        , viewNav model
        ]



-- NAV


viewNav : Model -> Html Msg
viewNav model =
    div [ class "ph2 lh-title" ]
        [ div [ class "pa2 " ]
            [ div [ class "flex hs3" ]
                [ a
                    [ class "no-underline"
                    , href Route.inboxUrl
                    , class "b"
                    ]
                    [ text "Inbox" ]
                ]
            ]
        , div [ class "pa2" ]
            [ div [ class "flex hs3" ]
                [ a
                    [ class "no-underline"
                    , href Route.inboxUrl
                    , class "b"
                    ]
                    [ text "Today" ]
                ]
            ]
        , div [ class "pa2 flex hs3" ]
            [ div [ class "ttu tracked flex-grow-1" ] [ text "Projects:" ]
            , button [ onClick OnAddProjectStart ] [ text "add project" ]
            ]
        , viewNavProjects (Project.filterActive model.projectList)
        ]


viewNavProjects : ProjectList -> Html Msg
viewNavProjects projectList =
    div [ class "b " ] (List.map viewProjectNavItem projectList)


viewProjectNavItem : Project -> Html Msg
viewProjectNavItem project =
    div [ class "pa3 flex " ]
        [ a
            [ class "no-underline flex-grow-1"
            , href (Route.projectUrl project.id)
            ]
            [ text project.title ]
        , button [ class "", onClick (OnDeleteProject project.id) ] [ text "X" ]
        ]


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
