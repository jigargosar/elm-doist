module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import HasErrors
import Html.Styled exposing (Html, a, button, div, input, text)
import Html.Styled.Attributes exposing (checked, class, disabled, href, tabindex, type_)
import Html.Styled.Events exposing (onCheck, onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Now exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (ProjectList)
import ProjectId exposing (ProjectId)
import Result.Extra
import Return
import Route exposing (Route)
import StyledKeyEvent
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (andThen, command, pure)
import Url exposing (Url)


type alias Error =
    String


type alias InlineEditTodo =
    { todo : Todo, pid : Maybe ProjectId }


type alias Model =
    { todoList : TodoList
    , projectList : ProjectList
    , inlineEditTodo : Maybe InlineEditTodo
    , authState : AuthState
    , errors : List Error
    , key : Nav.Key
    , route : Route
    }


type alias Return =
    Return.Return Msg Model


type alias Flags =
    { cachedTodoList : TodoList
    , cachedProjectList : ProjectList
    , cachedAuthState : AuthState
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "cachedTodoList" (JD.oneOf [ Todo.listDecoder, JD.null [] ])
        |> JDP.required "cachedProjectList" (JD.oneOf [ Project.listDecoder, JD.null [] ])
        |> JDP.required "cachedAuthState"
            (JD.oneOf [ AuthState.decoder, JD.null AuthState.initial ])


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
    | PatchTodo TodoId Todo.Msg Millis
    | OnAddTodo
    | AddTodo Millis
    | OnAddProject
    | AddProject Millis



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
            ( { model | route = route }, Cmd.none )

        OnAuthStateChanged encodedValue ->
            JD.decodeValue AuthState.decoder encodedValue
                |> Result.Extra.unpack onDecodeError onAuthStateChanged
                |> callWith model

        OnTodoListChanged encodedValue ->
            JD.decodeValue Todo.listDecoder encodedValue
                |> Result.Extra.unpack onDecodeError setAndCacheTodoList
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
                        |> Result.Extra.unpack onDecodeError setAndCacheTodoList
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

        PatchTodo todoId todoMsg now ->
            ( model
            , Ports.updateFirestoreDoc
                { userDocPath = "todos/" ++ todoId
                , data = JE.object (Todo.modifyPatch todoMsg now)
                }
            )

        OnAddTodo ->
            ( model, Now.perform AddTodo )

        AddTodo now ->
            ( model
            , Ports.addFirestoreDoc
                { userCollectionName = "todos"
                , data = Todo.new now
                }
            )

        OnAddProject ->
            ( model, Now.perform AddProject )

        AddProject now ->
            ( model
            , Ports.addFirestoreDoc
                { userCollectionName = "projects"
                , data = Project.new now
                }
            )


patchTodoCmd : TodoId -> Todo.Msg -> Cmd Msg
patchTodoCmd todoId todoMsg =
    PatchTodo todoId todoMsg |> Now.perform


queryTodoListCmd =
    Ports.queryFirestore
        { id = "todoList"
        , userCollectionName = "todos"
        }


queryProjectListCmd =
    Ports.queryFirestore
        { id = "projectList"
        , userCollectionName = "projects"
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
        |> pure


setTodoList : TodoList -> Model -> Model
setTodoList todoList model =
    { model | todoList = todoList }


setAndCacheTodoList : TodoList -> Model -> Return
setAndCacheTodoList todoList model =
    setTodoList todoList model
        |> pure
        |> command
            (Ports.localStorageSetJsonItem
                ( "cachedTodoList", Todo.listEncoder todoList )
            )


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


viewRoute : Route -> Model -> StyledDocument Msg
viewRoute route model =
    case route of
        Route.Inbox ->
            let
                displayTodoList =
                    Todo.filterSort
                        (Todo.AndFilter Todo.Pending (Todo.BelongsToProject ""))
                        [ Todo.ByIdx, Todo.ByRecentlyCreated ]
                        model.todoList

                title =
                    "Inbox"
            in
            { title = title
            , body =
                [ viewHeader model
                , todoListPageContent title displayTodoList
                ]
            }

        Route.Project pid ->
            let
                displayTodoList =
                    Todo.filterSort
                        (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))
                        [ Todo.ByIdx, Todo.ByRecentlyCreated ]
                        model.todoList

                title =
                    model.projectList
                        |> List.Extra.find (.id >> (==) pid)
                        |> Maybe.map .title
                        |> Maybe.withDefault "Project Not Found"
            in
            { title = title
            , body =
                [ viewHeader model
                , todoListPageContent title displayTodoList
                ]
            }

        Route.NotFound url ->
            viewRoute Route.Inbox model



-- TodoListPageContent


todoListPageContent title displayTodoList =
    div [ class "pa3 vs3" ]
        [ div [ class "flex items-center hs3" ]
            [ div [ class "b flex-grow-1" ] [ text title ]
            , button [ onClick OnAddTodo ] [ text "ADD" ]
            ]
        , div [ class "vs1" ] (List.map viewTodoItem displayTodoList)
        ]


viewTodoItem todo =
    div
        [ class "flex hs1 lh-copy db "
        , tabindex 0
        ]
        [ viewTodoCheck todo
        , viewTodoTitle todo
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
        , onClick (OnChangeTitleRequested todo.id)
        , OnChangeTitleRequested todo.id |> StyledKeyEvent.onEnter
        ]
        [ text title ]


viewDeleteTodoBtn todo =
    div [ class "flex items-center" ]
        [ button [ onClick (OnDelete todo.id) ] [ text "X" ] ]



-- HEADER


viewHeader : Model -> Html Msg
viewHeader model =
    div []
        [ div [ onClick NoOp ] [ text "ElmDOist" ]
        , div [ class "pa3 flex hs3" ]
            [ div [ class "b" ] [ text "AuthState:" ]
            , AuthState.view model.authState
            ]
        , div [ class "pa3 flex hs3" ]
            [ div [ class "b" ] [ text "Errors:" ]
            , HasErrors.view model.errors
            ]
        , div [ class "pa3 flex items-center hs3" ]
            [ div [ class "b" ] [ text "User:" ]
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
        , div [ class "pa3 hs3" ]
            [ div [ class "flex hs3" ]
                [ a [ class "no-underline", href Route.inboxUrl, class "b" ] [ text "Inbox" ]
                ]
            ]
        , div [ class "pa3 " ]
            [ div [ class "flex hs3" ]
                [ div [ class "b flex-grow-1" ] [ text "Projects:" ]
                , button [ onClick OnAddProject ] [ text "New Project" ]
                ]
            , viewNavProjects model.projectList
            ]
        ]


viewNavProjects : ProjectList -> Html msg
viewNavProjects projectList =
    div [ class "vs1" ] (List.map viewProjectNavItem projectList)


viewProjectNavItem project =
    div [ class "pa2" ]
        [ a
            [ class "no-underline"
            , href (Route.projectUrl project.id)
            ]
            [ text project.title ]
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
