module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import HasErrors
import Html.Styled exposing (Html, a, button, div, input, text)
import Html.Styled.Attributes exposing (checked, class, disabled, href, tabindex, type_, value)
import Html.Styled.Events exposing (onCheck, onClick)
import HtmlStyledExtra
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Maybe.Extra
import Now exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import Project exposing (ProjectList)
import Result.Extra
import Return
import Route exposing (Route)
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (andThen, command, pure)
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


cacheDecoder =
    JD.succeed Cache
        |> JDP.optional "dialog" dialogDecoder NoDialog


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
    | PatchTodo TodoId Todo.Msg Millis
    | OnAddTodo
    | AddTodo Millis
    | OnAddProject
    | OnEdit TodoId
    | AddProject Millis
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

        OnEdit todoId ->
            model.todoList
                |> List.Extra.find (.id >> (==) todoId)
                |> Maybe.Extra.unwrap pure startEditing
                |> callWith model

        OnEditCancel ->
            pure { model | inlineEditTodo = Nothing }

        OnEditSave ->
            pure { model | inlineEditTodo = Nothing }


startEditing : Todo -> Model -> Return
startEditing todo model =
    pure { model | inlineEditTodo = Just { todo = todo, title = Nothing } }


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


viewFooter model =
    div []
        [ case model.dialog of
            NoDialog ->
                HtmlStyledExtra.empty

            MoveToProjectDialog todo ->
                div [ class "absolute absolute--fill bg-black-50" ] []
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
            masterLayout title
                (todoListPageContent title model.inlineEditTodo displayTodoList)
                model

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
            masterLayout title
                (todoListPageContent title model.inlineEditTodo displayTodoList)
                model

        Route.NotFound _ ->
            viewRoute Route.Inbox model



-- TodoListPageContent


todoListPageContent : String -> Maybe InlineEditTodo -> TodoList -> Html Msg
todoListPageContent title edit displayTodoList =
    div [ class "pa3 vs3" ]
        [ div [ class "flex items-center hs3" ]
            [ div [ class "b flex-grow-1" ] [ text title ]
            , button [ onClick OnAddTodo ] [ text "ADD" ]
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
