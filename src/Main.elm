module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import HasErrors
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, tabindex)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode exposing (Value)
import KeyEvent
import Ports
import Result.Extra
import Return
import Route exposing (Route)
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (andThen, command, pure)
import Url exposing (Url)


type alias Error =
    String


type alias Model =
    { todoList : TodoList
    , authState : AuthState
    , errors : List Error
    , key : Nav.Key
    , route : Route
    }


type alias Return =
    Return.Return Msg Model


type alias Flags =
    { cachedTodoList : TodoList
    , cachedAuthState : AuthState
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "cachedTodoList" (JD.oneOf [ Todo.listDecoder, JD.null [] ])
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
            , authState = AuthState.initial
            , errors = []
            , key = key
            , route = route
            }
    in
    model
        |> pure
        |> andThen (updateFromEncodedFlags encodedFlags)


queryTodoListCmd =
    Ports.queryFirestore { id = "todoList", userCollectionName = "todos", limit = 5 }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnAuthStateChanged Value
    | OnTodoListChanged Value
    | OnSignInClicked
    | OnSignOutClicked
    | OnChangeTitleRequested TodoId



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
                |> Result.Extra.unpack updateDecodeError setAndCacheAuthState
                |> callWith model

        OnTodoListChanged encodedValue ->
            JD.decodeValue Todo.listDecoder encodedValue
                |> Result.Extra.unpack updateDecodeError setAndCacheTodoList
                |> callWith model

        OnSignInClicked ->
            ( model, Ports.signIn () )

        OnSignOutClicked ->
            ( model, Ports.signOut () )

        OnChangeTitleRequested todoId ->
            ( model, Ports.changeTodoTitle todoId )


updateFromEncodedFlags : Value -> Model -> Return
updateFromEncodedFlags encodedFlags model =
    JD.decodeValue flagsDecoder encodedFlags
        |> Result.Extra.unpack updateDecodeError updateFromFlags
        |> callWith model


updateFromFlags : Flags -> Model -> Return
updateFromFlags flags model =
    setTodoList flags.cachedTodoList model
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


setAuthState : AuthState -> Model -> Model
setAuthState authState model =
    { model | authState = authState }


setAndCacheAuthState : AuthState -> Model -> Return
setAndCacheAuthState authState model =
    setAuthState authState model
        |> pure
        |> command
            (Ports.localStorageSetJsonItem
                ( "cachedAuthState", AuthState.encoder authState )
            )


updateDecodeError : JD.Error -> Model -> Return
updateDecodeError error model =
    HasErrors.prependDecodeError error model
        |> pure


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged OnAuthStateChanged
        , Ports.onTodoListChanged OnTodoListChanged
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "ElmDoist"
    , body =
        [ div []
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
            , div [ class "pa3 vs3" ]
                [ div [ class "b" ] [ text "TodoList:" ]
                , viewTodoList model.todoList
                ]
            ]
        ]
    }


viewTodoList : List Todo -> Html Msg
viewTodoList displayList =
    let
        displayTitle todo =
            if String.trim todo.title |> String.isEmpty then
                ( "<no title>", "i black-70" )

            else
                ( todo.title, "" )

        viewTodoItem todo =
            let
                ( dt, cls ) =
                    displayTitle todo
            in
            div
                [ class "pointer lh-solid pa2 db hover-bg-light-yellow"
                , tabindex 0
                , onClick (OnChangeTitleRequested todo.id)
                , OnChangeTitleRequested todo.id |> KeyEvent.onEnter
                ]
                [ div [ class cls ] [ text dt ]
                ]
    in
    div [ class "vs1" ] (List.map viewTodoItem displayList)


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
