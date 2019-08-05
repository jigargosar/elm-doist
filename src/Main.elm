module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Dict.Extra
import HasErrors
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, disabled, tabindex)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import KeyEvent
import Ports
import Result.Extra
import Return
import Route exposing (Route)
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)


type alias Error =
    String


type alias Model =
    { todoDict : Dict TodoId Todo
    , authState : AuthState
    , errors : List Error
    , key : Nav.Key
    , route : Route
    }


type alias Return =
    Return.Return Msg Model


type alias Flags =
    { todoList : TodoList
    , cachedAuthState : AuthState
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.optional "todoList" Todo.listDecoder []
        |> JDP.optional "cachedAuthState" AuthState.decoder AuthState.initial


init : Value -> Url -> Nav.Key -> Return
init encodedFlags url key =
    let
        route =
            Route.fromUrl url

        model : Model
        model =
            { todoDict = Dict.empty
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
                |> Result.Extra.unpack updateDecodeError setAndCacheTodoDictFromList
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
    setTodoDictFromList flags.todoList model
        |> setAuthState flags.cachedAuthState
        |> pure


setTodoDictFromList : TodoList -> Model -> Model
setTodoDictFromList todoList model =
    { model | todoDict = Dict.Extra.fromListBy .id todoList }


setAndCacheTodoDictFromList : TodoList -> Model -> Return
setAndCacheTodoDictFromList todoList model =
    setTodoDictFromList todoList model
        |> pure
        |> command
            (Ports.localStorageSetJsonItem
                ( "todoList", Todo.listEncoder todoList )
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
                , viewTodoList model.todoDict
                ]
            ]
        ]
    }


type alias TodoDict =
    Dict TodoId Todo


viewTodoList : Dict TodoId Todo -> Html Msg
viewTodoList dict =
    let
        displayList =
            Dict.values dict

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
