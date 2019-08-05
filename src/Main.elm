module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import HasErrors
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Ports
import Result.Extra
import Return
import Route exposing (Route)
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)
import UpdateExtra exposing (pure)
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


init : Value -> Url -> Nav.Key -> Return
init _ url key =
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
    model |> pure


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnAuthStateChanged Value
    | OnTodoListChanged Value
    | OnSignInClicked
    | OnSignOutClicked



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
                |> Result.Extra.unpack updateDecodeError updateAuthState
                |> callWith model

        OnTodoListChanged encodedValue ->
            JD.decodeValue Todo.listDecoder encodedValue
                |> Result.Extra.unpack updateDecodeError updateTodoList
                |> callWith model

        OnSignInClicked ->
            ( model, Ports.signIn () )

        OnSignOutClicked ->
            ( model, Ports.signOut () )


updateTodoList : TodoList -> Model -> Return
updateTodoList todoList model =
    let
        _ =
            Debug.log "todoList" todoList
    in
    pure model


updateAuthState : AuthState -> Model -> Return
updateAuthState authState model =
    setAuthState authState model
        |> pure


updateDecodeError : JD.Error -> Model -> Return
updateDecodeError error model =
    HasErrors.prependDecodeError error model
        |> pure


setAuthState : AuthState -> Model -> Model
setAuthState authState model =
    { model | authState = authState }


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
            , case model.authState of
                AuthState.Unknown ->
                    button [ disabled True ] [ text "SignIn" ]

                AuthState.SignedIn user ->
                    div [ class "flex" ]
                        [ div [] [ text user.displayName ]
                        , button [ onClick OnSignOutClicked ] [ text "SignOut" ]
                        ]

                AuthState.NotSignedIn ->
                    button [ onClick OnSignInClicked ] [ text "SignIn" ]
            ]
        ]
    }


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
