module Main exposing (main)

import AuthState exposing (AuthState)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Ports
import Return
import Route exposing (Route)
import Todo exposing (Todo)
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
            case JD.decodeValue AuthState.decoder encodedValue of
                Ok authState ->
                    setAuthState authState model
                        |> pure

                Err decodeError ->
                    prependDecodeError decodeError model
                        |> pure


setAuthState : AuthState -> Model -> Model
setAuthState authState model =
    { model | authState = authState }


prependErrorString : Error -> Model -> Model
prependErrorString error model =
    { model | errors = error :: model.errors }


prependDecodeError : JD.Error -> Model -> Model
prependDecodeError error =
    prependErrorString (JD.errorToString error)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onAuthStateChanged OnAuthStateChanged ]


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
