module Main exposing (main)

import AuthState exposing (AuthState)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Errors exposing (Errors)
import Html exposing (Html, div, text)
import Json.Encode exposing (Value)
import Return
import Route exposing (Route)
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import UpdateExtra exposing (pure)
import Url exposing (Url)


type alias Model =
    { todoDict : Dict TodoId Todo
    , authState : AuthState
    , errors : Errors
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
            , errors = Errors.initial
            , key = key
            , route = route
            }
    in
    model |> pure


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url



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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []


view : Model -> Browser.Document Msg
view _ =
    { title = "ElmDoist"
    , body =
        [ div []
            [ div [] [ text "HW" ]
            , div [] [ text "AuthState" ]
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
