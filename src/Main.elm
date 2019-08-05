port module Main exposing (effect, main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Json.Encode exposing (Value)
import Return
import Route exposing (Route)
import TodoCollection as TC exposing (TodoCollection)
import Url exposing (Url)



-- PORTS
-- FLAGS


type alias Error =
    String


type alias Model =
    { todos : TodoCollection
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
            { todos = TC.initial
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


prependError : String -> Model -> Model
prependError error model =
    { model | errors = error :: model.errors }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []


view : Model -> Browser.Document Msg
view model =
    { title = "ElmDoist"
    , body = [ div [] [ text "HW" ] ]
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



-- UPDATE HELPERS


pure =
    Return.singleton


effect =
    Return.effect_


andThen =
    Return.andThen


command =
    Return.command



-- VIEW HELPERS


viewIf bool v =
    if bool then
        v

    else
        text ""


viewUnless bool v =
    viewIf (not bool) v



-- CORE HELPERS


unpackErr : (e -> v) -> Result e v -> v
unpackErr fn result =
    case result of
        Err e ->
            fn e

        Ok v ->
            v
