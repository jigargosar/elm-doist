module TodoItem exposing (Config, viewKeyed, viewList)

import BasicsExtra exposing (..)
import FontAwesome.Attributes as FAA
import FontAwesome.Regular as FAR
import FontAwesome.Solid as FAS
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (class)
import Html.Styled.Events exposing (onClick)
import String.Extra as SX
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import TodoPopup
import UI.IconButton as IconButton
import UI.TextButton as TextButton


type alias Config msg =
    { noOp : msg
    , doneChanged : TodoId -> Bool -> msg
    , editClicked : Todo -> msg
    , moreClicked : Todo -> msg
    }


viewList : Config msg -> Zone -> List Todo -> List ( String, Html msg )
viewList config zone =
    List.map (viewKeyed config zone)


viewKeyed : Config msg -> Zone -> Todo -> ( String, Html msg )
viewKeyed config zone todo =
    ( TodoId.toString todo.id, view config zone todo )


view : Config msg -> Zone -> Todo -> Html msg
view config zone todo =
    div
        [ class "flex hide-child"
        ]
        [ viewTodoItemDoneCheckbox (config.doneChanged todo.id) todo.isDone
        , viewTodoItemTitle (config.editClicked todo) todo.title
        , viewTodoItemDueDate config.noOp zone todo.dueAt
        , IconButton.view (config.moreClicked todo)
            [ A.id <| TodoPopup.triggerElDomId todo.id
            , class "pa2 tc child"
            ]
            FAS.ellipsisH
            []
        ]


viewTodoItemDueDate : msg -> Zone -> DueAt -> Html msg
viewTodoItemDueDate clickMsg here dueAt =
    div [ class "flex-shrink-0 relative flex" ]
        [ case Todo.formatDueAt "MMM dd" here dueAt of
            Nothing ->
                IconButton.view clickMsg
                    [ class "pa2 child" ]
                    FAR.calendarPlus
                    []

            Just formattedDueAt ->
                TextButton.view_ clickMsg
                    formattedDueAt
                    [ class "pa2 flex-shrink-0 f7 lh-copy" ]
        ]


viewTodoItemDoneCheckbox : (Bool -> msg) -> Bool -> Html msg
viewTodoItemDoneCheckbox checkedMsg isChecked =
    let
        faCheckBtn action icon =
            IconButton.view action
                [ class "pa2 " ]
                icon
                [ FAA.lg ]
    in
    ifElse isChecked
        (faCheckBtn (checkedMsg False) FAR.checkCircle)
        (faCheckBtn (checkedMsg True) FAR.circle)


viewTodoItemTitle : msg -> String -> Html msg
viewTodoItemTitle clickMsg title_ =
    let
        ( title, titleClass ) =
            ifElse (SX.isBlank title_)
                ( "<no title>", "i black-70" )
                ( title_, "" )
    in
    div
        [ class titleClass
        , class "pa2 flex-grow-1 hover-bg-light-yellow"
        , class " lh-title"
        , onClick clickMsg
        ]
        [ div [ class "" ] [ text title ] ]
