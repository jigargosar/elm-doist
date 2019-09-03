module TodoPopup exposing (SubPopup(..), ViewConfig, firstFocusable, view)

import Html.Styled as H exposing (Attribute, Html, div)
import Html.Styled.Attributes as A exposing (class, tabindex)
import Html.Styled.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import TodoId exposing (TodoId)
import UI.Key as Key
import UI.TextButton as TextButton


type SubPopup
    = ScheduleSubPopup
    | MoveSubPopup
    | NoSubPopup


firstFocusable : String
firstFocusable =
    "todo-popup--first-focusable"


type alias ViewConfig msg =
    { edit : TodoId -> msg
    , move : TodoId -> msg
    , delete : TodoId -> msg
    , schedule : TodoId -> msg
    , close : msg
    }


view :
    ViewConfig msg
    -> TodoId
    -> (SubPopup -> Html msg)
    -> Html msg
view config todoId viewSubPopup =
    H.node "track-focus-outside"
        [ class "absolute right-0 top-1"
        , class "bg-white shadow-1 w5"
        , class "z-1" -- if removed; causes flickering with hover icons
        , on "focusOutside" (JD.succeed config.close)
        , Key.onEscape config.close
        , tabindex -1
        ]
        (let
            containerDiv =
                div [ class "relative" ]
         in
         [ containerDiv
            [ TextButton.view (config.edit todoId)
                "Edit"
                [ class "pa2", A.id firstFocusable ]
            ]
         , containerDiv
            [ TextButton.view (config.move todoId)
                "Move to Project"
                [ class "pa2" ]
            , viewSubPopup MoveSubPopup
            ]
         , containerDiv
            [ TextButton.view (config.schedule todoId)
                "Schedule"
                [ class "pa2" ]
            , viewSubPopup ScheduleSubPopup
            ]
         , containerDiv
            [ TextButton.view (config.delete todoId)
                "Delete"
                [ class "pa2" ]
            ]
         ]
        )
