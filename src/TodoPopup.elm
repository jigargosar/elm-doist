module TodoPopup exposing
    ( By(..)
    , Model(..)
    , firstFocusableDomId
    )

import TodoId exposing (TodoId)


type Model
    = Open TodoId
    | Closed


type By
    = EscapeKey
    | FocusOutside


firstFocusableDomId : String
firstFocusableDomId =
    "todo-popup--first-focusable--dom-id"
