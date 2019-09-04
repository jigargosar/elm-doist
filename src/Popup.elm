module Popup exposing (..)

import Browser.Dom as Dom
import BrowserSize
import TodoId exposing (TodoId)


type TodoContextSubPopup
    = TodoContextSchedulePopup
    | TodoContextMovePopup


type PopupKind
    = Schedule TodoId
    | TodoContext TodoId (Maybe TodoContextSubPopup)


type Model
    = Opening PopupKind
    | Opened PopupKind Dom.Element
    | Closed


type Msg
    = OpenPopup PopupKind
    | GotTriggerElement (Result Dom.Error Dom.Element)
    | BrowserResized BrowserSize.BrowserSize
    | Close
