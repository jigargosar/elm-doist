module Msg exposing (Msg(..))

import Browser
import Browser.Dom as Dom
import BrowserSize exposing (BrowserSize)
import Json.Encode exposing (Value)
import Millis exposing (Millis)
import Ports exposing (FirestoreQueryResponse)
import ProjectId exposing (ProjectId)
import SchedulePopup
import Time exposing (Zone)
import Todo exposing (DueAt, Todo, TodoList)
import TodoId exposing (TodoId)
import TodoPopup
import Url exposing (Url)


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnHere Time.Zone
    | OnBrowserResize BrowserSize
    | Focused (Result Dom.Error ())
    | OnAuthStateChanged Value
    | OnFirestoreQueryResponse FirestoreQueryResponse
    | OnSignInClicked
    | OnSignOutClicked
      -- ExistingTodoOperations
    | OnChecked TodoId Bool
    | OnDelete TodoId
    | PatchTodo TodoId (List Todo.Msg) Millis
    | OnMoveStart TodoId
    | OnTodoPopupTriggered TodoId
    | OnTodoPopupMsg TodoPopup.Msg
    | OnSchedulePopupTriggered SchedulePopup.Location TodoId
    | OnSchedulePopupMsg SchedulePopup.Msg
    | OnSchedulePopupClosed SchedulePopup.Location TodoId (Maybe DueAt)
    | OnMoveToProject TodoId ProjectId
    | OnDialogOverlayClickedOrEscapePressed
    | EditTodoRequested TodoId
    | TodoEditorTitleChanged TodoId String
    | TodoEditorCanceled
    | TodoEditorSaved
      -- NewTodoOperations
    | OnAddTodoStart ProjectId
    | AddTodo ProjectId Millis
    | OnAddTodoTodayStart
    | AddTodoToday Millis
      -- Project
    | OnDeleteProject ProjectId
    | OnAddProjectStart
    | AddProject Millis
