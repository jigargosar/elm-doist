module InlineEditTodo exposing (..)

import Todo exposing (DueAt, Todo)


type alias Model =
    { todo : Todo, title : Maybe String, dueAt : Maybe DueAt }
