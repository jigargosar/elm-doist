module Dialog exposing (..)

import Todo exposing (Todo)


type Dialog
    = NoDialog
    | MoveToProjectDialog Todo
    | DueDialog Todo
