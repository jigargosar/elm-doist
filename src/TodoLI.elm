module TodoLI exposing (TodoDL, TodoLI, Transit(..), fromTodo, initDisplayList)

import Todo exposing (Todo)


type Transit
    = Entering Float
    | Leaving Float
    | Staying


type alias TodoLI =
    { todo : Todo, height : Maybe Float, transit : Transit }


type alias TodoDL =
    List TodoLI


fromTodo : Todo -> TodoLI
fromTodo t =
    { todo = t
    , height = Nothing
    , transit = Staying
    }


initDisplayList : List Todo -> List TodoLI
initDisplayList =
    List.map fromTodo
