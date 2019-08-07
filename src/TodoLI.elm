module TodoLI exposing (TodoDL, TodoLI, Transit(..))

import Todo exposing (Todo)


type Transit
    = Entering Float
    | Leaving Float
    | Staying


type alias TodoLI =
    { todo : Todo, height : Maybe Float, transit : Transit }


type alias TodoDL =
    List TodoLI
