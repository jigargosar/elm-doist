module TodoLI exposing (TodoDL, TodoLI, Transit(..), fromTodo, initDisplayList)

import Set exposing (Set)
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)


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


initDisplayList : TodoList -> TodoDL
initDisplayList =
    List.map fromTodo


toIdSet : TodoDL -> Set TodoId
toIdSet todoDL =
    todoDL |> List.map (.todo >> .id) |> Set.fromList


update todoList todoDL =
    let
        newTodoDL : TodoDL
        newTodoDL =
            initDisplayList todoList

        newIdSet : Set TodoId
        newIdSet =
            toIdSet newTodoDL

        oldIdSet : Set TodoId
        oldIdSet =
            toIdSet todoDL

        removedIdSet : Set TodoId
        removedIdSet =
            Set.diff oldIdSet newIdSet

        removedTLI : List TodoLI
        removedTLI =
            todoDL
                |> List.filter
                    (\tli -> Set.member tli.todo.id removedIdSet)
                |> List.map
                    (\tli ->
                        { tli
                            | transit =
                                case tli.transit of
                                    Leaving _ ->
                                        tli.transit

                                    Entering _ ->
                                        Leaving 0

                                    Staying ->
                                        Leaving 0
                        }
                    )

        newTodoDLWithRemovedAndSorted : List TodoLI
        newTodoDLWithRemovedAndSorted =
            newTodoDL ++ removedTLI
    in
    1
