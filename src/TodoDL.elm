module TodoDL exposing (TodoDL, TodoLI, Transit(..), empty, filterMap, fromTodo, initDisplayList, map, toList, update)

import Set exposing (Set)
import Todo exposing (Todo, TodoList)
import TodoId exposing (TodoId)


type Transit
    = Entering Float
    | Leaving Float
    | Staying


type alias TodoLI =
    { todo : Todo, height : Maybe Float, transit : Transit }


type TodoDL
    = TodoDL (List TodoLI)


fromTodo : Todo -> TodoLI
fromTodo t =
    { todo = t
    , height = Nothing
    , transit = Staying
    }


initDisplayList : TodoList -> TodoDL
initDisplayList =
    List.map fromTodo >> TodoDL


toIdSet : TodoDL -> Set TodoId
toIdSet (TodoDL todoDL) =
    todoDL |> List.map (.todo >> .id) |> Set.fromList


update : TodoList -> TodoDL -> TodoDL
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
    in
    concat newTodoDL (removedTLI removedIdSet todoDL)


concat : TodoDL -> TodoDL -> TodoDL
concat (TodoDL a) (TodoDL b) =
    a ++ b |> TodoDL


removedTLI : Set TodoId -> TodoDL -> TodoDL
removedTLI removedIdSet (TodoDL todoDL) =
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
        |> TodoDL


toList : TodoDL -> List TodoLI
toList (TodoDL todoDL) =
    todoDL


empty : TodoDL
empty =
    TodoDL []


filterMap : (TodoLI -> Maybe TodoLI) -> TodoDL -> TodoDL
filterMap fn (TodoDL todoDL) =
    List.filterMap fn todoDL
        |> TodoDL


map : (TodoLI -> TodoLI) -> TodoDL -> TodoDL
map fn (TodoDL todoDL) =
    List.map fn todoDL
        |> TodoDL
