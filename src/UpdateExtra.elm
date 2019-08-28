module UpdateExtra exposing (andThen, command, effect, pure, toCmd)

-- UPDATE HELPERS

import Maybe.Extra as MX
import Return exposing (Return)
import Task


pure =
    Return.singleton


effect =
    Return.effect_


andThen =
    Return.andThen


command =
    Return.command


toCmd : msg -> Cmd msg
toCmd =
    Task.succeed >> Task.perform identity


andThenMaybe :
    (a -> Maybe (Return.Return msg a))
    -> Return.ReturnF msg a
andThenMaybe fn ret =
    ret
        |> Tuple.mapFirst (fn >> Maybe.withDefault ret)
        |> Return.flatten
