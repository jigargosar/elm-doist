module UpdateExtra exposing (andThen, andThenMaybe, command, commandIf, effect, pure, toCmd)

-- UPDATE HELPERS

import BasicsExtra exposing (ifElse)
import Maybe.Extra as MX
import Return exposing (Return)
import Task


pure =
    Return.singleton


effect : (a -> Cmd msg) -> ( a, Cmd msg ) -> ( a, Cmd msg )
effect f ( model, cmd ) =
    ( model, Cmd.batch [ cmd, f model ] )


andThen =
    Return.andThen


command =
    Return.command


commandIf : Bool -> Cmd msg -> Return msg model -> Return msg model
commandIf bool cmd_ =
    command (ifElse bool cmd_ Cmd.none)


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
