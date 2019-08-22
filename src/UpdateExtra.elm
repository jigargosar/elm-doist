module UpdateExtra exposing (andThen, command, effect, pure)

-- UPDATE HELPERS

import Maybe.Extra as MX
import Return exposing (Return)


pure =
    Return.singleton


effect =
    Return.effect_


andThen =
    Return.andThen


command =
    Return.command
