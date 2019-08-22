module UpdateExtra exposing (andThen, command, effect, maybeUnpackPure, pure)

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


maybeUnpackPure :
    (a -> model -> Return msg model)
    -> model
    -> Maybe a
    -> Return msg model
maybeUnpackPure rfn model =
    MX.unpack (\_ -> pure model) (\val -> rfn val model)
