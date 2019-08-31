module UpdateExtra exposing (pure)

-- UPDATE HELPERS

import BasicsExtra exposing (ifElse)
import Maybe.Extra as MX
import Return exposing (Return)
import Task


pure =
    Return.singleton
