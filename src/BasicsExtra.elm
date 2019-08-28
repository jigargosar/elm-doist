module BasicsExtra exposing (callWith, eq_, extractAndApply, ifElse, uncurry, unpackErr)

-- CORE HELPERS


unpackErr : (e -> v) -> Result e v -> v
unpackErr fn result =
    case result of
        Err e ->
            fn e

        Ok v ->
            v


callWith : a -> (a -> b) -> b
callWith =
    (|>)


eq_ : a -> a -> Bool
eq_ =
    (==)


ifElse : Bool -> c -> c -> c
ifElse bool a b =
    if bool then
        a

    else
        b


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a1, a2 ) =
    fn a1 a2


extractAndApply : (arg2 -> arg1) -> (arg1 -> arg2 -> result) -> arg2 -> result
extractAndApply extractFn applyFn val =
    applyFn (extractFn val) val
