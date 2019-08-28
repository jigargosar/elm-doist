module Tuple2 exposing (..)


type alias Tuple2 a b =
    ( a, b )


repeat : a -> ( a, a )
repeat a =
    ( a, a )
