module ListZipper exposing (..)


type alias ListZipper a =
    ( List a, a, List a )


zipperFromListFocusedBy : (a -> Bool) -> List a -> Maybe ( List a, a, List a )
zipperFromListFocusedBy pred list =
    let
        ( l, c, r ) =
            zipperFromListByHelp pred list
    in
    Maybe.map (\jc -> ( l, jc, List.reverse r )) c


zipperToList : ( List a, a, List a ) -> List a
zipperToList ( l, c, r ) =
    List.reverse l ++ [ c ] ++ r


zipperFocus : ( List a, a, List a ) -> a
zipperFocus ( _, c, _ ) =
    c


zipperFromCons : a -> List a -> ( List a, a, List a )
zipperFromCons first list =
    ( [], first, list )


zipperFromListByHelp : (a -> Bool) -> List a -> ( List a, Maybe a, List a )
zipperFromListByHelp pred =
    List.foldl
        (\item ( l, c, r ) ->
            case c of
                Just _ ->
                    ( l, c, item :: r )

                Nothing ->
                    if pred item then
                        ( l, Just item, r )

                    else
                        ( item :: l, c, r )
        )
        ( [], Nothing, [] )
