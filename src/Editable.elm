module Editable exposing (..)


type Editable a
    = Editable a a


getCurrent : Editable a -> a
getCurrent (Editable _ a) =
    a


initEditable : a -> Editable a
initEditable new =
    Editable new new


changeEditable : a -> Editable a -> Editable a
changeEditable new (Editable old _) =
    Editable old new


isDirty (Editable old new) =
    old /= new
