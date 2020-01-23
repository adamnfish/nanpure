module Utils exposing (flattenList, resTraverse)


resTraverse : (a -> Result err b) -> List a -> Result err (List b)
resTraverse f aa =
    List.foldr
        (\a rbs -> Result.map2 (::) (f a) rbs)
        (Ok [])
        aa


flattenList : List (List a) -> List a
flattenList lls =
    List.foldr (++) [] lls
