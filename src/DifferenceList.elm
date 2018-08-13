module DifferenceList exposing (DifferenceList, append, fromList, toList)


type alias DifferenceList a =
    List a -> List a


fromList : List a -> DifferenceList a
fromList xs =
    \l -> xs ++ l


append : DifferenceList a -> DifferenceList a -> DifferenceList a
append xs ys =
    xs << ys


toList : DifferenceList a -> List a
toList xs =
    xs []
