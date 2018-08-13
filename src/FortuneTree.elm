module FortuneTree exposing (FortuneTree(..), flatten, insert, singleton)

import DifferenceList exposing (DifferenceList)
import Types exposing (Point)


type FortuneTree
    = Empty
    | Node Point FortuneTree FortuneTree


singleton : Point -> FortuneTree
singleton site =
    Node site Empty Empty


insert : Point -> FortuneTree -> FortuneTree
insert newPoint tree =
    case tree of
        Empty ->
            singleton newPoint

        Node point left Empty ->
            Node point left (singleton newPoint)

        Node point left right ->
            Node point (insert newPoint left) right


flatten : FortuneTree -> List Point
flatten tree =
    flattenHelp tree
        |> DifferenceList.toList


flattenHelp : FortuneTree -> DifferenceList Point
flattenHelp tree =
    case tree of
        Empty ->
            DifferenceList.fromList []

        Node point left right ->
            DifferenceList.append
                (DifferenceList.append (DifferenceList.fromList [ point ]) (flattenHelp right))
                (flattenHelp left)
