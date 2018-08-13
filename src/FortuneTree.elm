module FortuneTree exposing (FortuneTree(..), empty, flatten, insert, singleton)

import DifferenceList exposing (DifferenceList)
import Types exposing (Point)


type FortuneTree
    = Empty
    | Node Point FortuneTree FortuneTree


empty : FortuneTree
empty =
    Empty


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


{-| Returns a leaf count and a flattened list ordered with leaves first and moving right to left.
-}
flatten : FortuneTree -> ( Int, List Point )
flatten tree =
    flattenHelp 0 tree
        |> (\( leafCount, dList ) -> ( floor (toFloat leafCount / 2.0), List.reverse (DifferenceList.toList dList) ))


flattenHelp : Int -> FortuneTree -> ( Int, DifferenceList Point )
flattenHelp leafCount tree =
    case tree of
        Empty ->
            ( leafCount, DifferenceList.fromList [] )

        Node point left right ->
            let
                newLeafCount =
                    if right == Empty then
                        leafCount + 1
                    else
                        leafCount

                ( rightLeafCount, rightDList ) =
                    flattenHelp newLeafCount right

                ( leftLeafCount, leftDList ) =
                    flattenHelp newLeafCount left
            in
            ( rightLeafCount + leftLeafCount
            , DifferenceList.append
                (DifferenceList.append (DifferenceList.fromList [ point ]) rightDList)
                leftDList
            )
