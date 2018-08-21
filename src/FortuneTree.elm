module FortuneTree exposing (FortuneTree(..), PointType(..), empty, flatten, insert, singleton)

import DifferenceList exposing (DifferenceList)
import Types exposing (Point)


type FortuneTree
    = Empty
    | Node Point FortuneTree FortuneTree


type PointType
    = Edge Point
    | Curve Point


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

        Node point left right ->
            if newPoint > point then
                Node point left (insert newPoint right)
            else if newPoint < point then
                Node point (insert newPoint left) right
            else
                tree


{-| Returns a flattened list ordered with leaves first and moving right to left.
-}
flatten : FortuneTree -> List PointType
flatten tree =
    flattenHelp tree
        |> DifferenceList.toList


flattenHelp : FortuneTree -> DifferenceList PointType
flattenHelp tree =
    case tree of
        Empty ->
            DifferenceList.fromList []

        Node point left right ->
            let
                pointType =
                    case ( left, right ) of
                        ( Empty, Empty ) ->
                            Curve point

                        _ ->
                            Edge point

                leftDList =
                    flattenHelp left

                rightDList =
                    flattenHelp right
            in
            DifferenceList.append
                (DifferenceList.append (DifferenceList.fromList [ pointType ]) leftDList)
                rightDList
