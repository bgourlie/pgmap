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


{-| Returns a flattened list of points orders from least to greatest
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
            in
            DifferenceList.append
                (DifferenceList.append (flattenHelp left) (DifferenceList.fromList [ pointType ]))
                (flattenHelp right)
