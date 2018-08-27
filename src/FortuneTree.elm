module FortuneTree exposing (FortunePoint(..), FortuneTree(..), empty, flatten, insert, singleton)

import DifferenceList exposing (DifferenceList)
import Types exposing (Point)


type FortuneTree
    = Empty
    | Node Point FortuneTree FortuneTree


type FortunePoint
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



--insert2 : Point -> FortuneTree -> FortuneTree -> FortuneTree
--insert2 newSite previousNode tree =
--    case tree of
--        Empty ->
--            case previousNode of
--                Empty ->
--                    singleton newSite
--
--                Node parentPoint _ _ ->
--                    let
--                        subTree =
--                            Node parentPoint (parentPoint Empty Empty) (Node newSite (Node newSite Empty Empty) (Node newSite Empty Empty))
--                    in
--
--
--
--
--        Node point left right ->
--            if newSite > point then
--                Node point left (insert2 newSite right)
--            else
--                Node point (insert2 newSite left) right


{-| Returns a flattened list of points orders from least to greatest
-}
flatten : FortuneTree -> List FortunePoint
flatten tree =
    flattenHelp tree
        |> DifferenceList.toList


flattenHelp : FortuneTree -> DifferenceList FortunePoint
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
