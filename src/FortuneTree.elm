module FortuneTree exposing (FortunePoint(..), FortuneTree(..), empty, flatten, insert, insertSubtree, singleton)

import Parabola exposing (sampleParabola)
import DifferenceList exposing (DifferenceList)
import Types exposing (Line, Point)


type FortuneNode
    = UninterruptedParabola Point
    | LeftIntersectingParabola Point Float
    | RightIntersectingParabola Point Float
    | LeftRightIntersectingParabola Point Float Float
    | Border Line


type ParabolaIntersection
    = LeftRightIntersection Float Float
    | SingleIntersection Float
    | SameParabola
    | NoIntersection


type FortuneTree
    = Empty
    | Node FortuneNode FortuneTree FortuneTree


type FortunePoint
    = Edge Point
    | Curve Point


comparisonValue : FortuneNode -> Int
comparisonValue node =
    case node of
        UninterruptedParabola ( x, _ ) ->
            x

        LeftIntersectingParabola _ x ->
            x

        RightIntersectingParabola _ x ->
            x

        LeftRightIntersectingParabola ( x, _ ) _ _ ->
            x

        Border ( ( x, _ ), _ ) ->
            x


empty : FortuneTree
empty =
    Empty


singleton : Point -> FortuneTree
singleton site =
    Node (UninterruptedParabola site) Empty Empty


insert : Float -> Point -> FortuneTree -> FortuneTree
insert directrix newPoint tree =
    let
        maybeParentPoint =
            findParent newPoint tree Nothing
    in
    case maybeParentPoint of
        Nothing ->
            singleton newPoint

        Just parentPoint ->
            let
                ( leftIntersection, maybeRightIntersection ) =
                    case getIntersection directrix parentPoint newPoint of
                        LeftRightIntersection li ri ->
                            ( li, Just ri )

                        SingleIntersection i ->
                            ( i, Nothing )

                        _ ->
                            Debug.todo "Handle this"

                leftIntersectingPoint =
                    ( leftIntersection, sampleParabola newPoint directrix leftIntersection )

                leftSubTree =
                    Node (RightIntersectingParabola parentPoint leftIntersection) Empty Empty

                rightSubTree =
                    case maybeRightIntersection of
                        Just rightIntersection ->
                            let
                                rightIntersectingPoint =
                                    ( rightIntersection, sampleParabola newPoint directrix rightIntersection )
                            in
                            Node (Border ( leftIntersection, rightIntersection ))
                                (Node (LeftRightIntersectingParabola newPoint leftIntersection rightIntersection) Empty Empty)
                                (Node (LeftIntersectingParabola parentPoint rightIntersection) Empty Empty)

                        Nothing ->
                            Node (LeftIntersectingParabola newPoint leftIntersection) Empty Empty
            in
            insertSubtree leftSubTree tree
                |> insertSubtree rightSubTree


insertSubtree : FortuneTree -> FortuneTree -> FortuneTree
insertSubtree subTree tree =
    case subTree of
        Empty ->
            tree

        Node newNode _ _ ->
            case tree of
                Empty ->
                    subTree

                Node curNode left right ->
                    let
                        newNodeCompareValue =
                            comparisonValue newNode

                        curNodeCompareValue =
                            comparisonValue curNode
                    in
                    if newNodeCompareValue > curNodeCompareValue then
                        Node curNode left (insertSubtree subTree right)

                    else
                        Node curNode (insertSubtree subTree left) right


findParent : Point -> FortuneTree -> Maybe Point -> Maybe Point
findParent newPoint tree parentPoint =
    case tree of
        Empty ->
            parentPoint

        Node point left right ->
            if newPoint > point then
                findParent newPoint right (Just point)

            else
                findParent newPoint left (Just point)


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


{-| The intersection formula was derived from wolfram alpha using the following input:

`solve {(y2 - d)((x - x1)^2 + y1^2 - d^2) == (y1 - d)((x - x2)^2 + y2^2 - d^2)}`

This formula is simplified from a more general parabola intersection formula because it can assume that the directrix
for each parabola is the same.

-}
getIntersection : Float -> Point -> Point -> ParabolaIntersection
getIntersection d p1 p2 =
    if p1 == p2 then
        SameParabola

    else
        let
            ( x1, y1 ) =
                p1

            ( x2, y2 ) =
                p2
        in
        if y1 < d && y2 > d || y1 > d && y2 < d then
            NoIntersection

        else if y1 /= y2 then
            let
                left =
                    (1 / (y1 - y2)) * (-1 * sqrt ((d * d - d * y1 - d * y2 + y1 * y2) * (x1 * x1 - 2 * x1 * x2 + x2 * x2 + y1 * y1 - 2 * y1 * y2 + y2 * y2)) + d * x1 - d * x2 - x1 * y2 + x2 * y1)

                right =
                    (1 / (y1 - y2)) * (sqrt ((d * d - d * y1 - d * y2 + y1 * y2) * (x1 * x1 - 2 * x1 * x2 + x2 * x2 + y1 * y1 - 2 * y1 * y2 + y2 * y2)) + d * x1 - d * x2 - x1 * y2 + x2 * y1)
            in
            if left == right then
                -- If intersections are equal, it's not a parabola, its a line (y1 == d || y2 == d)
                NoIntersection

            else
                LeftRightIntersection left right

        else if (d - y2) * (x1 - x2) /= 0 then
            SingleIntersection ((x1 + x2) / 2)

        else
            NoIntersection
