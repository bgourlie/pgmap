module FortuneTree exposing (FortunePoint(..), FortuneTree(..), empty, flatten, insertParabola, insertSubtree, singleton)

import DifferenceList exposing (DifferenceList)
import Parabola exposing (sampleParabola)
import Types exposing (Line, Point)


type NodeValue
    = UninterruptedParabola Point
    | LeftIntersectingParabola Point Float
    | RightIntersectingParabola Point Float
    | LeftRightIntersectingParabola Point Float Float
    | Border Point Line -- The point is the focus of the parabola that created the initial intersection


type ParabolaIntersection
    = LeftRightIntersection Float Float
    | SingleIntersection Float
    | SameParabola
    | NoIntersection


type FortuneTree
    = Empty
    | Node NodeValue FortuneTree FortuneTree


type FortunePoint
    = ParabolaEdge Point
    | BorderEdge Line
    | Leaf Point Float Float


getFocus : NodeValue -> Point
getFocus val =
    case val of
        UninterruptedParabola focus ->
            focus

        LeftIntersectingParabola focus _ ->
            focus

        RightIntersectingParabola focus _ ->
            focus

        LeftRightIntersectingParabola focus _ _ ->
            focus

        Border focus _ ->
            focus


empty : FortuneTree
empty =
    Empty


singleton : Point -> FortuneTree
singleton site =
    Node (UninterruptedParabola site) Empty Empty


insertParabola : Float -> Point -> FortuneTree -> FortuneTree
insertParabola directrix newFocus tree =
    let
        maybeParentNodeValue =
            findParent newFocus tree Nothing
    in
    case maybeParentNodeValue of
        Nothing ->
            singleton newFocus

        Just parentNodeValue ->
            let
                parentFocus =
                    getFocus parentNodeValue

                ( leftIntersection, maybeRightIntersection ) =
                    case getIntersection directrix parentFocus newFocus of
                        LeftRightIntersection li ri ->
                            ( li, Just ri )

                        SingleIntersection i ->
                            ( i, Nothing )

                        SameParabola ->
                            Debug.todo "SameParabola This should never happen but how can we model that?"

                        NoIntersection ->
                            Debug.todo "NoIntersection This should never happen but how can we model that?"

                leftIntersectingPoint =
                    sampleParabola newFocus directrix leftIntersection

                leftSubTree =
                    Node (RightIntersectingParabola parentFocus leftIntersection) Empty Empty

                rightSubTree =
                    case maybeRightIntersection of
                        Just rightIntersection ->
                            let
                                rightIntersectingPoint =
                                    sampleParabola newFocus directrix rightIntersection
                            in
                            Node (Border newFocus ( leftIntersectingPoint, rightIntersectingPoint ))
                                (Node (LeftRightIntersectingParabola newFocus leftIntersection rightIntersection) Empty Empty)
                                (Node (LeftIntersectingParabola parentFocus rightIntersection) Empty Empty)

                        Nothing ->
                            Node (LeftIntersectingParabola newFocus leftIntersection) Empty Empty
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
                            getFocus newNode

                        curNodeCompareValue =
                            getFocus curNode
                    in
                    if newNodeCompareValue > curNodeCompareValue then
                        Node curNode left (insertSubtree subTree right)

                    else
                        Node curNode (insertSubtree subTree left) right


findParent : Point -> FortuneTree -> Maybe NodeValue -> Maybe NodeValue
findParent newPoint tree parentNode =
    case tree of
        Empty ->
            parentNode

        Node n left right ->
            let
                curNodeComparisonValue =
                    getFocus n
            in
            if newPoint > curNodeComparisonValue then
                findParent newPoint right (Just n)

            else
                findParent newPoint left (Just n)


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

        Node nodeValue left right ->
            let
                pointType =
                    case ( left, right ) of
                        ( Empty, Empty ) ->
                            case nodeValue of
                                UninterruptedParabola f ->
                                    Leaf f -1.0 1.0

                                LeftIntersectingParabola f i ->
                                    Leaf f i 1.0

                                RightIntersectingParabola f i ->
                                    Leaf f -1.0 i

                                LeftRightIntersectingParabola f il ir ->
                                    Leaf f il ir

                                Border _ _ ->
                                    Debug.todo "XX Should never happen but how do we model that?"

                        _ ->
                            case nodeValue of
                                UninterruptedParabola f ->
                                    ParabolaEdge f

                                LeftIntersectingParabola f _ ->
                                    ParabolaEdge f

                                RightIntersectingParabola f _ ->
                                    ParabolaEdge f

                                LeftRightIntersectingParabola f _ _ ->
                                    ParabolaEdge f

                                Border _ b ->
                                    BorderEdge b
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
