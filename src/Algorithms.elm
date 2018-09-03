module Algorithms exposing (FortuneEvent(..), ParabolaIntersection(..), fortunesAlgorithm, getIntersection, initialEventQueue, testIntersection)

import Dict exposing (Dict)
import FortuneTree exposing (FortunePoint(..), FortuneTree(..))
import Set exposing (Set)
import Types exposing (Line, Parabola, Point, PointList, PointSet)


type ParabolaIntersection
    = LeftRightIntersection Float Float
    | SingleIntersection Float
    | SameParabola
    | NoIntersection


type alias FortuneState =
    { completedEdges : List Line
    , incompleteEdges : List Line
    , beachLine : List Parabola
    , intersections : List Float
    }


type FortuneEvent
    = SiteEvent Point
    | CircleEvent Point


fortunesAlgorithm : Float -> PointSet -> FortuneState
fortunesAlgorithm toY sites =
    initialEventQueue toY sites
        |> fortunesAlgorithmHelp FortuneTree.empty
        |> FortuneTree.flatten
        |> generateFortuneState toY


fortunesAlgorithmHelp : FortuneTree -> List FortuneEvent -> FortuneTree
fortunesAlgorithmHelp tree eventQueue =
    case eventQueue of
        [] ->
            tree

        event :: rest ->
            case event of
                SiteEvent site ->
                    fortunesAlgorithmHelp (FortuneTree.insert site tree) rest

                CircleEvent site ->
                    fortunesAlgorithmHelp tree rest


generateFortuneState : Float -> List FortunePoint -> FortuneState
generateFortuneState toY points =
    List.foldl
        (\pointType state ->
            case pointType of
                Curve point ->
                    { state | beachLine = { focus = point, directrix = toY, startX = -1, endX = 1 } :: state.beachLine }

                _ ->
                    state
        )
        { completedEdges = [], incompleteEdges = [], beachLine = [], intersections = [] }
        points


{-| Sort sites by their y position in ascending order. If two sites fall on the same Y coordinate, we maintain the site
with the lowest X coordinate and discard the other. This provides us with the initial state of the event queue when
executing Fortune's algorithm.
-}
initialEventQueue : Float -> PointSet -> List FortuneEvent
initialEventQueue toY points =
    points
        |> Set.filter (\( _, y ) -> y >= toY)
        |> Set.toList
        |> initialEventQueueHelp Dict.empty
        |> Dict.foldl (\y x acc -> SiteEvent ( x, y ) :: acc) []
        |> List.sortBy
            (\event ->
                case event of
                    SiteEvent ( _, y ) ->
                        y

                    _ ->
                        -- there should be no other event types in the list
                        1
            )


initialEventQueueHelp : Dict Float Float -> PointList -> Dict Float Float
initialEventQueueHelp acc points =
    case points of
        ( x, y ) :: rest ->
            let
                updateFn =
                    \maybeX ->
                        case maybeX of
                            Just oldX ->
                                Just (min x oldX)

                            Nothing ->
                                Just x
            in
            initialEventQueueHelp (Dict.update y updateFn acc) rest

        [] ->
            acc


testIntersection : Float -> Point -> Point -> FortuneState
testIntersection directrix p1 p2 =
    let
        intersections =
            getIntersection directrix p1 p2
                |> flattenIntersections
    in
    { completedEdges = []
    , incompleteEdges = []
    , beachLine =
        [ { focus = p1
          , directrix = directrix
          , startX = -1
          , endX = 1
          }
        , { focus = p2
          , directrix = directrix
          , startX = -1
          , endX = 1
          }
        ]
    , intersections = intersections
    }


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


flattenIntersections : ParabolaIntersection -> List Float
flattenIntersections intersection =
    case intersection of
        LeftRightIntersection left right ->
            [ left, right ]

        SingleIntersection i ->
            [ i ]

        _ ->
            []
