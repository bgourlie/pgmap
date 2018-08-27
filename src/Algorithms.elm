module Algorithms exposing (FortuneEvent(..), fortunesAlgorithm, initialEventQueue, testIntersection)

import Dict exposing (Dict)
import FortuneTree exposing (FortunePoint(..), FortuneTree(..))
import Set exposing (Set)
import Types exposing (Line, Parabola, Point, PointList, PointSet)


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
        ( i1, i2 ) =
            getIntersection directrix p1 p2
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
    , intersections =
        [ i1
        , i2
        ]
    }


getIntersection : Float -> Point -> Point -> ( Float, Float )
getIntersection d p1 p2 =
    let
        ( x1, y1 ) =
            p1

        ( x2, y2 ) =
            p2

        i1 =
            (1 / (y1 - y2)) * (-1 * sqrt ((d * d - d * y1 - d * y2 + y1 * y2) * (x1 * x1 - 2 * x1 * x2 + x2 * x2 + y1 * y1 - 2 * y1 * y2 + y2 * y2)) + d * x1 - d * x2 - x1 * y2 + x2 * y1)

        i2 =
            (1 / (y1 - y2)) * (sqrt ((d * d - d * y1 - d * y2 + y1 * y2) * (x1 * x1 - 2 * x1 * x2 + x2 * x2 + y1 * y1 - 2 * y1 * y2 + y2 * y2)) + d * x1 - d * x2 - x1 * y2 + x2 * y1)
    in
    ( i1, i2 )
