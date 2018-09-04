module Algorithms exposing (FortuneEvent(..), fortunesAlgorithm, initialEventQueue)

import Dict exposing (Dict)
import FortuneTree exposing (FortunePoint(..), FortuneTree(..))
import Set exposing (Set)
import Types exposing (Line, Parabola, Point, PointList, PointSet)


type alias FortuneState =
    { completedEdges : List Line
    , incompleteEdges : List Line
    , beachLine : List Parabola
    }


type FortuneEvent
    = SiteEvent Point
    | CircleEvent Point


fortunesAlgorithm : Float -> PointSet -> FortuneState
fortunesAlgorithm toY sites =
    initialEventQueue toY sites
        |> fortunesAlgorithmHelp FortuneTree.empty
        |> FortuneTree.flatten
        |> Debug.log "Flattened"
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
        { completedEdges = [], incompleteEdges = [], beachLine = [] }
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


