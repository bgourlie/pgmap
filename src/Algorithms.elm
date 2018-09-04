module Algorithms exposing (FortuneEvent(..), fortunesAlgorithm, initialEventQueue)

import Dict exposing (Dict)
import FortuneTree exposing (FortunePoint(..), FortuneTree(..))
import Set exposing (Set)
import Types exposing (Line, Point, PointList, PointSet)


type alias FortuneState =
    { borders : List Line
    , beachLine : List { focus : Point, startX : Float, endX : Float }
    }


type FortuneEvent
    = SiteEvent Point
    | CircleEvent Point


fortunesAlgorithm : Float -> PointSet -> FortuneState
fortunesAlgorithm directrix sites =
    initialEventQueue directrix sites
        |> fortunesAlgorithmHelp directrix FortuneTree.empty
        |> FortuneTree.flatten
        |> generateFortuneState


fortunesAlgorithmHelp : Float -> FortuneTree -> List FortuneEvent -> FortuneTree
fortunesAlgorithmHelp directrix tree eventQueue =
    case eventQueue of
        [] ->
            tree

        event :: rest ->
            case event of
                SiteEvent site ->
                    fortunesAlgorithmHelp directrix (FortuneTree.insertParabola directrix site tree) rest

                CircleEvent site ->
                    fortunesAlgorithmHelp directrix tree rest


generateFortuneState : List FortunePoint -> FortuneState
generateFortuneState points =
    List.foldl
        (\pointType state ->
            case pointType of
                Leaf focus startX endX ->
                    { state | beachLine = { focus = focus, startX = startX, endX = endX } :: state.beachLine }

                BorderEdge b ->
                    { state | borders = b :: state.borders }

                _ ->
                    state
        )
        { borders = [], beachLine = [] }
        points


{-| Sort sites by their y position in ascending order. If two sites fall on the same Y coordinate, we maintain the site
with the lowest X coordinate and discard the other. This provides us with the initial state of the event queue when
executing Fortune's algorithm.
-}
initialEventQueue : Float -> PointSet -> List FortuneEvent
initialEventQueue directrix points =
    points
        |> Set.filter (\( _, y ) -> y > directrix)
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
