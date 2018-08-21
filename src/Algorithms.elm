module Algorithms exposing (FortuneEvent(..), fortunesAlgorithm, initialEventQueue)

import Dict exposing (Dict)
import FortuneTree exposing (FortuneTree(..))
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


fortunesAlgorithm : PointSet -> Float -> FortuneState
fortunesAlgorithm sites toY =
    initialEventQueue sites
        |> List.filterMap
            (\event ->
                case event of
                    SiteEvent ( x, y ) ->
                        if y >= toY then
                            Just { focus = ( x, y ), directrix = toY, startX = -1, endX = 1 }
                        else
                            Nothing

                    CircleEvent _ ->
                        Nothing
            )
        |> (\parabolas -> { completedEdges = [], incompleteEdges = [], beachLine = parabolas })


fortunesAlgorithmHelp : List FortuneEvent -> FortuneTree -> FortuneTree
fortunesAlgorithmHelp eventQueue tree =
    case eventQueue of
        [] ->
            tree

        event :: rest ->
            case event of
                SiteEvent ( x, y ) ->
                    fortunesAlgorithmHelp rest tree

                CircleEvent ( x, y ) ->
                    fortunesAlgorithmHelp rest tree


{-| Sort sites by their y position in ascending order. If two sites fall on the same Y coordinate, we maintain the site
with the lowest X coordinate and discard the other. This provides us with the initial state of the event queue when
executing Fortune's algorithm.
-}
initialEventQueue : PointSet -> List FortuneEvent
initialEventQueue points =
    points
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
