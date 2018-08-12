module Algorithms exposing (ySortedPoints)

import BinarySearchTree exposing (Tree)
import Dict exposing (Dict)
import Set exposing (Set)
import Types exposing (Point, PointList, PointSet)


--voronoiDiagram : PointSet -> Set ( Point, Point )
--voronoiDiagram points =
--    points
--        |> Set.toList
--        |> xSortedMinYPoints
--        |> List.foldl (\point queue -> EventQueue.insert point SiteEvent queue) EventQueue.empty
--        |> EventQueue.toList
--        -- Everything here on down is nonsense just to get this to compile
--        |> List.map (\( point, _ ) -> ( point, point ))
--        |> Set.fromList


{-| Sort sites by their y position in ascending order. If two sites fall on the same Y coordinate, we maintain the site
with the lowest X coordinate and discard the other. This provides us with the initial state of the event queue when
executing Fortune's algorithm.
-}
ySortedPoints : PointSet -> PointList
ySortedPoints points =
    points
        |> Set.toList
        |> ySortedPointsHelp Dict.empty
        |> Dict.foldl (\y x acc -> ( x, y ) :: acc) []
        |> List.sortBy (\( x, y ) -> y)


ySortedPointsHelp : Dict Float Float -> PointList -> Dict Float Float
ySortedPointsHelp acc points =
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
            ySortedPointsHelp (Dict.update y updateFn acc) rest

        [] ->
            acc
