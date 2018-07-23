module Algorithms exposing (voronoiDiagram)

import BinarySearchTree exposing (Tree)
import Dict exposing (Dict)
import EventQueue exposing (Event(..), EventQueue)
import Set exposing (Set)
import Types exposing (Point, PointList, PointSet)


voronoiDiagram : PointSet -> Set ( Point, Point )
voronoiDiagram points =
    points
        |> Set.toList
        |> xSortedMinYPoints
        |> List.foldl (\point queue -> EventQueue.insert point SiteEvent queue) EventQueue.empty
        |> EventQueue.toList
        -- Everything here on down is nonsense just to get this to compile
        |> List.map (\( point, _ ) -> ( point, point ))
        |> Set.fromList



-- hack just to compile


{-| Transform a list of points into a list of points with minimal y-coordinate ordered by the x-coordinate
-}
xSortedMinYPoints : PointList -> PointList
xSortedMinYPoints points =
    xToMinYDict points Dict.empty
        |> Dict.toList


xToMinYDict : PointList -> Dict Float Float -> Dict Float Float
xToMinYDict points acc =
    case points of
        ( x, y ) :: rest ->
            let
                updateFn =
                    \maybeY ->
                        case maybeY of
                            Just oldY ->
                                Just (min y oldY)

                            Nothing ->
                                Just y
            in
            xToMinYDict rest (Dict.update x updateFn acc)

        [] ->
            acc
