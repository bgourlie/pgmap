module Algorithms exposing (FortuneEvent(..), initialEventQueue)

import BinarySearchTree exposing (Tree)
import Dict exposing (Dict)
import Set exposing (Set)
import Types exposing (Point, PointList, PointSet)


type FortuneEvent
    = SiteEvent Point
    | CircleEvent Point


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
