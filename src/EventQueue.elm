module EventQueue exposing (Event(..), EventQueue, empty, insert, toList)

import Dict exposing (Dict)
import PairingHeap exposing (PairingHeap)
import Types exposing (Point)


type Event
    = SiteEvent
    | CircleEvent


type alias EventKey =
    ( Point, Int )


eventKey : Point -> Event -> EventKey
eventKey point event =
    case event of
        SiteEvent ->
            ( point, 0 )

        CircleEvent ->
            ( point, 1 )


type alias EventQueue =
    PairingHeap EventKey Event


insert : Point -> Event -> EventQueue -> EventQueue
insert point event queue =
    let
        key =
            eventKey point event
    in
    PairingHeap.insert ( key, event ) queue


empty : EventQueue
empty =
    PairingHeap.empty


toList : EventQueue -> List ( Point, Event )
toList queue =
    queue
        |> PairingHeap.toSortedList
        |> List.map (\( ( point, _ ), event ) -> ( point, event ))
