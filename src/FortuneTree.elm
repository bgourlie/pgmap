module FortuneTree exposing (FortuneTree(..), empty, flatten, insert, singleton)

import DifferenceList exposing (DifferenceList)
import Types exposing (Point)


type FortuneTree
    = Empty
    | Node Point FortuneTree FortuneTree


empty : FortuneTree
empty =
    Empty


singleton : Point -> FortuneTree
singleton site =
    Node site Empty Empty


insert : Point -> FortuneTree -> FortuneTree
insert newPoint tree =
    let
        ( _, newYPos ) =
            newPoint
    in
    case tree of
        Empty ->
            singleton newPoint

        Node point left right ->
            let
                ( _, yPos ) =
                    point
            in
            if newYPos > yPos then
                Node point left (insert newPoint right)
            else if newYPos < yPos then
                Node point (insert newPoint left) right
            else
                case ( left, right ) of
                    ( Node _ _ _, Empty ) ->
                        Node point left (insert newPoint right)

                    _ ->
                        Node point (insert newPoint left) right



{-| Returns a flattened list ordered with leaves first and moving right to left.
-}
flatten : FortuneTree -> List Point
flatten tree =
    flattenHelp tree
        |> DifferenceList.toList


flattenHelp : FortuneTree -> DifferenceList Point
flattenHelp tree =
    case tree of
        Empty ->
            DifferenceList.fromList []

        Node point left right ->
            let
                leftDList =
                    flattenHelp left

                rightDList =
                    flattenHelp right
            in
            DifferenceList.append
                (DifferenceList.append (DifferenceList.fromList [ point ]) leftDList)
                rightDList
