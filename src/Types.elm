module Types exposing (Color, Line, Point, PointList, PointSet)

import Math.Vector2 exposing (Vec2)
import Set exposing (Set)


type alias Point =
    ( Float, Float )


type alias Color =
    ( Float, Float, Float )


type alias Line =
    ( Point, Point )


type alias PointSet =
    Set Point


type alias PointList =
    List Point
