module Types exposing (..)

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


type alias Parabola =
    { focus : Point
    , directrix : Float
    , startX : Float
    , endX : Float
    }
