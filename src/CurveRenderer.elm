module CurveRenderer exposing (renderBezierCurve, renderParabola)

import Algorithms exposing (sampleParabola)
import Math.Vector2 exposing (Vec2, vec2)
import Set
import Types exposing (Parabola, Point, PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


renderBezierCurve : ( Point, Point, Point ) -> WebGL.Entity
renderBezierCurve controlPoints =
    let
        points =
            generateIntervals0To1 100
                |> List.map (\t -> bezier t controlPoints)
    in
    WebGL.entity
        vertexShader
        fragmentShader
        (mesh points)
        {}


{-| Take a point (which we will call the focus) and a straight line (which we will call the directrix). The curve is
drawn by sampling points on the plane for which the distance to the focus is the same as the distance to the closest
point on the directrix. The resulting set of points forms the parabola.
-}
renderParabola : Parabola -> WebGL.Entity
renderParabola { focus, directrix, startX, endX } =
    let
        points =
            generateIntervalsNeg1To1 100
                |> List.filterMap
                    (\x ->
                        if x >= startX && x <= endX then
                            Just (sampleParabola focus directrix x)

                        else
                            Nothing
                    )
    in
    WebGL.entity
        vertexShader
        fragmentShader
        (mesh points)
        {}


mesh : List Point -> Mesh Vertex
mesh points =
    points
        |> List.map (\( x, y ) -> { coordinates = vec2 x y })
        |> WebGL.lineStrip


vertexShader : Shader Vertex {} {}
vertexShader =
    [glsl|
        attribute vec2 coordinates;
        void main(void) {
            gl_Position = vec4(coordinates, 0.0, 1.0);
        }
    |]


fragmentShader : Shader {} {} {}
fragmentShader =
    [glsl|
        void main () {
            gl_FragColor = vec4(1, 0.5, 0.25, 1);
        }
    |]


bezier : Float -> ( Point, Point, Point ) -> Point
bezier t ( p1, p2, p3 ) =
    let
        ( p1x, p1y ) =
            p1

        ( p2x, p2y ) =
            p2

        ( p3x, p3y ) =
            p3

        t2 =
            t * t

        mt =
            1 - t

        mt2 =
            mt * mt
    in
    ( p1x * mt2 + p2x * 2 * mt * t + p3x * t2
    , p1y * mt2 + p2y * 2 * mt * t + p3y * t2
    )


generateIntervals0To1 : Int -> List Float
generateIntervals0To1 numIntervals =
    let
        step =
            1.0 / toFloat numIntervals
    in
    0 :: generateIntervals0To1Help numIntervals step 1 [ 1 ]


generateIntervals0To1Help : Int -> Float -> Float -> List Float -> List Float
generateIntervals0To1Help remainingIntervals step curInterval acc =
    if remainingIntervals <= 0 then
        acc

    else
        let
            nextInterval =
                curInterval - step
        in
        generateIntervals0To1Help (remainingIntervals - 1) step nextInterval (nextInterval :: acc)


generateIntervalsNeg1To1 : Int -> List Float
generateIntervalsNeg1To1 numIntervals =
    let
        step =
            1.0 / (toFloat numIntervals / 2)
    in
    -1 :: generateIntervalsNeg1To1Help numIntervals step 1 [ 1 ]


generateIntervalsNeg1To1Help : Int -> Float -> Float -> List Float -> List Float
generateIntervalsNeg1To1Help remainingIntervals step curInterval acc =
    if remainingIntervals <= 0 then
        acc

    else
        let
            nextInterval =
                curInterval - step
        in
        generateIntervals0To1Help (remainingIntervals - 1) step nextInterval (nextInterval :: acc)
