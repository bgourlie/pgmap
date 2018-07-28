module CurveRenderer exposing (renderCurve)

import Math.Vector2 exposing (Vec2, vec2)
import Set
import Types exposing (Point, PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


renderCurve : ( Point, Point, Point ) -> WebGL.Entity
renderCurve controlPoints =
    let
        points =
            generateIntervals 100
                |> List.map (\t -> bezier t controlPoints)
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


generateIntervals : Int -> List Float
generateIntervals numIntervals =
    let
        step =
            1.0 / toFloat numIntervals
    in
    0 :: generateIntervalsHelp numIntervals step 1 [ 1 ]


generateIntervalsHelp : Int -> Float -> Float -> List Float -> List Float
generateIntervalsHelp remainingIntervals step curInterval acc =
    if remainingIntervals <= 0 then
        acc
    else
        let
            nextInterval =
                curInterval - step
        in
        generateIntervalsHelp (remainingIntervals - 1) step nextInterval (nextInterval :: acc)


testSamples : List Point
testSamples =
    generateIntervals 100
        |> List.map (\t -> bezier t ( ( -1, -1 ), ( 0.25, 1 ), ( 1, -1 ) ))
