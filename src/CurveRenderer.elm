module CurveRenderer exposing (renderParabola)

import Math.Vector2 exposing (Vec2, vec2)
import Parabola exposing (sampleParabola)
import Set
import Types exposing (Point, PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


{-| Take a point (which we will call the focus) and a straight line (which we will call the directrix). The curve is
drawn by sampling points on the plane for which the distance to the focus is the same as the distance to the closest
point on the directrix. The resulting set of points forms the parabola.
-}
renderParabola : Float -> Point -> Float -> Float -> WebGL.Entity
renderParabola directrix focus startX endX =
    let
        firstPoint =
            sampleParabola focus directrix startX

        lastPoint =
            sampleParabola focus directrix endX

        points =
            generateIntervals startX endX 100
                |> List.map (sampleParabola focus directrix)
    in
    WebGL.entity
        vertexShader
        fragmentShader
        (mesh (firstPoint :: points))
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


generateIntervals : Float -> Float -> Int -> List Float
generateIntervals start stop numIntervals =
    let
        step =
            (stop - start) / toFloat numIntervals
    in
    start :: generateIntervalsHelp numIntervals step stop [ stop ]


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
