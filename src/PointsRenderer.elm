module PointsRenderer exposing (renderPoints)

import Math.Vector2 exposing (Vec2, vec2)
import Set
import Types exposing (PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


renderPoints : PointSet -> WebGL.Entity
renderPoints points =
    WebGL.entity
        vertexShader
        fragmentShader
        (mesh points)
        {}


mesh : PointSet -> Mesh Vertex
mesh renderPoints =
    renderPoints
        |> Set.toList
        |> List.map (\( x, y ) -> { coordinates = vec2 x y })
        |> WebGL.points


vertexShader : Shader Vertex {} {}
vertexShader =
    [glsl|
        attribute vec2 coordinates;

        void main(void) {
            gl_Position = vec4(coordinates, 0.0, 1.0);
            gl_PointSize = 2.5;
        }
    |]


fragmentShader : Shader {} {} {}
fragmentShader =
    [glsl|
        void main () {
            gl_FragColor = vec4(1, 0.5, 0.25, 1);
        }
    |]
