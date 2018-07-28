module LineRenderer exposing (renderLines)

import Math.Vector2 exposing (Vec2, vec2)
import Set
import Types exposing (Point, PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


renderLines : List ( Point, Point ) -> WebGL.Entity
renderLines points =
    WebGL.entity
        vertexShader
        fragmentShader
        (mesh points)
        {}


mesh : List ( Point, Point ) -> Mesh Vertex
mesh points =
    points
        |> List.map (\( ( x1, y1 ), ( x2, y2 ) ) -> ( { coordinates = vec2 x1 y2 }, { coordinates = vec2 x2 y2 } ))
        |> WebGL.lines


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
