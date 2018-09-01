module LineRenderer exposing (renderLines)

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Set
import Types exposing (Color, Line, PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


type alias Uniforms =
    { color : Vec3 }


renderLines : Color -> List Line -> WebGL.Entity
renderLines ( r, g, b ) points =
    WebGL.entity
        vertexShader
        fragmentShader
        (mesh points)
        { color = Math.Vector3.vec3 r g b }


mesh : List Line -> Mesh Vertex
mesh points =
    points
        |> List.map (\( ( x1, y1 ), ( x2, y2 ) ) -> ( { coordinates = vec2 x1 y1 }, { coordinates = vec2 x2 y2 } ))
        |> WebGL.lines


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec2 coordinates;
        void main(void) {
            gl_Position = vec4(coordinates, 0.0, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;

        void main () {
            gl_FragColor = vec4(color, 1);
        }
    |]
