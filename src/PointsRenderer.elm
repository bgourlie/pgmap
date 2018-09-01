module PointsRenderer exposing (renderPoints)

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Set
import Types exposing (Color, PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


type alias Uniforms =
    { color : Vec3
    }


renderPoints : Color -> PointSet -> WebGL.Entity
renderPoints ( r, g, b ) points =
    WebGL.entity
        vertexShader
        fragmentShader
        (mesh points)
        { color = Math.Vector3.vec3 r g b }


mesh : PointSet -> Mesh Vertex
mesh points =
    points
        |> Set.toList
        |> List.map (\( x, y ) -> { coordinates = vec2 x y })
        |> WebGL.points


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec2 coordinates;

        void main(void) {
            gl_Position = vec4(coordinates, 0.0, 1.0);
            gl_PointSize = 2.5;
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
