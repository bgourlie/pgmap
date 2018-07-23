module Renderer exposing (renderPoints)

import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3)
import Set
import Types exposing (PointSet)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { coordinates : Vec2
    }


type alias Uniforms =
    { perspective : Mat4 }


renderPoints : PointSet -> Html msg
renderPoints points =
    WebGL.toHtml
        [ width 600
        , height 600
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (mesh points)
            {}
        ]


mesh : PointSet -> Mesh Vertex
mesh points =
    points
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
