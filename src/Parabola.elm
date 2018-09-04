module Parabola exposing (sampleParabola)

import Types exposing (Point)


sampleParabola : Point -> Float -> Float -> Point
sampleParabola ( focusX, focusY ) directrix x =
    let
        fx =
            x - focusX

        y =
            (1 / (2 * (focusY - directrix))) * (fx * fx) + ((focusY + directrix) / 2)
    in
    ( x, y )
