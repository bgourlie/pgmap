module Tests exposing (..)

import Algorithms exposing (ySortedPoints)
import Expect
import Set
import Test exposing (..)


all : Test
all =
    describe "Algorithm Tests"
        [ test "ySortedPoints test" <|
            \_ ->
                Set.fromList [ ( 0.2, 1 ), ( 0, 0 ), ( 0.1, 1 ), ( 0.23, -1 ) ]
                    |> ySortedPoints
                    |> Expect.equal [ ( 0.23, -1 ), ( 0, 0 ), ( 0.1, 1 ) ]
        ]
