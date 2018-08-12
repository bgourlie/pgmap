module Tests exposing (..)

import Algorithms exposing (FortuneEvent(..), initialEventQueue)
import Expect
import Set
import Test exposing (..)


all : Test
all =
    describe "Algorithm Tests"
        [ test "ySortedPoints test" <|
            \_ ->
                Set.fromList [ ( 0.2, 1 ), ( 0, 0 ), ( 0.1, 1 ), ( 0.23, -1 ) ]
                    |> initialEventQueue
                    |> List.map
                        (\event ->
                            case event of
                                SiteEvent point ->
                                    point

                                CircleEvent point ->
                                    point
                        )
                    |> Expect.equal [ ( 0.23, -1 ), ( 0, 0 ), ( 0.1, 1 ) ]
        ]
