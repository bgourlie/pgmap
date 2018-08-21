module Tests exposing (..)

import Algorithms exposing (FortuneEvent(..), initialEventQueue)
import Expect
import FortuneTree exposing (FortuneTree(..))
import Set
import Test exposing (..)


all : Test
all =
    describe "Algorithm Tests"
        [ test "initialEventQueue test" <|
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
        , describe "FortuneTree Tests"
            [ test "insert test 1" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.insert ( 1, 0 )
                        |> FortuneTree.insert ( 1, 1 )
                        |> FortuneTree.insert ( 2, 0 )
                        |> FortuneTree.insert ( 2, 1 )
                        |> FortuneTree.insert ( 2, 2 )
                        |> FortuneTree.insert ( 2, 3 )
                        |> FortuneTree.insert ( 3, 0 )
                        |> FortuneTree.insert ( 3, 1 )
                        |> FortuneTree.insert ( 3, 2 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 2, 2 ), ( 3, 2 ), ( 2, 3 ) ]
            , test "insert test 2" <|
                \_ ->
                    FortuneTree.empty
                        |> FortuneTree.flatten
                        |> Expect.equal []
            , test "insert test 3" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ ( 0, 0 ) ]
            , test "insert test 4" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.insert ( 1, 0 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ ( 0, 0 ), ( 1, 0 ) ]
            , test "insert test 5" <|
                \_ ->
                    FortuneTree.singleton ( 1, 0 )
                        |> FortuneTree.insert ( 2, 1 )
                        |> FortuneTree.insert ( 2, 2 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ ( 1, 0 ), ( 2, 1 ), ( 2, 2 ) ]
            ]
        ]
