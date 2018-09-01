module Tests exposing (all)

import Algorithms exposing (FortuneEvent(..), initialEventQueue)
import Expect
import FortuneTree exposing (FortunePoint(..), FortuneTree(..))
import Set
import Test exposing (..)


all : Test
all =
    describe "Algorithm Tests"
        [ test "initialEventQueue test" <|
            \_ ->
                Set.fromList [ ( 0.2, 1.0 ), ( 0.0, 0.0 ), ( 0.1, 1.0 ), ( 0.23, -1.0 ) ]
                    |> initialEventQueue -1.0
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
                        |> FortuneTree.insert ( 0, 1 )
                        |> FortuneTree.insert ( 1, 1 )
                        |> FortuneTree.insert ( 0, 2 )
                        |> FortuneTree.insert ( 1, 2 )
                        |> FortuneTree.insert ( 2, 2 )
                        |> FortuneTree.insert ( 3, 2 )
                        |> FortuneTree.insert ( 0, 3 )
                        |> FortuneTree.insert ( 1, 3 )
                        |> FortuneTree.insert ( 2, 3 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ Edge ( 0, 0 ), Edge ( 0, 1 ), Edge ( 0, 2 ), Curve ( 0, 3 ), Edge ( 1, 1 ), Edge ( 1, 2 ), Curve ( 1, 3 ), Edge ( 2, 2 ), Curve ( 2, 3 ), Edge ( 3, 2 ) ]
            , test "insert test 2" <|
                \_ ->
                    FortuneTree.empty
                        |> FortuneTree.flatten
                        |> Expect.equal []
            , test "insert test 3" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ Curve ( 0, 0 ) ]
            , test "insert test 4" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.insert ( 1, 0 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ Edge ( 0, 0 ), Curve ( 1, 0 ) ]
            , test "insert test 5" <|
                \_ ->
                    FortuneTree.singleton ( 1, 0 )
                        |> FortuneTree.insert ( 2, 1 )
                        |> FortuneTree.insert ( 2, 2 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ Edge ( 1, 0 ), Edge ( 2, 1 ), Curve ( 2, 2 ) ]
            , test "insert test 6" <|
                \_ ->
                    FortuneTree.singleton ( 1, 0 )
                        |> FortuneTree.insert ( -1, 1 )
                        |> FortuneTree.insert ( 2, 2 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ Curve ( -1, 1 ), Edge ( 1, 0 ), Curve ( 2, 2 ) ]
            , test "insert test 7" <|
                \_ ->
                    FortuneTree.singleton ( 25, 0 )
                        |> FortuneTree.insert ( 15, 0 )
                        |> FortuneTree.insert ( 50, 0 )
                        |> FortuneTree.insert ( 10, 0 )
                        |> FortuneTree.insert ( 22, 0 )
                        |> FortuneTree.insert ( 35, 0 )
                        |> FortuneTree.insert ( 70, 0 )
                        |> FortuneTree.insert ( 4, 0 )
                        |> FortuneTree.insert ( 12, 0 )
                        |> FortuneTree.insert ( 18, 0 )
                        |> FortuneTree.insert ( 24, 0 )
                        |> FortuneTree.insert ( 31, 0 )
                        |> FortuneTree.insert ( 44, 0 )
                        |> FortuneTree.insert ( 66, 0 )
                        |> FortuneTree.insert ( 90, 0 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ Curve ( 4, 0 ), Edge ( 10, 0 ), Curve ( 12, 0 ), Edge ( 15, 0 ), Curve ( 18, 0 ), Edge ( 22, 0 ), Curve ( 24, 0 ), Edge ( 25, 0 ), Curve ( 31, 0 ), Edge ( 35, 0 ), Curve ( 44, 0 ), Edge ( 50, 0 ), Curve ( 66, 0 ), Edge ( 70, 0 ), Curve ( 90, 0 ) ]
            ]
        ]
