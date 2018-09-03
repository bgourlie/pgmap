module Tests exposing (all)

import Algorithms exposing (FortuneEvent(..), ParabolaIntersection(..), getIntersection, initialEventQueue)
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
        , describe "Parabola intersection tests"
            [ test "y1 == y2 == d should not intersect" <|
                \_ ->
                    getIntersection 0.9 ( 0.5, 0.9 ) ( 0.6, 0.9 )
                        |> Expect.equal NoIntersection
            , test "y1 == y2 != d should have a single intersection" <|
                \_ ->
                    getIntersection 0.95 ( -0.5, 0.9 ) ( 0, 0.9 )
                        |> Expect.equal (SingleIntersection -0.25)
            , test "y1 != y2 == d should not intersect" <|
                \_ ->
                    getIntersection 0.9 ( -0.5, 0.8 ) ( 0, 0.9 )
                        |> Expect.equal NoIntersection
            , test "y1 != y2 != d should have two intersections" <|
                \_ ->
                    getIntersection 0.73 ( -0.5, 0.8 ) ( 0, 0.9 )
                        |> Expect.equal (LeftRightIntersection -0.2937626405930644 -1.4062373594069364)
            , test "y1 < d < y2 should not intersect" <|
                \_ ->
                    getIntersection 0.85 ( -0.5, 0.8 ) ( 0, 0.9 )
                        |> Expect.equal NoIntersection
            , test "y2 < d < y1 should not intersect" <|
                \_ ->
                    getIntersection 0.85 ( 0, 0.9 ) ( -0.5, 0.8 )
                        |> Expect.equal NoIntersection
            , test "y1 == y2 and x1 == x2 are the same parabola" <|
                \_ ->
                    getIntersection 0.85 ( 0, 0.9 ) ( 0, 0.9 )
                        |> Expect.equal SameParabola
            ]
        , describe "FortuneTree Tests"
            [ test "insert subtree test 1" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 0, 1 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 1, 1 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 0, 2 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 1, 2 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 2, 2 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 3, 2 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 0, 3 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 1, 3 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 2, 3 ))
                        |> FortuneTree.flatten
                        |> Expect.equal [ Edge ( 0, 0 ), Edge ( 0, 1 ), Edge ( 0, 2 ), Curve ( 0, 3 ), Edge ( 1, 1 ), Edge ( 1, 2 ), Curve ( 1, 3 ), Edge ( 2, 2 ), Curve ( 2, 3 ), Edge ( 3, 2 ) ]
            , test "insert subtree test 2" <|
                \_ ->
                    FortuneTree.empty
                        |> FortuneTree.flatten
                        |> Expect.equal []
            , test "insert subtree test 3" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.flatten
                        |> Expect.equal [ Curve ( 0, 0 ) ]
            , test "insert subtree test 4" <|
                \_ ->
                    FortuneTree.singleton ( 0, 0 )
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 1, 0 ))
                        |> FortuneTree.flatten
                        |> Expect.equal [ Edge ( 0, 0 ), Curve ( 1, 0 ) ]
            , test "insert subtree test 5" <|
                \_ ->
                    FortuneTree.singleton ( 1, 0 )
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 2, 1 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 2, 2 ))
                        |> FortuneTree.flatten
                        |> Expect.equal [ Edge ( 1, 0 ), Edge ( 2, 1 ), Curve ( 2, 2 ) ]
            , test "insert subtree test 6" <|
                \_ ->
                    FortuneTree.singleton ( 1, 0 )
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( -1, 1 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 2, 2 ))
                        |> FortuneTree.flatten
                        |> Expect.equal [ Curve ( -1, 1 ), Edge ( 1, 0 ), Curve ( 2, 2 ) ]
            , test "insert subtree test 7" <|
                \_ ->
                    FortuneTree.singleton ( 25, 0 )
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 15, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 50, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 10, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 22, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 35, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 70, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 4, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 12, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 18, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 24, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 31, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 44, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 66, 0 ))
                        |> FortuneTree.insertSubtree (FortuneTree.singleton ( 90, 0 ))
                        |> FortuneTree.flatten
                        |> Expect.equal [ Curve ( 4, 0 ), Edge ( 10, 0 ), Curve ( 12, 0 ), Edge ( 15, 0 ), Curve ( 18, 0 ), Edge ( 22, 0 ), Curve ( 24, 0 ), Edge ( 25, 0 ), Curve ( 31, 0 ), Edge ( 35, 0 ), Curve ( 44, 0 ), Edge ( 50, 0 ), Curve ( 66, 0 ), Edge ( 70, 0 ), Curve ( 90, 0 ) ]
            ]
        ]
