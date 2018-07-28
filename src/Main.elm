module Main exposing (..)

import Algorithms
import Css exposing (..)
import CurveRenderer exposing (renderCurve)
import Html
import Html.Attributes
import Html.Styled exposing (Html, button, div, fromUnstyled, h1, h3, img, input, label, text)
import Html.Styled.Attributes exposing (css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import PointsRenderer exposing (renderPoints)
import Random exposing (Seed, initialSeed)
import Set
import Types exposing (Point, PointSet)
import WebGL


---- MODEL ----


type alias MaybeValidation =
    Maybe String


type GenerationStep
    = DisplayingPointPlot Int PointSet


type alias Model =
    { step : GenerationStep
    , seedInput : String
    , validationMessage : MaybeValidation
    }


numPoints : Int
numPoints =
    256


init : ( Model, Cmd Msg )
init =
    let
        initialState =
            { step = DisplayingPointPlot 0 Set.empty
            , seedInput = "0"
            , validationMessage = Nothing
            }
    in
    ( initialState, generatePointsWithRandomSeed DisplayPointPlot 256 )



---- UPDATE ----


type Msg
    = UpdateSeedInput String
    | ValidateSeedInput String
    | GeneratePointsFromRandomSeed
    | DisplayPointPlot Int PointSet
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSeedInput seedInput ->
            ( { model | seedInput = seedInput }, Cmd.none )

        GeneratePointsFromRandomSeed ->
            ( model, generatePointsWithRandomSeed DisplayPointPlot 256 )

        ValidateSeedInput seedInput ->
            case String.toInt seedInput of
                Ok seed ->
                    let
                        points =
                            generatePoints seed numPoints
                    in
                    ( { model | step = DisplayingPointPlot seed points }, Cmd.none )

                Err _ ->
                    ( { model | validationMessage = Just "Invalid seed input" }, Cmd.none )

        DisplayPointPlot seed points ->
            ( { model | step = DisplayingPointPlot seed points }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


pointGenerator : Random.Generator Point
pointGenerator =
    Random.pair (Random.float -1.0 1.0) (Random.float -1.0 1.0)


generatePointsWithRandomSeed : (Int -> PointSet -> msg) -> Int -> Cmd msg
generatePointsWithRandomSeed msgMapper numPoints =
    Random.int 0 Random.maxInt
        |> Random.generate (\seed -> msgMapper seed (generatePoints seed numPoints))


generatePoints : Int -> Int -> PointSet
generatePoints seed =
    generatePointsHelp Set.empty (initialSeed seed)


generatePointsHelp : PointSet -> Seed -> Int -> PointSet
generatePointsHelp accumulator seed numPoints =
    if Set.size accumulator >= numPoints then
        accumulator
    else
        let
            ( point, nextSeed ) =
                Random.step pointGenerator seed
        in
        generatePointsHelp (Set.insert point accumulator) nextSeed numPoints



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        stepView =
            case model.step of
                DisplayingPointPlot seed points ->
                    displayPointPlotView seed points
    in
    div []
        [ h1 [] [ text "Generate a procedural map!" ]
        , seedInputView model
        , stepView
        ]


seedInputView : Model -> Html Msg
seedInputView { seedInput, validationMessage } =
    div []
        [ button [ onClick GeneratePointsFromRandomSeed ] [ text "Generate from random seed" ]
        , label []
            [ text " or enter a seed"
            , input [ type_ "text", value seedInput, onInput (\text -> UpdateSeedInput text) ] []
            , button [ onClick (ValidateSeedInput seedInput) ] [ text "Generate!" ]
            ]
        , div [] [ text (Maybe.withDefault "" validationMessage) ]
        ]


displayPointPlotView : Int -> PointSet -> Html Msg
displayPointPlotView seed points =
    div []
        [ h3 [] [ text ("Plotted points for seed " ++ toString seed) ]
        , div
            [ css
                [ displayFlex
                , justifyContent center
                ]
            ]
            [ div
                [ css [ border3 (px 1) solid (rgb 0 0 0) ]
                ]
                [ fromUnstyled (glViewport [ renderPoints points, renderCurve ( ( -1, -1 ), ( 0, 1 ), ( 1, -1 ) ) ]) ]
            ]
        ]


glViewport : List WebGL.Entity -> Html.Html Msg
glViewport entities =
    WebGL.toHtml
        [ Html.Attributes.width 600
        , Html.Attributes.height 600
        , Html.Attributes.style [ ( "display", "block" ) ]
        ]
        entities



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view >> Html.Styled.toUnstyled
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
