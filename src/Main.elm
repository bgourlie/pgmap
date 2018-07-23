module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (Html, button, div, fromUnstyled, h1, h3, img, input, label, text)
import Html.Styled.Attributes exposing (css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Random exposing (Seed, initialSeed)
import Renderer
import Types exposing (Point, PointList)


---- MODEL ----


type alias MaybeValidation =
    Maybe String


type GenerationStep
    = DisplayingPointPlot Int PointList


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
            { step = DisplayingPointPlot 0 []
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
    | DisplayPointPlot Int PointList
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


generatePointsWithRandomSeed : (Int -> PointList -> msg) -> Int -> Cmd msg
generatePointsWithRandomSeed msgMapper numPoints =
    Random.int 0 Random.maxInt
        |> Random.generate (\seed -> msgMapper seed (generatePoints seed numPoints))


generatePoints : Int -> Int -> PointList
generatePoints seed =
    generatePointsHelp [] (initialSeed seed)


generatePointsHelp : PointList -> Seed -> Int -> PointList
generatePointsHelp accumulator seed numPoints =
    if numPoints <= 0 then
        accumulator
    else
        let
            ( point, nextSeed ) =
                Random.step pointGenerator seed
        in
        generatePointsHelp (point :: accumulator) nextSeed (numPoints - 1)



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


displayPointPlotView : Int -> PointList -> Html Msg
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
                [ fromUnstyled (Renderer.renderPoints points) ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view >> Html.Styled.toUnstyled
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
