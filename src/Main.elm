module Main exposing (GenerationStep(..), MaybeValidation, Model, Msg(..), defaultPoints, displayPointPlotView, drawFortuneState, generatePoints, generatePointsHelp, generatePointsWithRandomSeed, glViewport, glViewportHeight, glViewportWidth, init, main, pointGenerator, seedInputView, update, view)

import Algorithms exposing (FortuneEvent(..), fortunesAlgorithm)
import Browser exposing (Document)
import Css
import CurveRenderer exposing (renderParabola)
import Debug
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Html.Styled exposing (Html, button, div, fromUnstyled, h1, h3, img, input, label, text)
import Html.Styled.Attributes exposing (css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import LineRenderer exposing (renderLines)
import PointsRenderer exposing (renderPoints)
import Random exposing (Seed, initialSeed)
import Set
import Types exposing (Point, PointList, PointSet)
import WebGL


glViewportHeight : Int
glViewportHeight =
    600


glViewportWidth : Int
glViewportWidth =
    600



---- MODEL ----


type alias MaybeValidation =
    Maybe String


type GenerationStep
    = DisplayingPointPlot Int PointSet


type alias Model =
    { step : GenerationStep
    , mousePos : Point
    , seedInput : String
    , validationMessage : MaybeValidation
    }


defaultPoints : Int
defaultPoints =
    32


init : ( Model, Cmd Msg )
init =
    let
        initialState =
            { step = DisplayingPointPlot 0 (Set.fromList [ ( -0.5, 0.9 ), ( 0, 0 ) ])
            , mousePos = ( 0, 0 )
            , seedInput = "0"
            , validationMessage = Nothing
            }
    in
    ( initialState, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateSeedInput String
    | ValidateSeedInput String
    | GeneratePointsFromRandomSeed
    | DisplayPointPlot Int PointSet
    | UpdateMousePosition Point
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSeedInput seedInput ->
            ( { model | seedInput = seedInput }, Cmd.none )

        GeneratePointsFromRandomSeed ->
            ( model, generatePointsWithRandomSeed DisplayPointPlot defaultPoints )

        ValidateSeedInput seedInput ->
            case String.toInt seedInput of
                Just seed ->
                    let
                        points =
                            generatePoints seed defaultPoints
                    in
                    ( { model | step = DisplayingPointPlot seed points }, Cmd.none )

                Nothing ->
                    ( { model | validationMessage = Just "Invalid seed input" }, Cmd.none )

        DisplayPointPlot seed points ->
            ( { model | step = DisplayingPointPlot seed points }, Cmd.none )

        UpdateMousePosition ( x, y ) ->
            let
                translatedX =
                    x / toFloat glViewportWidth * 2 - 1

                translatedY =
                    y / toFloat glViewportHeight * -2 + 1
            in
            ( { model | mousePos = ( translatedX, translatedY ) }, Cmd.none )

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
                    displayPointPlotView seed model.mousePos points
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


displayPointPlotView : Int -> Point -> PointSet -> Html Msg
displayPointPlotView seed mouseCoordinates points =
    let
        ( mouseX, mouseY ) =
            mouseCoordinates
    in
    div []
        [ h3 [] [ text ("Plotted points for seed " ++ String.fromInt seed) ]
        , div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                ]
            ]
            [ div
                [ css [ Css.border3 (Css.px 1) Css.solid (Css.rgb 0 0 0) ]
                ]
                [ fromUnstyled
                    (glViewport
                        (List.append
                            [ renderLines ( 0, 1, 0 ) [ ( ( -1, mouseY ), ( 1, mouseY ) ) ]
                            , renderPoints ( 1, 0.5, 0.25 ) points
                            ]
                            (drawFortuneState mouseY points)
                        )
                    )
                ]
            ]
        ]


drawFortuneState : Float -> PointSet -> List WebGL.Entity
drawFortuneState sweepLine points =
    let
        state =
            fortunesAlgorithm sweepLine points
    in
    List.map (\p -> renderParabola sweepLine p.focus p.startX p.endX) state.beachLine


glViewport : List WebGL.Entity -> Html.Html Msg
glViewport entities =
    WebGL.toHtml
        [ Html.Attributes.width glViewportWidth
        , Html.Attributes.height glViewportHeight
        , Html.Attributes.style "display" "block"
        , Mouse.onMove (\e -> UpdateMousePosition e.offsetPos)
        ]
        entities



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view =
            \model ->
                { title = "Procedural generation in Elm!", body = [ (view >> Html.Styled.toUnstyled) model ] }
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
