module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (Html, button, div, fromUnstyled, h1, h3, img, input, label, text)
import Html.Styled.Attributes exposing (css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Random
import Renderer
import Types exposing (PointList)


---- MODEL ----


type alias MaybeValidation =
    Maybe String


type Model
    = CollectingGenerationInput String MaybeValidation
    | DisplayingPointPlot Int PointList


init : ( Model, Cmd Msg )
init =
    ( CollectingGenerationInput "0" Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateSeedInput String
    | ValidateSeedInput String
    | DisplayPointPlot Int PointList
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSeedInput seedInput ->
            case model of
                CollectingGenerationInput _ _ ->
                    ( CollectingGenerationInput seedInput Nothing, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ValidateSeedInput seedInput ->
            let
                pointGenerator =
                    Random.list 256 <| Random.pair (Random.float -1.0 1.0) (Random.float -1.0 1.0)
            in
            case String.toInt seedInput of
                Ok seed ->
                    ( CollectingGenerationInput seedInput Nothing, Random.generate (\points -> DisplayPointPlot seed points) pointGenerator )

                Err _ ->
                    ( CollectingGenerationInput seedInput (Just "Invalid seed input"), Cmd.none )

        DisplayPointPlot seed points ->
            ( DisplayingPointPlot seed points, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        stepView =
            case model of
                CollectingGenerationInput seedInput validation ->
                    generateInputView seedInput validation

                DisplayingPointPlot seed points ->
                    displayPointPlotView seed points
    in
    div [] [ stepView ]


generateInputView : String -> MaybeValidation -> Html Msg
generateInputView seedInput validation =
    div []
        [ h1 [] [ text "Generate a procedural map!" ]
        , label []
            [ text "Enter a seed"
            , input [ type_ "text", value seedInput, onInput (\text -> UpdateSeedInput text) ] []
            , button [ onClick (ValidateSeedInput seedInput) ] [ text "Generate!" ]
            ]
        , div [] [ text (Maybe.withDefault "" validation) ]
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
