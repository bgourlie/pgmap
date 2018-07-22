module Main exposing (..)

import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onClick, onInput)
import Random


---- MODEL ----


type alias MaybeValidation =
    Maybe String


type Model
    = CollectingGenerationInput String MaybeValidation
    | DisplayingPointPlot Int (List ( Int, Int ))


init : ( Model, Cmd Msg )
init =
    ( CollectingGenerationInput "0" Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateSeedInput String
    | ValidateSeedInput String
    | DisplayPointPlot Int (List ( Int, Int ))
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
                    Random.list 256 <| Random.pair (Random.int 0 255) (Random.int 0 255)
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
                    getGenerateInputView seedInput validation

                DisplayingPointPlot seed points ->
                    div []
                        [ h3 [] [ text ("Generating map using seed " ++ toString seed) ]
                        ]
    in
    div [] [ stepView ]


getGenerateInputView : String -> MaybeValidation -> Html Msg
getGenerateInputView seedInput validation =
    div []
        [ h1 [] [ text "Generate a procedural map!" ]
        , label []
            [ text "Enter a seed"
            , input [ type_ "text", value seedInput, onInput (\text -> UpdateSeedInput text) ] []
            , button [ onClick (ValidateSeedInput seedInput) ] [ text "Generate!" ]
            ]
        , div [] [ text (Maybe.withDefault "" validation) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
