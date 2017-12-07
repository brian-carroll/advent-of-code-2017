module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Task
import Array.Hamt as Array exposing (Array)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { instructions : Array Int
    , programCounter : Int
    , cycles : Int
    }


type Msg
    = InputChanged String
    | Execute ()
    | Halt ()


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { instructions = Array.empty
      , programCounter = 0
      , cycles = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged inputString ->
            ( { model
                | instructions =
                    inputString
                        |> String.lines
                        |> List.filterMap (String.toInt >> Result.toMaybe)
                        |> Array.fromList
              }
            , Task.perform Execute (Task.succeed ())
            )

        Execute _ ->
            case execute model of
                Just newModel ->
                    ( newModel
                    , Task.perform Execute (Task.succeed ())
                    )

                Nothing ->
                    ( model
                    , Task.perform Halt (Task.succeed ())
                    )

        Halt _ ->
            ( model, Cmd.none )


execute : Model -> Maybe Model
execute { instructions, programCounter, cycles } =
    case Array.get programCounter instructions of
        Nothing ->
            Nothing

        Just jump ->
            Just
                { programCounter =
                    programCounter + jump
                , instructions =
                    Array.set programCounter (jump + 1) instructions
                , cycles = cycles + 1
                }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 5: A Maze of Twisty Trampolines, All Alike" ]
        , textarea
            [ placeholder "Enter input here"
            , onInput InputChanged
            , rows 10
            , style [ ( "width", "500px" ) ]
            ]
            []
        , p []
            [ text ("# cycles to escape: " ++ toString model.cycles)
            ]
        , p []
            [ text "In case of halting problem, click below"
            ]
        , button [ onClick (Halt ()) ] [ text "HALT" ]
        ]
