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


type alias Banks =
    Array Int


type alias Model =
    { banks : Banks
    , history : List Banks
    , finished : Bool
    }


type Msg
    = InputChanged String
    | Redistribute ()
    | Halt ()


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { banks = Array.empty
      , history = []
      , finished = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged inputString ->
            let
                banks =
                    inputString
                        |> String.words
                        |> List.filterMap (String.toInt >> Result.toMaybe)
                        |> Array.fromList
            in
                ( { model
                    | banks = banks
                    , history = [ banks ]
                  }
                , Task.perform Redistribute (Task.succeed ())
                )

        Redistribute _ ->
            let
                newBanksArrangement =
                    redistribute model.banks

                seenBefore =
                    List.member newBanksArrangement model.history

                newModel =
                    { model
                        | banks = newBanksArrangement
                        , history = newBanksArrangement :: model.history
                    }
            in
                if seenBefore || model.finished then
                    ( newModel
                    , Task.perform Halt (Task.succeed ())
                    )
                else
                    ( newModel
                    , Task.perform Redistribute (Task.succeed ())
                    )

        Halt _ ->
            ( { model | finished = True }, Cmd.none )


redistribute : Array Int -> Array Int
redistribute banks =
    let
        ( maxPos, maxVal ) =
            findMax banks

        clearedBanks =
            Array.set maxPos 0 banks

        size =
            Array.length banks

        startPos =
            (maxPos + 1) % size
    in
        distribute size startPos maxVal clearedBanks


distribute : Int -> Int -> Int -> Array Int -> Array Int
distribute size pos blocksLeft banks =
    if blocksLeft == 0 then
        banks
    else
        let
            val =
                Array.get pos banks
                    |> Maybe.withDefault 0

            newBanks =
                Array.set pos (val + 1) banks

            newPos =
                (pos + 1) % size
        in
            distribute size newPos (blocksLeft - 1) newBanks


findMax : Array Int -> ( Int, Int )
findMax banks =
    findMaxHelp banks -1 -1 0


findMaxHelp : Array Int -> Int -> Int -> Int -> ( Int, Int )
findMaxHelp banks maxPos maxVal pos =
    case Array.get pos banks of
        Nothing ->
            ( maxPos, maxVal )

        Just val ->
            if val > maxVal then
                findMaxHelp banks pos val (pos + 1)
            else
                findMaxHelp banks maxPos maxVal (pos + 1)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 6: Memory Reallocation" ]
        , textarea
            [ placeholder "Enter input here"
            , onInput InputChanged
            , rows 10
            , style [ ( "width", "500px" ) ]
            ]
            []
        , p []
            [ text ("# cycles to repeat: " ++ toString ((List.length model.history) - 1))
            ]
        , p []
            [ text <|
                if model.finished then
                    "Finished"
                else
                    "Working..."
            ]
        , p []
            [ text "In case of halting problem, click below"
            ]
        , button [ onClick (Halt ()) ] [ text "HALT" ]
        ]
