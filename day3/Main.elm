module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Solution =
    { part1 : Int
    , part2 : Int
    }


type alias Model =
    { input : Int
    , output : Result String Solution
    }


type Msg
    = InputChanged String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


initMessage : String
initMessage =
    "Please enter a number to start"


init : ( Model, Cmd Msg )
init =
    ( { input = 0
      , output = Err initMessage
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged inputString ->
            let
                nextModel =
                    case validate inputString of
                        Err errMsg ->
                            { input = 0
                            , output = Err errMsg
                            }

                        Ok input ->
                            { input = input
                            , output =
                                Ok
                                    { part1 = solvePart1 input
                                    , part2 = solvePart2 input
                                    }
                            }
            in
                ( nextModel
                , Cmd.none
                )


validate : String -> Result String Int
validate input =
    if input == "" then
        Err initMessage
    else
        input
            |> String.trim
            |> String.toInt


type Direction
    = Right
    | Left
    | Up
    | Down



{-
   17  16  15  14  13
   18   5   4   3  12
   19   6   1   2  11
   20   7   8   9  10
   21  22  23---> ...

-}


solvePart1 : Int -> Int
solvePart1 input =
    let
        ( x, y ) =
            move input 1 1 Right 0 0
    in
        (Basics.abs x) + (Basics.abs y)


move : Int -> Int -> Int -> Direction -> Int -> Int -> ( Int, Int )
move target current sideLength direction x y =
    if target == current then
        ( x, y )
    else
        case direction of
            Up ->
                if y == sideLength // 2 then
                    move target (current + 1) sideLength Left (x - 1) y
                else
                    move target (current + 1) sideLength Up x (y + 1)

            Left ->
                if x == (negate sideLength) // 2 then
                    move target (current + 1) sideLength Down x (y - 1)
                else
                    move target (current + 1) sideLength Left (x - 1) y

            Down ->
                if y == (negate sideLength) // 2 then
                    move target (current + 1) sideLength Right (x + 1) y
                else
                    move target (current + 1) sideLength Down x (y - 1)

            Right ->
                if x == sideLength // 2 then
                    move target (current + 1) (sideLength + 2) Up (x + 1) y
                else
                    move target (current + 1) sideLength Right (x + 1) y


solvePart2 : Int -> Int
solvePart2 input =
    let
        grid =
            Dict.insert ( 0, 0 ) 1 Dict.empty
    in
        test input 1 Right 0 0 grid


type alias Grid =
    Dict ( Int, Int ) Int


test : Int -> Int -> Direction -> Int -> Int -> Grid -> Int
test target sideLength direction x y grid =
    let
        ( nextLength, nextDir, nextX, nextY ) =
            case direction of
                Up ->
                    if y == sideLength // 2 then
                        ( sideLength, Left, (x - 1), y )
                    else
                        ( sideLength, Up, x, (y + 1) )

                Left ->
                    if x == (negate sideLength) // 2 then
                        ( sideLength, Down, x, (y - 1) )
                    else
                        ( sideLength, Left, (x - 1), y )

                Down ->
                    if y == (negate sideLength) // 2 then
                        ( sideLength, Right, (x + 1), y )
                    else
                        ( sideLength, Down, x, (y - 1) )

                Right ->
                    if x == sideLength // 2 then
                        ( (sideLength + 2), Up, (x + 1), y )
                    else
                        ( sideLength, Right, (x + 1), y )

        nextGridValue =
            gridValue ( nextX, nextY ) grid
    in
        if nextGridValue > target then
            nextGridValue
        else
            test target nextLength nextDir nextX nextY <|
                Dict.insert ( nextX, nextY ) nextGridValue grid


gridValue : ( Int, Int ) -> Grid -> Int
gridValue currentPoint grid =
    neighbourCoords currentPoint
        |> List.filterMap (\point -> Dict.get point grid)
        |> List.sum


neighbourCoords : ( Int, Int ) -> List ( Int, Int )
neighbourCoords ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 3: Spiral Memory" ]
        , textarea
            [ placeholder "Enter input here"
            , onInput InputChanged
            ]
            []
        , viewSolution model.output
        , pre []
            [ text """

   17  16  15  14  13
   18   5   4   3  12
   19   6   1   2  11
   20   7   8   9  10
   21  22  23---> ...

"""
            ]
        ]


viewSolution : Result String Solution -> Html msg
viewSolution solution =
    case solution of
        Ok { part1, part2 } ->
            div []
                [ text "Solution to part:"
                , ol []
                    [ li [] [ text (toString part1) ]
                    , li [] [ text (toString part2) ]
                    ]
                ]

        Err errMsg ->
            [ errMsg ]
                |> List.map (\s -> li [] [ text s ])
                |> ul [ style [ ( "color", "red" ) ] ]
