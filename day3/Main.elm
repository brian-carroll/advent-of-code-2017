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
                                    , part2 = 0 --solvePart2 input
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


validateRow : List String -> Result (List String) (List Int)
validateRow row =
    row
        |> List.map (String.toInt)
        |> List.foldr validateChar (Ok [])


validateChar : Result String Int -> Result (List String) (List Int) -> Result (List String) (List Int)
validateChar intResult acc =
    case ( intResult, acc ) of
        ( Err charMsg, Ok _ ) ->
            Err [ charMsg ]

        ( Err charMsg, Err strMsg ) ->
            Err (charMsg :: strMsg)

        ( Ok int, Err msg ) ->
            Err msg

        ( Ok int, Ok ints ) ->
            Ok (int :: ints)


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



{-
   147  142  133  122   59
   304    5    4    2   57
   330   10    1    1   54
   351   11   23   25   26
   362  747  806--->   ...

-}


solvePart2 : Int -> Int
solvePart2 input =
    let
        grid =
            Dict.insert ( 0, 0 ) 1 Dict.empty
    in
        test input 1 3 Up 0 0 grid


type alias Grid =
    Dict ( Int, Int ) Int


test : Int -> Int -> Int -> Direction -> Int -> Int -> Grid -> Int
test target current sideLength direction x y grid =
    if Dict.size grid > 100 then
        current
    else if current > target then
        current
    else
        let
            distanceToTravel =
                Basics.min (target - current) (sideLength - 1)

            nextNumber =
                current + distanceToTravel

            -- _ =
            --     Debug.log "grid" grid
            -- { target = target
            -- , current = current
            -- , sideLength = sideLength
            -- , direction = direction
            -- , x = x
            -- , y = y
            -- , distanceToTravel = distanceToTravel
            -- , nextNumber = nextNumber
            -- }
        in
            case direction of
                Up ->
                    let
                        nextX =
                            x + 1

                        nextY =
                            y - 1 + distanceToTravel

                        points =
                            Debug.log (toString ( y, nextY )) <|
                                List.map (\yi -> ( nextX, yi )) (List.range y nextY)

                        newGrid =
                            List.foldl insertIntoGrid grid points
                    in
                        test target nextNumber sideLength Left nextX nextY newGrid

                Left ->
                    let
                        nextX =
                            x - distanceToTravel

                        points =
                            List.range nextX x
                                |> List.reverse
                                |> List.map (\xi -> ( xi, y ))

                        newGrid =
                            List.foldl insertIntoGrid grid points
                    in
                        test target nextNumber sideLength Down nextX y newGrid

                Down ->
                    let
                        nextY =
                            y - distanceToTravel

                        points =
                            List.range nextY y
                                |> List.reverse
                                |> List.map (\yi -> ( x, yi ))

                        newGrid =
                            List.foldl insertIntoGrid grid points
                    in
                        test target nextNumber sideLength Right x nextY newGrid

                Right ->
                    let
                        nextX =
                            x + distanceToTravel

                        points =
                            List.map (\xi -> ( xi, y )) (List.range x nextX)

                        newGrid =
                            List.foldl insertIntoGrid grid points
                    in
                        test target nextNumber (sideLength + 2) Up nextX y newGrid


insertIntoGrid : ( Int, Int ) -> Grid -> Grid
insertIntoGrid currentPoint grid =
    let
        value =
            neighbourCoords currentPoint
                |> List.filterMap (\point -> Dict.get point grid)
                |> List.sum
    in
        Dict.insert currentPoint value grid


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
