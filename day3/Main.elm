module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


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
                                    , part2 = 0
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


solvePart1 : Int -> Int
solvePart1 input =
    let
        ( x, y ) =
            move input 1 3 Up 0 0
    in
        (Basics.abs x) + (Basics.abs y)



{-
   17  16  15  14  13
   18   5   4   3  12
   19   6   1   2  11
   20   7   8   9  10
   21  22  23---> ...

-}


move : Int -> Int -> Int -> Direction -> Int -> Int -> ( Int, Int )
move target current sideLength direction x y =
    if target == current then
        ( x, y )
    else
        let
            distanceToTravel =
                Basics.min (target - current) (sideLength - 1)

            nextNumber =
                current + distanceToTravel

            _ =
                Debug.log ""
                    { target = target
                    , current = current
                    , sideLength = sideLength
                    , direction = direction
                    , x = x
                    , y = y
                    , distanceToTravel = distanceToTravel
                    , nextNumber = nextNumber
                    }
        in
            case direction of
                Up ->
                    move target nextNumber sideLength Left (x + 1) (y - 1 + distanceToTravel)

                Left ->
                    move target nextNumber sideLength Down (x - distanceToTravel) y

                Down ->
                    move target nextNumber sideLength Right x (y - distanceToTravel)

                Right ->
                    move target nextNumber (sideLength + 2) Up (x + distanceToTravel) y


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
