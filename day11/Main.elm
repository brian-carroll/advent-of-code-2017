module Main exposing (..)

import Input exposing (input)


-- Directions


type Direction
    = N
    | NW
    | NE
    | S
    | SW
    | SE


type alias Hex =
    ( Float, Float )


origin : Hex
origin =
    ( 0, 0 )


parseDir : String -> Maybe Direction
parseDir dir =
    case dir of
        "n" ->
            Just N

        "ne" ->
            Just NE

        "nw" ->
            Just NW

        "s" ->
            Just S

        "se" ->
            Just SE

        "sw" ->
            Just SW

        _ ->
            Nothing


directionsFromString : String -> List Direction
directionsFromString str =
    String.split "," str
        |> List.filterMap parseDir


{-| X distance travelled when going NE, NW, SE or SW
-}
xDiag : Float
xDiag =
    sin (pi / 3)


{-| Y distance travelled when going NE, NW, SE or SW
-}
yDiag : Float
yDiag =
    0.5


step : Direction -> Hex -> Hex
step dir ( x, y ) =
    case dir of
        N ->
            ( x
            , y + 1
            )

        NW ->
            ( x - xDiag
            , y + yDiag
            )

        NE ->
            ( x + xDiag
            , y + yDiag
            )

        S ->
            ( x
            , y - 1
            )

        SW ->
            ( x - xDiag
            , y - yDiag
            )

        SE ->
            ( x + xDiag
            , y - yDiag
            )


followDirections : List Direction -> Hex
followDirections dirList =
    List.foldl step origin dirList


distSq : Hex -> Float
distSq ( x, y ) =
    (x ^ 2) + (y ^ 2)


distance : Hex -> Float
distance hex =
    distSq hex
        |> sqrt


allDirections : List Direction
allDirections =
    [ N, NE, NW, S, SE, SW ]


hexInBounds : Hex -> Hex -> Bool
hexInBounds ( xBound, yBound ) ( xTest, yTest ) =
    coordInBounds xBound xTest
        && coordInBounds yBound yTest


coordInBounds : Float -> Float -> Bool
coordInBounds bound test =
    if bound > 0 then
        test - bound < 0.9
    else
        test - bound > -0.9


isOrigin : Hex -> Bool
isOrigin ( x, y ) =
    (x < 0.1) && (y < 0.1)



{-

   SHORTEST PATH

-}


findLeastSteps : Hex -> Float -> Int -> Int
findLeastSteps current currentDistSq steps =
    if isOrigin current then
        steps
    else
        let
            ( closest, closestDistSq ) =
                allDirections
                    |> List.map
                        (\dir ->
                            let
                                neighbour =
                                    step dir current
                            in
                                ( neighbour, distSq neighbour )
                        )
                    |> List.foldl
                        (\( hex, d2 ) ( accHex, accDistSq ) ->
                            if d2 < accDistSq then
                                ( hex, d2 )
                            else
                                ( accHex, accDistSq )
                        )
                        ( current, currentDistSq )
        in
            findLeastSteps closest closestDistSq (steps + 1)


answerPart1 : Int
answerPart1 =
    directionsFromString input
        |> followDirections
        |> (\hex ->
                findLeastSteps hex (distSq hex) 0
           )



{-

   PART 2

-}


furthestAway : List Direction -> Int
furthestAway dirList =
    List.foldl
        (\dir ( current, maxSteps ) ->
            let
                next =
                    step dir current

                nextSteps =
                    findLeastSteps next (distSq next) 0
            in
                if nextSteps > maxSteps then
                    ( next, nextSteps )
                else
                    ( next, maxSteps )
        )
        ( origin, 0 )
        dirList
        |> Tuple.second


answerPart2 : Int
answerPart2 =
    directionsFromString input
        |> furthestAway


{-| Directions examples
ne,ne,ne is 3 steps away.
ne,ne,sw,sw is 0 steps away (back where you started).
ne,ne,s,s is 2 steps away (se,se).
se,sw,se,sw,sw is 3 steps away (s,s,sw).
-}
dirExamples =
    [ ( "ne,ne,ne", 3 )
    , ( "ne,ne,sw,sw", 0 )
    , ( "ne,ne,s,s", 2 )
    , ( "se,se", 2 )
    , ( "se,sw,se,sw,sw", 3 )
    , ( "s,s,sw", 3 )
    ]


testFollowDirections =
    List.map
        (\( str, fewestSteps ) ->
            let
                finalHex =
                    directionsFromString str
                        |> followDirections
            in
                ( str, fewestSteps, finalHex, distance finalHex )
        )
        dirExamples


pretty : List a -> String
pretty xs =
    List.map toString xs
        |> String.join "\n"
        |> flip Debug.log ""
