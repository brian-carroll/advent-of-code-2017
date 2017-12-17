module Part2 exposing (..)

import Input exposing (..)
import Dict exposing (Dict)


{-

   PART 2

   Brute force simulation is soooo sloooow
   Need to get mathematical!

   range = 2, period = 2 = (2-1)*2
   0 [S] [ ]
   1 [ ] [S]
   2 [S] [ ]

   range = 3, period = 4 = (3-1)*2
   00 [S] [ ] [ ]
   01 [ ] [S] [ ]
   02 [ ] [ ] [S]
   03 [ ] [S] [ ]
   04 [S] [ ] [ ]

   range = 6, period = 10 = (5-1)*2
   00 [S] [ ] [ ] [ ] [ ] [ ]
   01 [ ] [S] [ ] [ ] [ ] [ ]
   02 [ ] [ ] [S] [ ] [ ] [ ]
   03 [ ] [ ] [ ] [S] [ ] [ ]
   04 [ ] [ ] [ ] [ ] [S] [ ]
   05 [ ] [ ] [ ] [ ] [ ] [S]
   06 [ ] [ ] [ ] [ ] [S] [ ]
   07 [ ] [ ] [ ] [S] [ ] [ ]
   08 [ ] [ ] [S] [ ] [ ] [ ]
   09 [ ] [S] [ ] [ ] [ ] [ ]
   10 [S] [ ] [ ] [ ] [ ] [ ]

-}


calcPeriods : List ( Int, Int ) -> Dict Int Int
calcPeriods input =
    List.foldl
        (\( depth, range ) ->
            Dict.insert depth ((range - 1) * 2)
        )
        Dict.empty
        input


depthIsSafe : Int -> Int -> Int -> Bool
depthIsSafe delay depth period =
    (delay + depth) % period /= 0


delayIsSafe : Int -> Dict Int Int -> Bool
delayIsSafe delay periods =
    Dict.foldl
        (\depth period acc ->
            acc && depthIsSafe delay depth period
        )
        True
        periods


findSafeDelay : List ( Int, Int ) -> Int
findSafeDelay input =
    calcPeriods input
        |> findSafeDelayHelp 0


findSafeDelayHelp : Int -> Dict Int Int -> Int
findSafeDelayHelp delay periods =
    if delayIsSafe delay periods then
        delay
    else
        findSafeDelayHelp (delay + 1) periods


answerExample : Int
answerExample =
    findSafeDelay example


answer : Int
answer =
    findSafeDelay input
