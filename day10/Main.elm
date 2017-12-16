module Main exposing (..)

import CircularList exposing (CircularList)


input : List Int
input =
    [ 130, 126, 1, 11, 140, 2, 255, 207, 18, 254, 246, 164, 29, 104, 0, 224 ]


size : Int
size =
    256


hashRound : List Int -> CircularList
hashRound sectionLengths =
    List.foldl
        CircularList.reverseSection
        (CircularList.create size)
        sectionLengths


finalCircularList : CircularList
finalCircularList =
    hashRound input


answerPart1 : Int
answerPart1 =
    CircularList.check finalCircularList


inputPart2 : String
inputPart2 =
    input
        |> List.map toString
        |> String.join ","


answerPart2 : String
answerPart2 =
    CircularList.knotHash inputPart2
