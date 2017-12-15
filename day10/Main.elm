module Main exposing (..)

import CircularList exposing (CircularList)


input : List Int
input =
    [ 130, 126, 1, 11, 140, 2, 255, 207, 18, 254, 246, 164, 29, 104, 0, 224 ]


size : Int
size =
    256


finalCircularList : CircularList
finalCircularList =
    List.foldl
        (\len acc ->
            CircularList.reverseSection len acc
        )
        (CircularList.create size)
        input


answer : Int
answer =
    CircularList.check finalCircularList
