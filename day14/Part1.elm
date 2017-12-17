module Part1 exposing (..)

import KnotHash
import Bitwise


keyExample : String
keyExample =
    "flqrgnkx"


key : String
key =
    "hxtvlmkl"


generateRowKeys : String -> List String
generateRowKeys key =
    List.range 0 127
        |> List.map
            (\row ->
                key ++ "-" ++ (toString row)
            )


bitWeights : List Int
bitWeights =
    List.range 0 7
        |> List.map (\x -> 2 ^ x)


bitCount : Int -> Int
bitCount byte =
    List.foldl
        (\bitWeight count ->
            if Bitwise.and bitWeight byte > 0 then
                count + 1
            else
                count
        )
        0
        bitWeights


answer : String -> Int
answer key =
    generateRowKeys key
        |> List.concatMap KnotHash.hashBytes
        |> List.foldl
            (\byte count ->
                count + bitCount byte
            )
            0
