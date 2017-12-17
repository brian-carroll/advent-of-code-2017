module KnotHash exposing (hash, hashBytes)

import CircularList exposing (CircularList(..))
import Bitwise
import Hex
import Ascii
import Array.Hamt as Array exposing (Array)


suffix : List Int
suffix =
    [ 17, 31, 73, 47, 23 ]


hashBytes : String -> List Int
hashBytes input =
    let
        charCodes =
            (Ascii.fromString input) ++ suffix

        sparseHashClist =
            iterateHash 64 charCodes (CircularList.create 256)

        sparse =
            case sparseHashClist of
                CircularList c ->
                    c.values
    in
        condense sparse


hash : String -> String
hash input =
    hashBytes input
        |> List.map zeroPadHexToString
        |> String.concat


zeroPadHexToString : Int -> String
zeroPadHexToString n =
    Hex.toString n
        |> (++) "0"
        |> String.right 2


iterateHash : Int -> List Int -> CircularList -> CircularList
iterateHash iters input circularList =
    if iters <= 0 then
        circularList
    else
        let
            newCircularList =
                List.foldl
                    CircularList.reverseSection
                    circularList
                    input
        in
            iterateHash (iters - 1) input newCircularList


condense : Array Int -> List Int
condense sparse =
    List.range 0 15
        |> List.map
            (\idx ->
                idx
                    |> ((*) 16)
                    |> (\i -> Array.slice i (i + 16) sparse)
                    |> Array.foldl Bitwise.xor 0
            )
