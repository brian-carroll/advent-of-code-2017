module Part1 exposing (..)

{-
   operations
       flip H
       flip V
       rotate 90
       rotate 180
       rotate 270
       split 4 -> 2
       count

-}

import Dict exposing (Dict)
import Types exposing (..)
import Rulebook exposing (startingPattern, fromString)


bitToString : Bit -> String
bitToString bit =
    case bit of
        On ->
            "#"

        Off ->
            "."


patternToString : Pattern -> String
patternToString pattern =
    let
        ( str, _ ) =
            Dict.foldl
                (\( r, c ) bit ( acc, previousRow ) ->
                    ( if r == previousRow then
                        acc ++ bitToString bit
                      else
                        acc ++ "/" ++ bitToString bit
                    , r
                    )
                )
                ( "", 0 )
                pattern
    in
        str


flipCols : Int -> Pattern -> Pattern
flipCols side pattern =
    Dict.foldl
        (\( r, c ) val acc ->
            Dict.insert ( r, side - c ) val acc
        )
        Dict.empty
        pattern


flipRows : Int -> Pattern -> Pattern
flipRows side pattern =
    Dict.foldl
        (\( r, c ) val acc ->
            Dict.insert ( side - r, c ) val acc
        )
        Dict.empty
        pattern


rot90 : Int -> Pattern -> Pattern
rot90 side pattern =
    Dict.foldl
        (\( r, c ) val acc ->
            Dict.insert ( side - c, r ) val acc
        )
        Dict.empty
        pattern


breakOffSmallPattern : Int -> Pattern -> Point -> Pattern
breakOffSmallPattern side bigPattern ( rChunk, cChunk ) =
    let
        rMin =
            rChunk * side

        cMin =
            cChunk * side

        rMax =
            rMin + side - 1

        cMax =
            cMin + side - 1
    in
        Dict.foldl
            (\( r, c ) v acc ->
                if
                    (r >= rMin)
                        && (r <= rMax)
                        && (c >= cMin)
                        && (c <= cMax)
                then
                    Dict.insert ( r - rMin, c - cMin ) v acc
                else
                    acc
            )
            Dict.empty
            bigPattern


split : Pattern -> Dict Point Pattern
split pattern =
    {-
       figure out how many regions we want
           (div by 2 or 3, whichever fits)
       generate (x,y) offsets
       fold over offsets
            generate list of points in little pattern
            fold over points
                get value from bigpattern, put in little pattern
    -}
    let
        side =
            Dict.size pattern
                |> toFloat
                |> sqrt
                |> round

        chunkSize =
            if side % 2 == 0 then
                2
            else
                3

        chunksPerSide =
            side // chunkSize

        range =
            List.range 0 (chunksPerSide - 1)

        offsets =
            List.foldl
                (\r acc ->
                    acc ++ (List.map (\c -> ( r, c )) range)
                )
                []
                range
    in
        List.foldl
            (\offset acc ->
                let
                    smallPattern =
                        breakOffSmallPattern side pattern offset
                in
                    Dict.insert offset smallPattern acc
            )
            Dict.empty
            offsets
