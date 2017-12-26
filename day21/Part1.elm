module Part1 exposing (..)

import Dict exposing (Dict)
import Types exposing (..)
import Rulebook exposing (Rulebook, startingPattern, fromString)
import Input


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
            Dict.insert ( r, side - 1 - c ) val acc
        )
        Dict.empty
        pattern


flipRows : Int -> Pattern -> Pattern
flipRows side pattern =
    Dict.foldl
        (\( r, c ) val acc ->
            Dict.insert ( side - 1 - r, c ) val acc
        )
        Dict.empty
        pattern


rot90 : Int -> Pattern -> Pattern
rot90 side pattern =
    Dict.foldl
        (\( r, c ) val acc ->
            Dict.insert ( side - 1 - c, r ) val acc
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


split : Int -> Int -> Pattern -> Dict Point Pattern
split side chunkSize pattern =
    let
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
                        breakOffSmallPattern chunkSize pattern offset
                in
                    Dict.insert offset smallPattern acc
            )
            Dict.empty
            offsets


tryModifiers : Pattern -> Rulebook -> Maybe Pattern -> List (Pattern -> Pattern) -> Pattern
tryModifiers pattern rulebook output modifiers =
    case output of
        Just out ->
            out

        Nothing ->
            case modifiers of
                [] ->
                    Debug.crash "no modifier worked"

                current :: rest ->
                    let
                        hashKey =
                            current pattern
                                |> patternToString

                        nextOutput =
                            Dict.get hashKey rulebook
                    in
                        tryModifiers pattern rulebook nextOutput rest


enhanceSinglePattern : Int -> Rulebook -> Pattern -> ( Pattern, Rulebook )
enhanceSinglePattern side rulebook pattern =
    let
        hash =
            patternToString pattern
    in
        case Dict.get hash rulebook of
            Just bigPattern ->
                ( bigPattern, rulebook )

            Nothing ->
                let
                    bigPattern =
                        tryModifiers pattern rulebook Nothing <|
                            [ identity
                            , flipRows side
                            , flipCols side
                            , rot90 side
                            , rot90 side >> rot90 side
                            , rot90 side >> rot90 side >> rot90 side
                            , flipRows side >> rot90 side
                            , flipRows side >> rot90 side >> rot90 side
                            , flipRows side >> rot90 side >> rot90 side >> rot90 side
                            , flipCols side >> rot90 side
                            , flipCols side >> rot90 side >> rot90 side
                            , flipCols side >> rot90 side >> rot90 side >> rot90 side
                            ]
                in
                    ( bigPattern
                    , Dict.insert hash bigPattern rulebook
                    )


enhance : Int -> Rulebook -> Dict Point Pattern -> ( Dict Point Pattern, Rulebook )
enhance chunkSize rulebook splitPatterns =
    Dict.foldl
        (\point pattern ( accSplitPatterns, accRulebook ) ->
            let
                ( bigPattern, updatedRulebook ) =
                    enhanceSinglePattern chunkSize rulebook pattern
            in
                ( Dict.insert point bigPattern accSplitPatterns
                , updatedRulebook
                )
        )
        ( Dict.empty, rulebook )
        splitPatterns


join : Int -> Dict Point Pattern -> Pattern
join chunkSize splitPatterns =
    Dict.foldl
        (\( rChunk, cChunk ) subPattern outerAcc ->
            Dict.foldl
                (\( r, c ) bit innerAcc ->
                    Dict.insert
                        ( r + (rChunk * chunkSize)
                        , c + (cChunk * chunkSize)
                        )
                        bit
                        innerAcc
                )
                outerAcc
                subPattern
        )
        Dict.empty
        splitPatterns


singleIteration : Rulebook -> Pattern -> ( Pattern, Rulebook )
singleIteration rulebook pattern =
    let
        side =
            Dict.size pattern
                |> toFloat
                |> sqrt
                |> round

        ( smallChunk, bigChunk ) =
            if side % 2 == 0 then
                ( 2, 3 )
            else
                ( 3, 4 )

        ( enhanced, updatedRulebook ) =
            pattern
                |> split side smallChunk
                |> enhance smallChunk rulebook
    in
        ( join bigChunk enhanced
        , updatedRulebook
        )


loop : Rulebook -> Int -> Pattern -> Pattern
loop rulebook n pattern =
    if n > 0 then
        let
            ( nextPattern, nextRulebook ) =
                singleIteration rulebook pattern

            _ =
                ( Debug.log "Iterations left" (n - 1)
                , Debug.log "Bits ON" (countOnBits nextPattern)
                )
        in
            loop nextRulebook (n - 1) nextPattern
    else
        pattern


exampleTwoIterations : String
exampleTwoIterations =
    "##.##./#..#../....../##.##./#..#../......"


testTwoIterations : () -> Bool
testTwoIterations () =
    let
        rulebook =
            Rulebook.fromString Input.example
                |> Result.withDefault Dict.empty

        actual =
            loop rulebook 2 startingPattern
    in
        patternToString actual == exampleTwoIterations


countOnBits : Pattern -> Int
countOnBits pattern =
    Dict.foldl
        (\_ bit acc ->
            case bit of
                On ->
                    acc + 1

                Off ->
                    acc
        )
        0
        pattern


answer : String -> Int -> Int
answer input n =
    let
        rulebook =
            Rulebook.fromString Input.input
                |> Result.withDefault Dict.empty
    in
        loop rulebook n startingPattern
            |> countOnBits
