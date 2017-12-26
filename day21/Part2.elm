module Part2 exposing (..)

import Input exposing (..)
import Rulebook exposing (..)
import Types exposing (..)
import Dict exposing (Dict)
import Part1 exposing (countOnBits)


{-
   OK... time for massive radical performance improvements
   Number of bits is increasing exponentially, 18 iterations is silly.

   I bet I can ignore everything except the number of ON bits
   No need to rotate and flip at all

   Test this on Part1 answer and see if it works
   Ignore all details about shapes and just stick with the number of 1's

   Also I can flatten two iterations into one
   Go straight from 2x2 to 4x4, skipping a step

-}


fullRulebook : Rulebook
fullRulebook =
    Rulebook.fromString input
        |> Result.withDefault Dict.empty


simplifiedRulebook =
    simplify fullRulebook


simplify : Dict String Pattern -> Dict ( Int, Int, String ) Int
simplify rulebook =
    Dict.foldl
        (\str pattern acc ->
            let
                keyLen =
                    String.length str

                keyBits =
                    str
                        |> String.filter ((==) '#')
                        |> String.length

                midPoint =
                    (keyLen // 2)

                middleChar =
                    String.slice midPoint (midPoint + 1) str

                valueBits =
                    countOnBits pattern
            in
                case Dict.get ( keyLen, keyBits, middleChar ) acc of
                    Just prev ->
                        let
                            _ =
                                -- Debug.log "Already in dict"
                                { key = ( keyLen, keyBits, middleChar )
                                , valueBits = valueBits
                                , prev = prev
                                , match = prev == valueBits
                                }
                        in
                            acc

                    Nothing ->
                        let
                            _ =
                                -- Debug.log "NEW"
                                { keyLen = ( keyLen, keyBits, middleChar )
                                , valueBits = valueBits
                                }
                        in
                            Dict.insert ( keyLen, keyBits, middleChar ) valueBits acc
        )
        Dict.empty
        rulebook
