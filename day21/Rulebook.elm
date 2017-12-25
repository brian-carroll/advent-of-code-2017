module Rulebook exposing (Rulebook, fromString, startingPattern)

import Parser exposing (..)
import Types exposing (..)
import Dict exposing (Dict)


type alias Rulebook =
    Dict String Pattern


onParser : Parser Bit
onParser =
    succeed (\_ -> On)
        |= keep (Exactly 1) ((==) '#')


offParser : Parser Bit
offParser =
    succeed (\_ -> Off)
        |= keep (Exactly 1) ((==) '.')


pixelParser : Parser Bit
pixelParser =
    oneOf
        [ onParser
        , offParser
        ]


rowToPattern : Int -> List Bit -> Pattern
rowToPattern r row =
    row
        |> List.indexedMap
            (\c bit ->
                ( ( r, c ), bit )
            )
        |> Dict.fromList


rowParser : Int -> Int -> Parser Pattern
rowParser n r =
    succeed (rowToPattern r)
        |= repeat (Exactly n) pixelParser
        |. oneOf
            [ symbol "/"
            , succeed ()
            ]


combineRows : List Pattern -> Pattern
combineRows list =
    List.foldl
        Dict.union
        Dict.empty
        list


size3Parser : Parser Pattern
size3Parser =
    inContext "size 3 pattern" <|
        succeed (\r0 r1 r2 -> combineRows [ r0, r1, r2 ])
            |= rowParser 3 0
            |= rowParser 3 1
            |= rowParser 3 2


size4Parser : Parser Pattern
size4Parser =
    inContext "size 4 pattern" <|
        succeed (\r0 r1 r2 r3 -> combineRows [ r0, r1, r2, r3 ])
            |= rowParser 4 0
            |= rowParser 4 1
            |= rowParser 4 2
            |= rowParser 4 3


stringForPatternSize : Int -> Parser String
stringForPatternSize size =
    let
        n =
            (size ^ 2) + (size - 1)
    in
        keep (Exactly n) (\c -> c == '.' || c == '#' || c == '/')


rule2to3Parser : Parser ( String, Pattern )
rule2to3Parser =
    inContext "2-to-3 rule" <|
        delayedCommitMap (,)
            (succeed identity
                |= stringForPatternSize 2
                |. symbol " => "
            )
            (succeed identity
                |= size3Parser
                |. symbol "\n"
            )


rule3to4Parser : Parser ( String, Pattern )
rule3to4Parser =
    inContext "3-to-4 rule" <|
        delayedCommitMap (,)
            (succeed identity
                |= stringForPatternSize 3
                |. symbol " => "
            )
            (succeed identity
                |= size4Parser
                |. oneOf
                    [ symbol "\n"
                    , end
                    ]
            )


rulebookParser : Parser Rulebook
rulebookParser =
    Parser.map Dict.fromList <|
        repeat oneOrMore <|
            oneOf
                [ rule3to4Parser
                , rule2to3Parser
                ]


fromString : String -> Result Error Rulebook
fromString str =
    run rulebookParser str


startingPattern : Pattern
startingPattern =
    run size3Parser ".#./..#/###"
        |> Result.withDefault Dict.empty
