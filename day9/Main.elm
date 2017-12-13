module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Input exposing (stream)


type Msg
    = ParseInput String


type alias Group =
    List Element


type Element
    = SubGroup Group
    | Garbage String


type alias Model =
    { input : String
    , stream : Group
    , score : Int
    }


groupParser : Parser Group
groupParser =
    sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = ignore zeroOrMore (\c -> c == ' ')
        , item = lazy (\_ -> elementParser)
        , trailing = Forbidden
        }


elementParser : Parser Element
elementParser =
    oneOf
        [ Parser.map
            SubGroup
            (lazy (\_ -> groupParser))
        , Parser.map Garbage garbageParser
        ]


garbageParser : Parser String
garbageParser =
    inContext "garbage" <|
        succeed String.concat
            |. symbol "<"
            |= repeat zeroOrMore garbageCharParser
            |. symbol ">"


garbageCharParser : Parser String
garbageCharParser =
    oneOf
        [ cancelParser
        , keep (Exactly 1) ((/=) '>')
        ]


cancelParser : Parser String
cancelParser =
    inContext "cancel" <|
        (succeed ""
            |. keep (Exactly 1) ((==) '!')
            |. keep (Exactly 1) (\c -> True)
        )


groupExamples : List ( String, Int )
groupExamples =
    [ ( "{}", 1 )
    , ( "{{{}}}", 3 )
    , ( "{{},{}}", 3 )
    , ( "{{{},{},{{}}}}", 6 )
    , ( "{<{},{},{{}}>}", 1 )
    , ( "{<a>,<a>,<a>,<a>}", 1 )
    , ( "{{<a>},{<a>},{<a>},{<a>}}", 5 )
    , ( "{{<!>},{<!>},{<!>},{<a>}}", 2 )
    ]


scoreExamples =
    [ ( "{}", 1 )
    , ( "{{{}}}", 6 )
    , ( "{{},{}}", 5 )
    , ( "{{{},{},{{}}}}", 16 )
    , ( "{<a>,<a>,<a>,<a>}", 1 )
    , ( "{{<ab>},{<ab>},{<ab>},{<ab>}}", 9 )
    , ( "{{<!!>},{<!!>},{<!!>},{<!!>}}", 9 )
    , ( "{{<a!>},{<a!>},{<a!>},{<ab>}}", 3 )
    ]


score : Group -> Int
score group =
    groupScore 1 group


groupScore : Int -> Group -> Int
groupScore depth group =
    List.map (elementScore depth) group
        |> List.sum
        |> (+) depth


elementScore : Int -> Element -> Int
elementScore depth element =
    case element of
        SubGroup group ->
            groupScore (depth + 1) group

        Garbage _ ->
            0


countGarbage group =
    List.map countGarbageInElement group
        |> List.sum


countGarbageInElement element =
    case element of
        SubGroup group ->
            countGarbage group

        Garbage str ->
            String.length str


testScore =
    List.map
        (\( str, expected ) ->
            case run groupParser str of
                Ok group ->
                    let
                        actual =
                            score group
                    in
                        if actual == expected then
                            "Correct " ++ (toString expected)
                        else
                            "Got "
                                ++ (toString actual)
                                ++ " expected "
                                ++ (toString expected)

                Err e ->
                    toString e
        )
        scoreExamples


inputScore =
    run groupParser stream
        |> Result.map score


inputGarbageCount =
    run groupParser stream
        |> Result.map countGarbage


singleGarbageExamples : List String
singleGarbageExamples =
    [ "<>"
    , "<random characters>"
    , "<<<<>"
    , "<{!>}>"
    , "<!!>"
    , "<!!!>>"
    , "<{o\"i!a,<{i<a>"
    ]


testGarbageParser =
    List.map (run garbageParser) singleGarbageExamples


pretty : List a -> String
pretty list =
    Debug.log
        (list |> List.map toString |> String.join "\n")
        ""
