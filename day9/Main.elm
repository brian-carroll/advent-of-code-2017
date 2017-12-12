module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)


type Msg
    = ParseInput String


type alias Group =
    List Element


type Element
    = SubGroup Group
    | Garbage


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
        , Parser.map
            (\_ -> Garbage)
            garbageParser
        ]


garbageParser : Parser ()
garbageParser =
    symbol "whatevs"


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
