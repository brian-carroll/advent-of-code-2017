module Part1 exposing (..)

import Input exposing (..)
import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (sequence, Trailing(..))


type alias Triplet =
    ( Int, Int, Int )


type alias Particle =
    { p : Triplet
    , v : Triplet
    , a : Triplet
    }


intParser : Parser Int
intParser =
    oneOf
        [ int
        , succeed negate
            |. symbol "-"
            |= int
        ]


tripletParser : Parser Triplet
tripletParser =
    succeed (,,)
        |. symbol "<"
        |= intParser
        |. symbol ","
        |= intParser
        |. symbol ","
        |= intParser
        |. symbol ">"


particleParser : Parser Particle
particleParser =
    succeed Particle
        |. symbol "p="
        |= tripletParser
        |. symbol ", v="
        |= tripletParser
        |. symbol ", a="
        |= tripletParser
        |. oneOf
            [ ignore (Exactly 1) ((==) '\n')
            , end
            ]


inputParser : Parser (List Particle)
inputParser =
    repeat oneOrMore particleParser
