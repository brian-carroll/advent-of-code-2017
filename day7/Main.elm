module Main exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Char


type alias Prog =
    { name : String
    , weight : Maybe Int
    , children : Children
    }


type Children
    = Children (List Prog)


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')


newline : Parser ()
newline =
    ignore oneOrMore (\char -> char == '\n')


nameParser : Parser String
nameParser =
    keep oneOrMore Char.isLower


weightParser : Parser Int
weightParser =
    succeed identity
        |. symbol "("
        |= int
        |. symbol ")"


childParser : Parser Prog
childParser =
    Parser.map
        (\name ->
            { name = name
            , weight = Nothing
            , children = Children []
            }
        )
        nameParser


childrenParser : Parser Children
childrenParser =
    Parser.map Children <|
        sequence
            { start = "->"
            , separator = ","
            , end = ""
            , spaces = spaces
            , item = childParser
            , trailing = Forbidden
            }


progParser : Parser Prog
progParser =
    succeed Prog
        |= nameParser
        |. spaces
        |= Parser.map Just weightParser
        |. spaces
        |= childrenParser
