module Part1 exposing (..)

import Input exposing (..)
import Parser exposing (..)


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


manhattan : Triplet -> Int
manhattan ( x, y, z ) =
    abs x + abs y + abs z


sortByAccel : List ( Int, Particle ) -> List ( Int, Particle )
sortByAccel list =
    List.sortWith
        (\( _, p1 ) ( _, p2 ) ->
            compare (manhattan p1.a) (manhattan p2.a)
        )
        list


{-| Tricky question!
The one with the smallest acceleration will be closest to the origin in the long term
Nothing else really matters.
-}
answer : String -> Result Error (Maybe Int)
answer input =
    run inputParser input
        |> Result.map
            (\particles ->
                particles
                    |> List.indexedMap (,)
                    |> sortByAccel
                    |> List.head
                    |> Maybe.map Tuple.first
            )
