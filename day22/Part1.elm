module Part1 exposing (..)

import Parser exposing (..)
import Dict exposing (Dict)


{-

   TYPES

-}


type alias Point =
    ( Int, Int )


type Node
    = Infected
    | Clean


type alias Grid =
    Dict Point Node


type Direction
    = Up
    | Down
    | Left
    | Right



{-

   PARSING

-}


nodeParser : Parser Node
nodeParser =
    oneOf
        [ keep (Exactly 1) ((==) '.') |> Parser.map (\_ -> Clean)
        , keep (Exactly 1) ((==) '#') |> Parser.map (\_ -> Infected)
        ]


rowParser : Parser (List Node)
rowParser =
    repeat oneOrMore nodeParser
        |. oneOf
            [ symbol "\n"
            , end
            ]


gridFromList : List (List Node) -> Grid
gridFromList nodeListList =
    nodeListList
        |> List.indexedMap (,)
        |> List.foldl
            (\( rIndex, row ) gridAcc ->
                row
                    |> List.indexedMap (,)
                    |> List.foldl
                        (\( cIndex, node ) rowAcc ->
                            Dict.insert ( rIndex, cIndex ) node rowAcc
                        )
                        gridAcc
            )
            Dict.empty


gridParser : Parser Grid
gridParser =
    repeat oneOrMore rowParser
        |> Parser.map gridFromList



{-

   VISUALISATION

-}


gridToString : Grid -> String
gridToString grid =
    let
        ( _, str ) =
            Dict.foldl
                (\( r, c ) node ( previousRow, accStr ) ->
                    ( r
                    , accStr
                        ++ (if r /= previousRow then
                                "\n"
                            else
                                ""
                           )
                        ++ case node of
                            Infected ->
                                "#"

                            Clean ->
                                "."
                    )
                )
                ( 0, "" )
                grid
    in
        str


pretty : Grid -> ()
pretty grid =
    let
        displayStr =
            gridToString grid
    in
        Debug.log ("\n" ++ displayStr ++ "\n\n") ()


testParser : String -> Result Error Bool
testParser input =
    input
        |> run gridParser
        |> Result.map gridToString
        |> Result.map ((==) input)



{-

   GAME MECHANICS

-}


go : Direction -> Point -> Point
go dir ( r, c ) =
    case dir of
        Up ->
            ( r - 1, c )

        Down ->
            ( r + 1, c )

        Left ->
            ( r, c - 1 )

        Right ->
            ( r, c + 1 )



{-

   ANSWER

-}
