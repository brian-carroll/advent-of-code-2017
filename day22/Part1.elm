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


type alias Boundaries =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }


gridBoundaries : Grid -> Boundaries
gridBoundaries grid =
    let
        points =
            Dict.keys grid

        rowIndices =
            List.map Tuple.first points

        colIndices =
            List.map Tuple.second points
    in
        { left = List.minimum colIndices |> Maybe.withDefault 0
        , right = List.maximum colIndices |> Maybe.withDefault 0
        , top = List.minimum rowIndices |> Maybe.withDefault 0
        , bottom = List.maximum rowIndices |> Maybe.withDefault 0
        }


stateToString : State -> String
stateToString state =
    let
        boundaries =
            gridBoundaries state.grid

        rowRange =
            List.range boundaries.top boundaries.bottom

        colRange =
            List.range boundaries.left boundaries.right

        allPoints =
            List.foldl
                (\r acc ->
                    acc ++ List.map (\c -> ( r, c )) colRange
                )
                []
                rowRange

        fullGrid =
            List.foldl
                (\point gridAcc ->
                    Dict.update point
                        ((Maybe.withDefault Clean) >> Just)
                        gridAcc
                )
                state.grid
                allPoints

        ( _, gridStr ) =
            Dict.foldl
                (\( r, c ) node ( previousRow, accStr ) ->
                    ( r
                    , accStr
                        ++ (if r /= previousRow then
                                "\n"
                            else
                                ""
                           )
                        ++ if ( r, c ) == state.location then
                            case node of
                                Infected ->
                                    "[#]"

                                Clean ->
                                    "[.]"
                           else
                            case node of
                                Infected ->
                                    " # "

                                Clean ->
                                    " . "
                    )
                )
                ( 0, "" )
                fullGrid
    in
        gridStr
            ++ "\n\nFacing "
            ++ (toString state.direction)
            ++ "\n"


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


pretty : State -> ()
pretty state =
    let
        displayStr =
            stateToString state
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


type Direction
    = Up
    | Down
    | Left
    | Right


type alias State =
    { grid : Grid
    , direction : Direction
    , location : Point
    , infections : Int
    }


initState : String -> State
initState input =
    let
        grid =
            run gridParser input
                |> Result.withDefault Dict.empty

        { left, right, top, bottom } =
            gridBoundaries grid

        midPoint =
            ( (top + bottom) // 2
            , (left + right) // 2
            )
    in
        { grid = grid
        , direction = Up
        , location = midPoint
        , infections = 0
        }


loop : Int -> State -> Int
loop iterations state =
    if iterations <= 0 then
        state.infections
    else
        loop (iterations - 1) (burst state)


{-| Burst of activity

If the current node is infected, it turns to its right. Otherwise, it turns to its left. (Turning is done in-place; the current node does not change.)
If the current node is clean, it becomes infected. Otherwise, it becomes cleaned. (This is done after the node is considered for the purposes of changing direction.)
The virus carrier moves forward one node in the direction it is facing.

    Operations:
        Get current infection status
        Turn on the spot
        Flip cleanliness
        Move forward

-}
burst : State -> State
burst state =
    let
        ( newDir, newNodeStatus, newInfections ) =
            case Dict.get state.location state.grid of
                Just Infected ->
                    ( turnRight state.direction
                    , Clean
                    , state.infections
                    )

                _ ->
                    ( turnLeft state.direction
                    , Infected
                    , state.infections + 1
                    )
    in
        { grid = Dict.insert state.location newNodeStatus state.grid
        , direction = newDir
        , location = goForward newDir state.location
        , infections = newInfections
        }


turnLeft : Direction -> Direction
turnLeft dir =
    case dir of
        Up ->
            Left

        Down ->
            Right

        Left ->
            Down

        Right ->
            Up


turnRight : Direction -> Direction
turnRight dir =
    case dir of
        Up ->
            Right

        Down ->
            Left

        Left ->
            Up

        Right ->
            Down


goForward : Direction -> Point -> Point
goForward dir ( r, c ) =
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


answer : String -> Int -> Int
answer input iterations =
    initState input
        |> loop iterations
