module Part2 exposing (..)

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
    | Weakened
    | Flagged


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

                                Weakened ->
                                    "[W]"

                                Flagged ->
                                    "[F]"
                           else
                            case node of
                                Infected ->
                                    " # "

                                Clean ->
                                    " . "

                                Weakened ->
                                    " W "

                                Flagged ->
                                    " F "
                    )
                )
                ( 0, "" )
                fullGrid
    in
        gridStr
            ++ "\n\nFacing "
            ++ (toString state.direction)
            ++ "\n"


pretty : State -> ()
pretty state =
    let
        displayStr =
            stateToString state
    in
        Debug.log ("\n" ++ displayStr ++ "\n\n") ()



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


loop : Int -> State -> State
loop iterations state =
    if iterations <= 0 then
        state
    else
        let
            _ =
                if iterations % 1000 == 0 then
                    Debug.log "Iterations left" iterations
                else
                    iterations
        in
            loop (iterations - 1) (burst state)


{-| Burst of activity

Clean nodes become weakened.
Weakened nodes become infected.
Infected nodes become flagged.
Flagged nodes become clean.

If it is clean, it turns left.
If it is weakened, it does not turn, and will continue moving in the same direction.
If it is infected, it turns right.
If it is flagged, it reverses direction, and will go back the way it came.

-}
burst : State -> State
burst state =
    let
        ( newDir, newNodeStatus, newInfections ) =
            case Dict.get state.location state.grid of
                Just Infected ->
                    ( turnRight state.direction
                    , Flagged
                    , state.infections
                    )

                Just Weakened ->
                    ( state.direction
                    , Infected
                    , state.infections + 1
                    )

                Just Flagged ->
                    ( turnAround state.direction
                    , Clean
                    , state.infections
                    )

                _ ->
                    ( turnLeft state.direction
                    , Weakened
                    , state.infections
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


turnAround : Direction -> Direction
turnAround dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


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
        |> .infections
        |> Debug.log "answer"
