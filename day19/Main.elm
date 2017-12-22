module Main exposing (..)

import Dict exposing (Dict)
import Char


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Point =
    ( Int, Int )


type alias Grid =
    Dict Point Char


type alias State =
    { grid : Grid
    , point : Point
    , direction : Direction
    , reverseLetters : List Char
    , reachedEnd : Bool
    , count : Int
    }


init : String -> State
init str =
    let
        ( grid, _, _, start ) =
            String.foldl
                (\c ( d, x, y, start ) ->
                    case c of
                        '\n' ->
                            ( d
                            , 0
                            , y + 1
                            , start
                            )

                        ' ' ->
                            ( d
                            , x + 1
                            , y
                            , start
                            )

                        _ ->
                            ( Dict.insert ( x, y ) c d
                            , x + 1
                            , y
                            , if c == '|' && start == ( 0, 0 ) then
                                ( x, y )
                              else
                                start
                            )
                )
                ( Dict.empty, 0, 0, ( 0, 0 ) )
                str
    in
        { grid = grid
        , point = start
        , direction = Down
        , reverseLetters = []
        , reachedEnd = False
        , count = 0
        }


go : Direction -> Point -> Point
go dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


keepGoing : State -> State
keepGoing state =
    { state
        | point =
            go state.direction state.point
        , count = state.count + 1
    }


turnCorner : State -> State
turnCorner state =
    let
        ( dir1, dir2 ) =
            case state.direction of
                Up ->
                    ( Left, Right )

                Down ->
                    ( Left, Right )

                Left ->
                    ( Up, Down )

                Right ->
                    ( Up, Down )

        ( point1, point2 ) =
            ( go dir1 state.point
            , go dir2 state.point
            )
    in
        case ( Dict.member point1 state.grid, Dict.member point2 state.grid ) of
            ( True, False ) ->
                { state
                    | point = point1
                    , direction = dir1
                    , count = state.count + 1
                }

            ( False, True ) ->
                { state
                    | point = point2
                    , direction = dir2
                    , count = state.count + 1
                }

            _ ->
                Debug.crash ("Can't turn corner at " ++ toString state.point)


next : State -> State
next state =
    -- Debug.log "next" <|
    case Dict.get state.point state.grid of
        Just '|' ->
            keepGoing state

        Just '-' ->
            keepGoing state

        Just '+' ->
            turnCorner state

        Just letter ->
            { state | reverseLetters = letter :: state.reverseLetters }
                |> keepGoing

        Nothing ->
            { state | reachedEnd = True }


navigate : State -> ( String, Int )
navigate state =
    if state.reachedEnd then
        ( List.reverse state.reverseLetters
            |> String.fromList
        , state.count
        )
    else
        navigate (next state)


answer : String -> ( String, Int )
answer str =
    init str
        -- |> Debug.log "init"
        |> navigate
