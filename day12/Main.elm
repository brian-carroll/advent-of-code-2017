module Main exposing (..)

import Input exposing (input)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Connections =
    Dict Int (List Int)


parseInput : String -> Connections
parseInput str =
    str
        |> String.split "\n"
        |> List.foldl
            parseLine
            Dict.empty


parseLine : String -> Connections -> Connections
parseLine line dict =
    case String.split " <-> " line of
        [ keyStr, valueStr ] ->
            let
                key =
                    String.toInt keyStr
                        |> Result.withDefault 0

                values =
                    String.split ", " valueStr
                        |> List.map (String.toInt >> Result.withDefault 0)
            in
                Dict.insert key values dict

        _ ->
            dict


type alias Accumulator =
    { group : Set Int
    , connections : Connections
    }


traverse : Int -> Accumulator -> Accumulator
traverse current acc =
    case Dict.get current acc.connections of
        Nothing ->
            acc

        Just childList ->
            List.foldl
                traverse
                { group = Set.insert current acc.group
                , connections = Dict.remove current acc.connections
                }
                childList


findGroupContaining : Int -> Connections -> Accumulator
findGroupContaining start connections =
    traverse start
        { connections = connections
        , group = Set.singleton 0
        }


groupSize : Int -> Connections -> Int
groupSize start connections =
    let
        finalAcc =
            findGroupContaining start connections
    in
        Set.size finalAcc.group


example : Connections
example =
    Dict.fromList
        [ ( 0, [ 2 ] )
        , ( 1, [ 1 ] )
        , ( 2, [ 0, 3, 4 ] )
        , ( 3, [ 2, 4 ] )
        , ( 4, [ 2, 3, 6 ] )
        , ( 5, [ 6 ] )
        , ( 6, [ 4, 5 ] )
        ]


answerExample : Int
answerExample =
    groupSize 0 example


inputConnections : Connections
inputConnections =
    parseInput input


answerPart1 : Int
answerPart1 =
    inputConnections
        |> groupSize 0



{-

   PART 2

-}


countGroups : Int -> Connections -> Int
countGroups groupCount connections =
    case getFirstKey connections of
        Nothing ->
            groupCount

        Just firstKey ->
            let
                acc : Accumulator
                acc =
                    findGroupContaining firstKey connections
            in
                countGroups (groupCount + 1) acc.connections


getFirstKey : Dict comparable v -> Maybe comparable
getFirstKey dict =
    Dict.keys dict
        |> List.head


answerPart2 : Int
answerPart2 =
    countGroups 0 inputConnections
