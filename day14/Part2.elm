module Part2 exposing (..)

import Part1 exposing (generateRowKeys)
import KnotHash
import Set exposing (Set)
import Bitwise


type alias Address =
    ( Int, Int )


type alias Grid =
    Set Address


key : String
key =
    Part1.key


keyExample : String
keyExample =
    Part1.keyExample


bitWeights : List ( Int, Int )
bitWeights =
    List.range 0 7
        |> List.map (\x -> ( x, 2 ^ (7 - x) ))


insertByte : Address -> Int -> Grid -> Grid
insertByte ( row, byteCol ) byte grid =
    List.foldl
        (\( bit, weight ) accGrid ->
            if Bitwise.and weight byte > 0 then
                Set.insert
                    ( row, (byteCol * 8 + bit) )
                    accGrid
            else
                accGrid
        )
        grid
        bitWeights


insertRow : Int -> Grid -> List Int -> Grid
insertRow row grid bytes =
    let
        listAddrBytes =
            List.indexedMap
                (\col byte ->
                    ( row, col, byte )
                )
                bytes
    in
        List.foldl
            (\( row, col, byte ) accGrid ->
                insertByte ( row, col ) byte accGrid
            )
            grid
            listAddrBytes


buildRow : ( Int, String ) -> Grid -> Grid
buildRow ( rowIndex, rowKey ) grid =
    KnotHash.hashBytes rowKey
        |> insertRow rowIndex grid


buildGrid : String -> Grid
buildGrid key =
    let
        rowKeys =
            generateRowKeys key

        rowIndicesAndKeys =
            List.indexedMap (,) rowKeys
    in
        List.foldl buildRow Set.empty rowIndicesAndKeys


head : Set comparable -> Maybe comparable
head set =
    Set.foldl
        (\item acc ->
            case acc of
                Nothing ->
                    Just item

                Just _ ->
                    acc
        )
        Nothing
        set


neighbours : Address -> List Address
neighbours ( x, y ) =
    List.map
        (\( dx, dy ) ->
            ( x + dx, y + dy )
        )
        [ ( -1, 0 )
        , ( 0, -1 )
        , ( 0, 1 )
        , ( 1, 0 )
        ]


type alias Accumulator =
    { region : List Address
    , grid : Grid
    }


exploreRegion : Address -> Grid -> Grid
exploreRegion addr grid =
    if Set.member addr grid then
        let
            foldInit =
                Set.remove addr grid
        in
            List.foldl
                exploreRegion
                foldInit
                (neighbours addr)
    else
        grid


countRegions : Int -> Grid -> Int
countRegions regionCount grid =
    case head grid of
        Nothing ->
            regionCount

        Just addr ->
            let
                newGrid =
                    exploreRegion addr grid
            in
                countRegions (regionCount + 1) newGrid


answer : Int
answer =
    buildGrid key
        |> countRegions 0
