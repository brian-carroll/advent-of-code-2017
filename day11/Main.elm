module Main exposing (..)

import Dict exposing (Dict)


-- Directions


type Direction
    = N
    | NW
    | NE
    | S
    | SW
    | SE


type alias Hex =
    ( Float, Float )


origin : Hex
origin =
    ( 0, 0 )


parseDir : String -> Maybe Direction
parseDir dir =
    case dir of
        "n" ->
            Just N

        "ne" ->
            Just NE

        "nw" ->
            Just NW

        "s" ->
            Just S

        "se" ->
            Just SE

        "sw" ->
            Just SW

        _ ->
            Nothing


directionsFromString : String -> List Direction
directionsFromString str =
    String.split "," str
        |> List.filterMap parseDir


{-| X distance travelled when going NE, NW, SE or SW
-}
xDiag : Float
xDiag =
    sin (pi / 3)


{-| Y distance travelled when going NE, NW, SE or SW
-}
yDiag : Float
yDiag =
    0.5


step : Direction -> Hex -> Hex
step dir ( x, y ) =
    case dir of
        N ->
            ( x
            , y + 1
            )

        NW ->
            ( x - xDiag
            , y + yDiag
            )

        NE ->
            ( x + xDiag
            , y + yDiag
            )

        S ->
            ( x
            , y - 1
            )

        SW ->
            ( x - xDiag
            , y - yDiag
            )

        SE ->
            ( x + xDiag
            , y - yDiag
            )


followDirections : List Direction -> Hex
followDirections dirList =
    List.foldl step origin dirList


distSq : Hex -> Float
distSq ( x, y ) =
    (x ^ 2) + (y ^ 2)


distance : Hex -> Float
distance hex =
    distSq hex
        |> sqrt



{-
   Dijkstra's algorithm
   - Start with
       - initial set of nodes, all unvisited
           - #steps from origin = Nothing
       - Need to generate all possible nodes!
       - Maybe better to do it implicitly somehow
       - any node with X and Y abs values < final node are valid
    - For current Hex
        - Step in all directions
            - Check if this node is in range (X & Y < target)
            - If not, don't change state for next iteration
            - If in range
                - If has value in Dict,
                    - Replace with smaller of
                        parent steps + 1
                        existing value
                - If not in Dict yet
                    - Put it in with parent steps + 1
                    - Add it to the set of unvisited nodes
        - We have just visited the node
            - remove it from unvisited nodes
        - Pick up next unvisted node
            - visit it

    Data structure
        - type alias Point = (Float, Float)
        - Dict Point (Int, Point)
        -      self, steps from origin, parent
-}
{-
   Finite precison issue (accumulated errors)
   GETting a hex point from a Dict may not work if the value is slightly off
    May need to deliberately limit precision in a controlled way?
    Give the point a hash ID made of rounded X and Y?
    Also store the exact values?
-}


{-| Adjacency table
-}
type alias HashTable =
    Dict Hash TableEntry


type alias Hash =
    ( Int, Int )


type alias TableEntry =
    { self : Hex
    , parentHash : Hash
    , steps : Maybe Int
    }


dijkstra : Hex -> Int
dijkstra ( x, y ) =
    let
        bottomLeft =
            ( min 0 x
            , min 0 y
            )

        topRight =
            ( max 0 x
            , max 0 y
            )

        unvisited =
            fillGrid (hexInBounds ( x, y )) topRight bottomLeft []

        initHashTable =
            Dict.insert
                (hash origin)
                { self = ( 0, 0 )
                , parentHash = ( 0, 0 )
                , steps = Just 0
                }
                Dict.empty
    in
        0


{-| Create a grid, given the max X and Y boundary
-}
fillGrid : (Hex -> Bool) -> Hex -> Hex -> List Hex -> List Hex
fillGrid boundsCheck topRight current list =
    if not (boundsCheck current) then
        list
    else
        fillGrid
            boundsCheck
            topRight
            (step N current)
            (fillRow boundsCheck topRight current list)


fillRow : (Hex -> Bool) -> Hex -> Hex -> List Hex -> List Hex
fillRow boundsCheck topRight current list =
    if boundsCheck current then
        list
    else
        let
            hex1 =
                step SE current

            hex2 =
                step NE hex1
        in
            fillRow
                boundsCheck
                topRight
                hex2
                (hex2 :: hex1 :: list)


visit : Hex -> HashTable -> TableEntry -> HashTable
visit boundary table currentEntry =
    let
        childEntries =
            allDirections
                |> List.map (flip step currentEntry.self)
                |> List.filter (hexInBounds boundary)
                |> List.map
                    (\hex ->
                        { self = hex
                        , parentHash = hash currentEntry.self
                        , steps = Maybe.map ((+) 1) currentEntry.steps
                        }
                    )
    in
        List.foldl
            (\entry accTable ->
                Dict.update
                    (hash entry.self)
                    (updateTable entry)
                    accTable
            )
            table
            childEntries


{-| Hash to manage rounding errors in Dict keys

Taking two different paths to the same hex could result in two *slightly* different
values for its coordinates (at the nth decimal place)
So we could *think* something is not in the table when actually it is. Yuck.
Make sure we always round to the nearest 'hex', in a controlled way.

-}
hash : Hex -> Hash
hash ( x, y ) =
    ( round (x * 10)
    , round (y * 10)
    )


updateTable : TableEntry -> Maybe TableEntry -> Maybe TableEntry
updateTable new maybeOld =
    case maybeOld of
        Nothing ->
            Just new

        Just old ->
            case ( new.steps, old.steps ) of
                ( Just newSteps, Just oldSteps ) ->
                    if newSteps < oldSteps then
                        Just new
                    else
                        Just old

                _ ->
                    Just new


allDirections : List Direction
allDirections =
    [ N, NE, NW, S, SE, SW ]


hexInBounds : Hex -> Hex -> Bool
hexInBounds ( xBound, yBound ) ( xTest, yTest ) =
    coordInBounds xBound xTest
        && coordInBounds yBound yTest


coordInBounds : Float -> Float -> Bool
coordInBounds bound test =
    if bound > 0 then
        test - bound < 0.9
    else
        test - bound > -0.9


isOrigin : Hex -> Bool
isOrigin ( x, y ) =
    (x < 0.1) && (y < 0.1)


{-| Directions examples
ne,ne,ne is 3 steps away.
ne,ne,sw,sw is 0 steps away (back where you started).
ne,ne,s,s is 2 steps away (se,se).
se,sw,se,sw,sw is 3 steps away (s,s,sw).
-}
dirExamples =
    [ ( "ne,ne,ne", 3 )
    , ( "ne,ne,sw,sw", 0 )
    , ( "ne,ne,s,s", 2 )
    , ( "se,se", 2 )
    , ( "se,sw,se,sw,sw", 3 )
    , ( "s,s,sw", 3 )
    ]


testFollowDirections =
    List.map
        (\( str, fewestSteps ) ->
            let
                finalHex =
                    directionsFromString str
                        |> followDirections
            in
                ( str, fewestSteps, finalHex, distance finalHex )
        )
        dirExamples


pretty : List a -> String
pretty xs =
    List.map toString xs
        |> String.join "\n"
        |> flip Debug.log ""
