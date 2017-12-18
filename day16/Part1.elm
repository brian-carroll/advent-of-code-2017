module Part1 exposing (..)


type DanceMove
    = Spin Int
    | Exchange Int Int
    | Partner String String


{-| Spin
Move n chars from end to start, without changing order
-}
spin : Int -> String -> String
spin n lineup =
    let
        end =
            String.right n lineup

        start =
            String.dropRight n lineup
    in
        end ++ start


{-| Exchange
Swap at positions a and b
-}
exchange : Int -> Int -> String -> String
exchange a b lineup =
    if a == b then
        lineup
    else if a < b then
        let
            sBegin =
                String.slice 0 a lineup

            sa =
                String.slice a (a + 1) lineup

            sMid =
                String.slice (a + 1) b lineup

            sb =
                String.slice b (b + 1) lineup

            sEnd =
                String.dropLeft (b + 1) lineup
        in
            sBegin ++ sb ++ sMid ++ sa ++ sEnd
    else
        exchange b a lineup


{-| Partner
Swap letters a and b
-}
partner : String -> String -> String -> String
partner a b lineup =
    let
        listA =
            String.indices a lineup

        listB =
            String.indices b lineup
    in
        case ( listA, listB ) of
            ( [ xa ], [ xb ] ) ->
                exchange xa xb lineup

            _ ->
                lineup


doMove : DanceMove -> String -> String
doMove move lineup =
    case move of
        Spin x ->
            spin x lineup

        Exchange a b ->
            exchange a b lineup

        Partner a b ->
            partner a b lineup


example =
    { lineup = "abcde"
    , moves = [ Spin 1, Exchange 3 4, Partner "e" "b" ]
    }


exampleAnswer : String
exampleAnswer =
    List.foldl
        doMove
        example.lineup
        example.moves
