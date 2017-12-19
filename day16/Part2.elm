module Part2 exposing (..)

import Part1 exposing (DanceMove)


{-
   Load specific Part1 stuff into REPL, along with Part2
-}


dance : String -> List DanceMove -> String
dance =
    Part1.dance


initialLineup : String
initialLineup =
    Part1.initialLineup


inputDanceMoves : () -> List DanceMove
inputDanceMoves =
    Part1.inputDanceMoves



{-
   Work out the repetitive pattern in the dance so we don't have to do it a billion times
-}


findRepetition : String -> List DanceMove -> List String -> List String
findRepetition init moves reverseHistory =
    case reverseHistory of
        [] ->
            findRepetition init moves [ dance init moves, init ]

        lineup :: rest ->
            if lineup == init then
                List.reverse rest
            else
                let
                    nextLineup =
                        dance lineup moves
                in
                    findRepetition init moves (nextLineup :: reverseHistory)


billion : Int
billion =
    1000 * 1000 * 1000


answer : () -> String
answer _ =
    let
        moves =
            inputDanceMoves ()

        pattern =
            findRepetition initialLineup moves []

        remainder =
            billion % (List.length pattern)
    in
        List.indexedMap (,) pattern
            |> Debug.log "pattern"
            |> List.filter (\( i, s ) -> i == remainder)
            |> (\x ->
                    case x of
                        [ ( _, s ) ] ->
                            s

                        _ ->
                            ""
               )
