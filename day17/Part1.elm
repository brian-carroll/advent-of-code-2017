module Part1 exposing (..)

import List.Extra


type alias CircularBuffer =
    { step : Int
    , size : Int
    , position : Int
    , values : List Int
    }


init : Int -> CircularBuffer
init step =
    { step = step
    , size = 1
    , position = 0
    , values = [ 0 ]
    }


step : CircularBuffer -> CircularBuffer
step c =
    { c
        | position = (c.position + c.step) % c.size
    }


insert : Int -> CircularBuffer -> CircularBuffer
insert val c =
    let
        newPos =
            c.position + 1

        newValues =
            List.take newPos c.values
                ++ (val :: List.drop newPos c.values)
    in
        { c
            | values = newValues
            , position = newPos
            , size = c.size + 1
        }


iterateOnce : Int -> CircularBuffer -> CircularBuffer
iterateOnce val c =
    c |> step |> insert val


iterateN : Int -> CircularBuffer -> CircularBuffer
iterateN n c =
    List.foldl
        iterateOnce
        c
        (List.range 1 n)


afterLastValue : Int -> Int -> Maybe Int
afterLastValue stepSize target =
    let
        c =
            init stepSize
                |> iterateN target

        -- |> Debug.log "c"
    in
        c.values
            |> List.Extra.dropWhile ((/=) target)
            |> List.drop 1
            |> List.head


example : Maybe Int
example =
    afterLastValue 3 2017


answer : Maybe Int
answer =
    afterLastValue 359 2017
