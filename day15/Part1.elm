module Part1 exposing (..)

import Bitwise


type alias Generator =
    { factor : Int
    , value : Int
    }


iterations : Int
iterations =
    40 * 1000 * 1000


divisor : Int
divisor =
    2147483647


sixteenOnes : Int
sixteenOnes =
    (2 ^ 16) - 1


generate : Generator -> Generator
generate gen =
    { gen
        | value = (gen.factor * gen.value) % divisor
    }


run : Generator -> Generator -> Int
run genA genB =
    iterate genA genB iterations 0


iterate : Generator -> Generator -> Int -> Int -> Int
iterate genA genB iter matches =
    if iter == 0 then
        matches
    else
        let
            a =
                generate genA

            b =
                generate genB

            newMatches =
                if
                    Bitwise.and a.value sixteenOnes
                        == Bitwise.and b.value sixteenOnes
                then
                    matches + 1
                else
                    matches
        in
            iterate a b (iter - 1) newMatches


example : () -> Int
example _ =
    let
        generatorA =
            { factor = 16807
            , value = 65
            }

        generatorB =
            { factor = 48271
            , value = 8921
            }
    in
        run generatorA generatorB


answer : () -> Int
answer _ =
    let
        generatorA =
            { factor = 16807
            , value = 703
            }

        generatorB =
            { factor = 48271
            , value = 516
            }
    in
        run generatorA generatorB
