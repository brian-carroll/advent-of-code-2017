module Part2 exposing (..)


type alias Generator =
    { factor : Int
    , value : Int
    , criterion : Int
    }


iterations : Int
iterations =
    5 * 1000 * 1000


divisor : Int
divisor =
    2147483647


twoToThe16 : Int
twoToThe16 =
    2 ^ 16


generate : Generator -> Generator
generate gen =
    let
        newGen =
            { gen
                | value = (gen.factor * gen.value) % divisor
            }
    in
        if newGen.value % gen.criterion == 0 then
            newGen
        else
            generate newGen


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
                    (a.value % twoToThe16)
                        == (b.value % twoToThe16)
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
            , criterion = 4
            }

        generatorB =
            { factor = 48271
            , value = 8921
            , criterion = 8
            }
    in
        run generatorA generatorB


answer : () -> Int
answer _ =
    let
        generatorA =
            { factor = 16807
            , value = 703
            , criterion = 4
            }

        generatorB =
            { factor = 48271
            , value = 516
            , criterion = 8
            }
    in
        run generatorA generatorB
