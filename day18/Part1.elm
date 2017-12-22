module Part1 exposing (..)

{-

   snd X plays a sound with a frequency equal to the value of X.
   set X Y sets register X to the value of Y.
   add X Y increases register X by the value of Y.
   mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
   mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
   rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
   jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

-}

import Dict exposing (Dict)
import Parser exposing (..)
import Char
import Input exposing (..)


type alias RegName =
    String


type Operand
    = Register RegName
    | Value Int


type alias State =
    { registers : Dict RegName Int
    , program : Dict Int Instruction
    , progCounter : Int
    , lastSound : Int
    , recovered : Maybe Int
    }


type Instruction
    = Snd Operand
    | Set RegName Operand
    | Add RegName Operand
    | Mul RegName Operand
    | Mod RegName Operand
    | Rcv RegName
    | Jgz Operand Operand


space : Parser ()
space =
    ignore (Exactly 1) ((==) ' ')


newline : Parser ()
newline =
    ignore (Exactly 1) ((==) '\n')


regParser : Parser RegName
regParser =
    keep (Exactly 1) Char.isLower


valueParser : Parser Int
valueParser =
    oneOf
        [ int
        , succeed negate
            |. symbol "-"
            |= int
        ]


operandParser : Parser Operand
operandParser =
    oneOf
        [ Parser.map Register regParser
        , Parser.map Value valueParser
        ]


instructionParser : Parser Instruction
instructionParser =
    Parser.oneOf
        [ succeed Snd |. symbol "snd" |. space |= operandParser
        , succeed Set |. symbol "set" |. space |= regParser |. space |= operandParser
        , succeed Add |. symbol "add" |. space |= regParser |. space |= operandParser
        , succeed Mul |. symbol "mul" |. space |= regParser |. space |= operandParser
        , succeed Mod |. symbol "mod" |. space |= regParser |. space |= operandParser
        , succeed Rcv |. symbol "rcv" |. space |= regParser
        , succeed Jgz |. symbol "jgz" |. space |= operandParser |. space |= operandParser
        ]


programFromList : List Instruction -> Dict Int Instruction
programFromList instructions =
    List.indexedMap (,) instructions
        |> Dict.fromList


programParser : Parser (Dict Int Instruction)
programParser =
    succeed programFromList
        |= repeat oneOrMore
            (instructionParser
                |. oneOf [ newline, end ]
            )


init : State
init =
    { registers = Dict.empty
    , program = Dict.empty
    , progCounter = 0
    , lastSound = 0
    , recovered = Nothing
    }


incrementPc : State -> State
incrementPc state =
    { state | progCounter = state.progCounter + 1 }


execute : State -> State
execute ({ registers, lastSound, program, progCounter } as state) =
    let
        valueOf op =
            case op of
                Register name ->
                    Dict.get name registers
                        |> Maybe.withDefault 0

                Value x ->
                    x

        read reg =
            valueOf (Register reg)

        write reg value =
            Dict.insert reg value registers
    in
        case Dict.get progCounter program of
            Nothing ->
                Debug.crash "Executing out of program memory!"

            Just instruction ->
                (case instruction of
                    Snd op ->
                        { state
                            | lastSound =
                                valueOf op
                        }

                    Set reg op ->
                        { state
                            | registers =
                                write reg (valueOf op)
                        }

                    Add reg op ->
                        { state
                            | registers =
                                (read reg)
                                    + (valueOf op)
                                    |> write reg
                        }

                    Mul reg op ->
                        { state
                            | registers =
                                (read reg)
                                    * (valueOf op)
                                    |> write reg
                        }

                    Mod reg op ->
                        { state
                            | registers =
                                (read reg)
                                    % (valueOf op)
                                    |> write reg
                        }

                    Rcv reg ->
                        { state
                            | recovered =
                                if read reg > 0 then
                                    Just state.lastSound
                                else
                                    state.recovered
                        }

                    Jgz op1 op2 ->
                        { state
                            | progCounter =
                                if valueOf op1 > 0 then
                                    state.progCounter + (valueOf op2) - 1
                                else
                                    state.progCounter
                        }
                )
                    |> incrementPc


runProgram : State -> Int
runProgram state =
    case state.recovered of
        Nothing ->
            runProgram (execute state)

        Just recovered ->
            recovered


answer : () -> Result Parser.Error Int
answer _ =
    Parser.run programParser input
        |> Result.map
            (\prog ->
                runProgram { init | program = prog }
            )
