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
    , terminated : Bool
    , multiplies : Int
    }


type Instruction
    = Set RegName Operand
    | Sub RegName Operand
    | Mul RegName Operand
    | Jnz Operand Operand


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
        [ succeed Set |. symbol "set" |. space |= regParser |. space |= operandParser
        , succeed Sub |. symbol "sub" |. space |= regParser |. space |= operandParser
        , succeed Mul |. symbol "mul" |. space |= regParser |. space |= operandParser
        , succeed Jnz |. symbol "jnz" |. space |= operandParser |. space |= operandParser
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


init : Dict Int Instruction -> State
init prog =
    { registers = Dict.empty
    , program = prog
    , progCounter = 0
    , terminated = False
    , multiplies = 0
    }


incrementPc : State -> State
incrementPc state =
    { state | progCounter = state.progCounter + 1 }


execute : State -> State
execute ({ registers, program, progCounter } as state) =
    let
        _ =
            Debug.log "state" state

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
                { state | terminated = True }

            Just instruction ->
                (case instruction of
                    Set reg op ->
                        { state
                            | registers =
                                write reg (valueOf op)
                        }

                    Sub reg op ->
                        { state
                            | registers =
                                (read reg)
                                    - (valueOf op)
                                    |> write reg
                        }

                    Mul reg op ->
                        { state
                            | registers =
                                (read reg)
                                    * (valueOf op)
                                    |> write reg
                            , multiplies = state.multiplies + 1
                        }

                    Jnz op1 op2 ->
                        { state
                            | progCounter =
                                if valueOf op1 /= 0 then
                                    state.progCounter + (valueOf op2) - 1
                                else
                                    state.progCounter
                        }
                )
                    |> incrementPc


runProgram : State -> State
runProgram state =
    if state.terminated then
        state
    else
        runProgram (execute state)


answer : () -> Result Parser.Error State
answer _ =
    Parser.run programParser input
        |> Result.map init
        |> Result.map runProgram
        |> Debug.log "Final state"
