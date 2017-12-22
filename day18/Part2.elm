module Part2 exposing (..)

{-
   Concurrency with channels

   - Running progs independently and block on Rcv
        - Outer loop
            - Inner loop
                - Execute instructions from prog until Snd or Rcv
            - Now have a tuple of Snd | Rcv
            - Match on this and update 2 channels
            - Work out deadlock condition
                - terminates
    - Termination
        - Program counter falls off the end
        - Deadlock
    - Perhaps use a union type to tell the top-level what to do
        - ChannelSnd | ChannelRcv | Terminated | Continue
        - Hmm, dunno, this is kind of repetition
        - But we do need a Terminated thing in the State

-}

import Dict exposing (Dict)
import Parser
import Part1
    exposing
        ( Instruction(..)
        , RegName
        , Operand(..)
        , programParser
        )
import Input exposing (..)
import Channel exposing (..)


type alias State =
    { registers : Dict RegName Int
    , program : Dict Int Instruction
    , progCounter : Int
    , sendChannel : Channel Int
    , receiveChannel : Channel Int
    , status : Status
    }


type Status
    = Running
    | Waiting
    | Terminated


incrementPc : State -> State
incrementPc state =
    { state | progCounter = state.progCounter + 1 }


execute : State -> State
execute s =
    let
        valueOf op =
            case op of
                Register name ->
                    Dict.get name s.registers
                        |> Maybe.withDefault 0

                Value x ->
                    x

        read reg =
            valueOf (Register reg)

        write reg value =
            Dict.insert reg value s.registers
    in
        case Dict.get s.progCounter s.program of
            Nothing ->
                { s | status = Terminated }

            Just instruction ->
                (case instruction of
                    Snd op ->
                        { s
                            | sendChannel =
                                Channel.send (valueOf op) s.sendChannel
                        }

                    Set reg op ->
                        { s
                            | registers =
                                write reg (valueOf op)
                        }

                    Add reg op ->
                        { s
                            | registers =
                                (read reg)
                                    + (valueOf op)
                                    |> write reg
                        }

                    Mul reg op ->
                        { s
                            | registers =
                                (read reg)
                                    * (valueOf op)
                                    |> write reg
                        }

                    Mod reg op ->
                        { s
                            | registers =
                                (read reg)
                                    % (valueOf op)
                                    |> write reg
                        }

                    Rcv reg ->
                        case Channel.receive s.receiveChannel of
                            ( Nothing, _ ) ->
                                { s
                                    | progCounter = s.progCounter - 1
                                    , status = Waiting
                                }

                            ( Just rx, newChannel ) ->
                                { s
                                    | receiveChannel = newChannel
                                    , registers = write reg rx
                                    , status = Running
                                }

                    Jgz op1 op2 ->
                        { s
                            | progCounter =
                                if valueOf op1 > 0 then
                                    s.progCounter + (valueOf op2) - 1
                                else
                                    s.progCounter
                        }
                )
                    |> incrementPc


init : Int -> Dict Int Instruction -> State
init id prog =
    { registers = Dict.fromList [ ( "p", id ) ]
    , program = prog
    , progCounter = 0
    , sendChannel = Channel.empty
    , receiveChannel = Channel.empty
    , status = Running
    }


shouldTerminated : State -> State -> Bool
shouldTerminated state0 state1 =
    case ( state0.status, state1.status ) of
        ( Terminated, _ ) ->
            True

        ( _, Terminated ) ->
            True

        ( Waiting, Waiting ) ->
            isEmpty state0.sendChannel && isEmpty state1.sendChannel

        _ ->
            False


runPrograms : State -> State -> Int
runPrograms state0 state1 =
    if shouldTerminated state0 state1 then
        Channel.countSentItems state1.sendChannel
    else
        let
            channel0to1 =
                state0.sendChannel
                    |> updateReadIndex (getReadIndex state1.receiveChannel)

            channel1to0 =
                state1.sendChannel
                    |> updateReadIndex (getReadIndex state0.receiveChannel)

            newState0 =
                execute
                    { state0
                        | sendChannel = channel0to1
                        , receiveChannel = channel1to0
                    }

            newState1 =
                execute
                    { state1
                        | sendChannel = channel1to0
                        , receiveChannel = channel0to1
                    }
        in
            runPrograms newState0 newState1


answer : () -> Result Parser.Error Int
answer _ =
    Parser.run programParser input
        |> Result.map
            (\prog ->
                runPrograms (init 0 prog) (init 1 prog)
            )
