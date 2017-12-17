module Part1 exposing (..)

import Input exposing (input)
import Dict exposing (Dict)


type Direction
    = Inc
    | Dec


type alias Layer =
    { range : Int
    , scannerPos : Int
    , direction : Direction
    }


type alias Depth =
    Int


type alias Firewall =
    Dict Depth Layer


type alias State =
    { packetDepth : Int
    , firewall : Firewall
    }


initState : List ( Int, Int ) -> State
initState input =
    { packetDepth = 0
    , firewall =
        List.foldl
            (\( depth, range ) dict ->
                Dict.insert
                    depth
                    { range = range, scannerPos = 0, direction = Inc }
                    dict
            )
            Dict.empty
            input
    }


updateScanner : Layer -> Layer
updateScanner layer =
    case layer.direction of
        Inc ->
            if layer.scannerPos >= (layer.range - 1) then
                { layer
                    | direction = Dec
                    , scannerPos = (layer.scannerPos - 1)
                }
            else
                { layer
                    | scannerPos = (layer.scannerPos + 1)
                }

        Dec ->
            if layer.scannerPos <= 0 then
                { layer
                    | direction = Inc
                    , scannerPos = 1
                }
            else
                { layer
                    | scannerPos = (layer.scannerPos - 1)
                }


updateFirewall : Firewall -> Firewall
updateFirewall firewall =
    Dict.map
        (\k v -> updateScanner v)
        firewall


countLayers : State -> Int
countLayers state =
    Dict.keys state.firewall
        |> List.maximum
        |> Maybe.withDefault 0


updateState : State -> State
updateState state =
    { packetDepth = state.packetDepth + 1
    , firewall = updateFirewall state.firewall
    }


getSeverity : State -> Int
getSeverity state =
    case Dict.get state.packetDepth state.firewall of
        Nothing ->
            0

        Just layer ->
            if layer.scannerPos == 0 then
                state.packetDepth * layer.range
            else
                0


run : List ( Int, Int ) -> Int
run input =
    let
        initialState =
            initState input

        layers =
            List.range 0 (countLayers initialState)

        ( finalState, finalSeverity ) =
            List.foldl
                (\_ ( state, severity ) ->
                    ( updateState state
                    , severity + (getSeverity state)
                    )
                )
                ( initialState, 0 )
                layers
    in
        finalSeverity



{-

   Logging visualisations

-}


logState : State -> String
logState state =
    state.firewall
        |> Dict.keys
        |> List.maximum
        |> Maybe.withDefault 0
        |> List.range 0
        |> List.map (logDepth state)
        |> String.join "\n"
        |> flip (++) "\n\n"
        |> flip Debug.log ""


logDepth : State -> Int -> String
logDepth state depth =
    toString depth
        ++ " "
        ++ logPacket state depth
        ++ " "
        ++ logLayer state depth


logLayer : State -> Int -> String
logLayer state depth =
    case Dict.get depth state.firewall of
        Nothing ->
            ""

        Just layer ->
            List.range 0 (layer.range - 1)
                |> List.map
                    (\x ->
                        if x == layer.scannerPos then
                            "[S]"
                        else
                            "[ ]"
                    )
                |> String.join " "


logPacket : State -> Int -> String
logPacket state depth =
    if state.packetDepth == depth then
        "*"
    else
        " "
