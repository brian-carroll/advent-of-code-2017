module Types exposing (..)

import Dict exposing (Dict)


type alias Point =
    ( Int, Int )


type Bit
    = On
    | Off


type alias Pattern =
    Dict Point Bit
