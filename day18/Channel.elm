module Channel
    exposing
        ( Channel
        , empty
        , send
        , receive
        , isEmpty
        , countSentItems
        , getReadIndex
        , updateReadIndex
        )

import Dict exposing (Dict)


{-| FIFO channel
wIndex index is always >= rIndex index
-}
type Channel a
    = Channel
        { data : Dict Int a
        , wIndex : Int
        , rIndex : Int
        }


empty : Channel a
empty =
    Channel
        { data = Dict.empty
        , wIndex = 0
        , rIndex = 0
        }


send : a -> Channel a -> Channel a
send value (Channel c) =
    Channel
        { c
            | data = Dict.insert c.wIndex value c.data
            , wIndex = c.wIndex + 1
        }


receive : Channel a -> ( Maybe a, Channel a )
receive (Channel c) =
    case Dict.get c.rIndex c.data of
        Nothing ->
            ( Nothing
            , Channel c
            )

        Just value ->
            ( Just value
            , Channel
                { c
                    | rIndex = c.rIndex + 1
                }
            )


isEmpty : Channel a -> Bool
isEmpty (Channel c) =
    c.rIndex >= c.wIndex


updateReadIndex : Int -> Channel a -> Channel a
updateReadIndex rIndex (Channel c) =
    Channel { c | rIndex = rIndex }


getReadIndex : Channel a -> Int
getReadIndex (Channel c) =
    c.rIndex


countSentItems : Channel a -> Int
countSentItems (Channel c) =
    Dict.size c.data
