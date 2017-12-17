module CircularList exposing (..)

-- ( CircularList
-- , reverseSection
-- , create
-- , check
-- , knotHash
-- )

import Array.Hamt as Array exposing (..)


{-| opaque type
-}
type CircularList
    = CircularList
        { position : Int
        , skipSize : Int
        , values : Array Int
        }


create : Int -> CircularList
create size =
    CircularList
        { position = 0
        , skipSize = 0
        , values = Array.initialize size identity
        }


reverseSection : Int -> CircularList -> CircularList
reverseSection sectionLength (CircularList c) =
    let
        size =
            -- Debug.log "size" <|
            Array.length c.values

        end1 =
            -- Debug.log "end1" <|
            min size (c.position + sectionLength)

        slice1 =
            -- Debug.log "slice1" <|
            Array.slice c.position end1 c.values

        end2 =
            -- Debug.log "end2" <|
            c.position
                + sectionLength
                - size

        slice2 =
            -- Debug.log "slice2" <|
            if end2 <= 0 then
                Array.empty
            else
                Array.slice 0 end2 c.values

        wrapped =
            -- Debug.log "wrapped" <|
            Array.append slice1 slice2

        wrappedMaxIdx =
            (Array.length wrapped) - 1

        newValues =
            -- Debug.log "newValues" <|
            reverseInto wrapped c.values wrappedMaxIdx c.position size

        -- _ =
        --     Debug.log "------------------" "------------------"
    in
        CircularList
            { position = (c.position + sectionLength + c.skipSize) % size
            , skipSize = c.skipSize + 1
            , values = newValues
            }


reverseInto : Array a -> Array a -> Int -> Int -> Int -> Array a
reverseInto smallArray bigArray smallIdx bigIdx bigSize =
    -- let
    --     _ =
    --         Debug.log "reverseInto"
    --             { smallArray = Array.toList smallArray
    --             , bigArray = Array.toList bigArray
    --             , smallIdx = smallIdx
    --             , bigIdx = bigIdx
    --             , bigSize = bigSize
    --             }
    -- in
    case Array.get smallIdx smallArray of
        Nothing ->
            bigArray

        Just smallArrayVal ->
            reverseInto
                smallArray
                (Array.set bigIdx smallArrayVal bigArray)
                (smallIdx - 1)
                ((bigIdx + 1) % bigSize)
                bigSize


check : CircularList -> Int
check (CircularList c) =
    c.values
        |> Array.toList
        |> List.take 2
        |> List.product
