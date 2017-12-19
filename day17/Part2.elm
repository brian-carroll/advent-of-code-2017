module Part2 exposing (..)

import List.Extra


{-
   work out what *would* happen in position 1
   when does something get inserted in position 1?
   When current position is zero

   size 1 2
   pos  0 (1+x)%2

-}


loop step target size pos val1 current =
    if current == target then
        val1
    else
        let
            newPos =
                (pos + step) % size

            newSize =
                size + 1

            newVal1 =
                if newPos == 0 then
                    current
                else
                    val1

            -- _ =
            --     Debug.log "args"
            --         { size = newSize
            --         , pos = newPos + 1
            --         , val1 = newVal1
            --         , current = current
            --         }
        in
            loop step target newSize (newPos + 1) newVal1 (current + 1)


smallExample =
    loop 3 10 1 0 -1 1


example =
    loop 3 2107 1 0 -1 1


fiftyMillion =
    50 * 1000 * 1000


answer : () -> Int
answer _ =
    loop 359 fiftyMillion 1 0 -1 1
