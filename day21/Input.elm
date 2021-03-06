module Input exposing (..)


example : String
example =
    String.trim
        """
../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
"""


input : String
input =
    String.trim
        """
../.. => .../.##/##.
#./.. => .##/.##/#..
##/.. => ..#/.../###
.#/#. => #.#/..#/##.
##/#. => .#./.#./..#
##/## => #.#/#../###
.../.../... => ..../#.../.##./..#.
#../.../... => ####/#.##/##.#/..#.
.#./.../... => ..##/..##/..##/..##
##./.../... => ..../..#./##../##.#
#.#/.../... => ##.#/..../####/...#
###/.../... => .#.#/.###/.#../.#.#
.#./#../... => .###/#.#./...#/##..
##./#../... => #.##/#.../####/###.
..#/#../... => ####/...#/...#/#.##
#.#/#../... => .#../##../..##/..#.
.##/#../... => .#../..##/..../.##.
###/#../... => #.../..#./.#.#/#..#
.../.#./... => #.#./.#.#/.###/...#
#../.#./... => ###./.#../...#/.#..
.#./.#./... => ##.#/.#../#..#/##..
##./.#./... => #..#/...#/.#.#/###.
#.#/.#./... => .##./#.../#..#/.###
###/.#./... => .#.#/##.#/..../##.#
.#./##./... => ##.#/#.##/.#.#/#.##
##./##./... => #.##/..#./..#./.##.
..#/##./... => ..../#.../..#./..##
#.#/##./... => .##./####/####/####
.##/##./... => #.##/####/#.##/#..#
###/##./... => .#../.###/##../...#
.../#.#/... => ...#/...#/#.##/####
#../#.#/... => ..#./..#./###./.##.
.#./#.#/... => .##./##../.###/.#.#
##./#.#/... => #.#./.#../.##./...#
#.#/#.#/... => ##.#/..##/#.../##.#
###/#.#/... => ..##/##../.#.#/..##
.../###/... => .#../#.../.##./....
#../###/... => ..##/..##/...#/.##.
.#./###/... => #..#/..#./#.#./..##
##./###/... => #.##/.#../##.#/##.#
#.#/###/... => ####/###./.##./...#
###/###/... => #..#/#.##/..../.##.
..#/.../#.. => #.#./.#../##../..#.
#.#/.../#.. => ##.#/####/##../.#.#
.##/.../#.. => ####/##../#..#/..#.
###/.../#.. => ##../..#./####/##.#
.##/#../#.. => ##../#.#./###./..##
###/#../#.. => ..../.#../#..#/...#
..#/.#./#.. => ..#./...#/.###/.#.#
#.#/.#./#.. => ###./..../#.#./###.
.##/.#./#.. => ####/#.##/.#.#/.#..
###/.#./#.. => ###./#.##/##../####
.##/##./#.. => ##.#/..##/..#./.#..
###/##./#.. => ##.#/.##./.###/.##.
#../..#/#.. => #.../###./##.#/#..#
.#./..#/#.. => ..##/.###/...#/..#.
##./..#/#.. => ##../#.#./...#/.#..
#.#/..#/#.. => ..#./###./##../.###
.##/..#/#.. => #.../.##./..../#.#.
###/..#/#.. => .#.#/#.##/#.##/..#.
#../#.#/#.. => ..##/..##/#.../####
.#./#.#/#.. => #.../...#/..../..##
##./#.#/#.. => ###./..##/.#../.##.
..#/#.#/#.. => ...#/..##/..#./.#..
#.#/#.#/#.. => #.#./.#../..../##..
.##/#.#/#.. => ..#./.###/##.#/....
###/#.#/#.. => #.##/..##/...#/##..
#../.##/#.. => #.#./##../###./.#.#
.#./.##/#.. => .###/#..#/.##./....
##./.##/#.. => .#.#/.#../.###/.##.
#.#/.##/#.. => .#../..##/###./#.##
.##/.##/#.. => ##../.##./..#./.#..
###/.##/#.. => .#.#/..#./#..#/.###
#../###/#.. => #.##/#..#/.#.#/#.#.
.#./###/#.. => #.../#..#/#.../.#.#
##./###/#.. => ##../####/##../.###
..#/###/#.. => #.../..../####/##.#
#.#/###/#.. => ...#/..../...#/..##
.##/###/#.. => .#../####/#.##/.#..
###/###/#.. => ###./.#.#/#.../##..
.#./#.#/.#. => ...#/##../####/...#
##./#.#/.#. => ####/#..#/###./#.##
#.#/#.#/.#. => .###/#..#/..#./...#
###/#.#/.#. => ###./.###/##.#/###.
.#./###/.#. => #..#/#.../..#./####
##./###/.#. => #.../..../#..#/..##
#.#/###/.#. => #..#/.#.#/#.../##..
###/###/.#. => .#.#/..../.#.#/#.##
#.#/..#/##. => .#../..##/...#/###.
###/..#/##. => .###/..#./##.#/##.#
.##/#.#/##. => ####/#.##/.##./##..
###/#.#/##. => #..#/#..#/####/#.##
#.#/.##/##. => .###/#.#./#..#/.#.#
###/.##/##. => #.#./#.#./#.##/..##
.##/###/##. => ####/###./##.#/##.#
###/###/##. => ##../..##/#.#./#...
#.#/.../#.# => .#../###./.###/##.#
###/.../#.# => ..../.#.#/#..#/##..
###/#../#.# => ..#./#.../.##./...#
#.#/.#./#.# => ...#/#.../##.#/.##.
###/.#./#.# => ..../..../#.#./##.#
###/##./#.# => .#../...#/...#/###.
#.#/#.#/#.# => ...#/#.../##../.###
###/#.#/#.# => #.../...#/.#../#.##
#.#/###/#.# => ..../.##./..../##..
###/###/#.# => .##./.#.#/#.##/.##.
###/#.#/### => #.#./####/.##./.##.
###/###/### => .#.#/..##/#.##/.##.
"""
