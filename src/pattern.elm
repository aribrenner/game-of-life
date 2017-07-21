module Pattern exposing (Pattern, patterns, blinker)

{-| `Game of Life` patterns
@docs Pattern
@docs patterns
@docs blinker
-}

{-| type for `Game of Life` patterns
@docs Pattern
-}
type alias Pattern = List (Int, Int)

{-| a common GoL osillator
@docs blinker
-}
blinker : Pattern
blinker = [(0,0), (0,1), (0,2)]

glider : Pattern
glider = [(2,0), (2,1), (2,2), (1,2), (0,1)]

llws : Pattern
llws = [(0,0), (3,0), (4,1), (4,2), (4,3), (3,3), (2,3), (1,3), (0,2)]

dot : Pattern
dot = [(0,0)]

block : Pattern
block = [(0,0), (0,1), (1,0), (1,1)]

pentomino : Pattern
pentomino = [(0,1), (0,2), (1,0), (1,1), (2, 1)]


{-| List of common GoL patterns
@docs patterns
-}
patterns : List Pattern
patterns = [blinker, glider, llws, dot, block, pentomino]
