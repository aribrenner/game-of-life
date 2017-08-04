module Model exposing (..)

import Types exposing (..)
import Set exposing (Set)
import Time exposing (second)
import Pattern exposing (Pattern)
import Constants exposing (..)

type alias Model =
  { board : Board
  , paused : Bool
  , interval : Float
  , lastUpdate : Float
  , tempBoard : Board
  , pattern : Pattern
  , iOffset : Int
  , jOffset : Int
  , isEraser: Bool
  }

isAlive : Board -> Pair -> Bool
isAlive board pair =
  Set.member pair board

occupiedNeighbors : Pair -> Board -> Int
occupiedNeighbors pair board =
  let
    neighbors = allNeighbors pair
  in
    List.length (List.filter (\p -> isAlive board p) neighbors)

allNeighbors : Pair -> List Pair
allNeighbors pair =
  let
    i = Tuple.first pair
    j = Tuple.second pair

    i1 = (i - 1) % boardSize
    i2 = (i + 1) % boardSize
    j1 = (j - 1) % boardSize
    j2 = (j + 1) % boardSize
  in
    [ (i1, j1), (i1, j), (i1, j2)
    , (i,  j1),          (i,  j2)
    , (i2, j1), (i2, j), (i2, j2)
    ]

createModel : EncodedGame -> Model
createModel {board, iOffset, jOffset, interval} =
  { board = Set.fromList board
  , paused = True
  , interval = interval
  , lastUpdate = 0
  , tempBoard = Set.empty
  , pattern = Pattern.blinker
  , iOffset = iOffset
  , jOffset = jOffset
  , isEraser = False
  }
