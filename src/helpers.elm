module Helpers exposing (..)

import Types exposing (..)
import Set exposing (Set)
import Time exposing (second)
import Pattern exposing (Pattern)
import Constants exposing (..)

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

createModel : Board -> Model
createModel board =
  { board = board
  , paused = True
  , interval = second / 10
  , lastUpdate = 0
  , tempBoard = Set.empty
  , pattern = Pattern.blinker
  , iOffset = boardSize // 2
  , jOffset = boardSize // 2
  , isEraser = False
  }
