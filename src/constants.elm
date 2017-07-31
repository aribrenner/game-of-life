module Constants exposing (boardSize, intervalMin, intervalMax, intervalStep, boardIndexes, noCmd, fullBoard)
import Types exposing (..)

boardSize = 40
intervalMin = 30
intervalMax = 500
intervalStep = 10
boardIndexes = List.range 0 (boardSize - 1)

noCmd = Cmd.none

fullBoard : BoardRowIndexes
fullBoard =
  List.concat (buildBoard (boardSize - 1))
--
--
buildBoard : Int -> BoardIndexes
buildBoard cur =
  let
    miniBoard = [fullRow (boardSize - 1) cur]
  in
    if cur == 0 then miniBoard else List.append (buildBoard (cur - 1)) miniBoard

fullRow : Int -> Int -> BoardRowIndexes
fullRow i j =
  let
    pair = [(i, j)]
  in
    if i == 0 then pair else List.append (fullRow (i-1) j) pair
