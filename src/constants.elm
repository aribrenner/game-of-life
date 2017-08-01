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
  List.concat (List.map fullRow boardIndexes)

fullRow : Int -> BoardRowIndexes
fullRow j =
  List.map (\i -> (i, j)) boardIndexes
