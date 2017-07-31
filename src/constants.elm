module Constants exposing (..)

boardSize = 40
intervalMin = 30
intervalMax = 500
intervalStep = 10
boardIndexes = List.range 0 (boardSize - 1)

noCmd = Cmd.none
