module UpdateHelper exposing (
    clearBoard,
    clearTempBoard,
    saveBoard,
    setEraser,
    setPattern,
    setTempBoard,
    setTempToBoard,
    togglePause,
    updateInterval,
    updateKeyPress,
    updateOffsetI,
    updateOffsetJ,
    updateTick )

import Types exposing (..)
import Keyboard exposing (KeyCode)
import Pattern exposing (Pattern)
import Constants exposing (..)
import Set exposing (Set)
import Time exposing (Time, second)
import Model exposing (..)

clearBoard : Model -> Model
clearBoard model =
  { model | board = Set.empty }

clearTempBoard : Model -> Model
clearTempBoard model =
  { model | tempBoard = Set.empty }

saveBoard : Model -> Model
saveBoard model =
  -- this update only serves to trigger command
  model

setEraser : Model -> Model
setEraser model =
  { model | isEraser = not model.isEraser }

setPattern : Model -> Pattern -> Model
setPattern model pattern =
  if model.pattern == pattern && not model.isEraser then
    {model | isEraser = True}
  else
    {model | pattern = pattern, isEraser = False}

setTempBoard : Model -> Pair -> Model
setTempBoard model pair =
  if model.isEraser then
    model
  else
    {model | tempBoard = createTempBoard model pair}

setTempToBoard : Model -> Pair -> Model
setTempToBoard model pair =
  if model.isEraser then
    {model | board = Set.remove pair model.board}
  else
    {model | board = Set.union (model.tempBoard) model.board}

togglePause : Model -> Model
togglePause model =
  { model | paused = not model.paused }

updateInterval : Model -> String -> Model
updateInterval model str =
  {model | interval = Result.withDefault second (String.toFloat str)}

updateKeyPress : KeyCode -> Model -> Model
updateKeyPress keyCode model =
  case keyCode of
    37 -> -- left
      {model | jOffset = model.jOffset + 1}
    38 -> -- up
      {model | iOffset = model.iOffset + 1}
    39 -> -- right
      {model | jOffset = model.jOffset - 1}
    40 -> -- down
      {model | iOffset = model.iOffset - 1}
    32 -> -- spacebar
      {model | paused = not model.paused }
    187 -> -- +
      decrementInterval model
    189 -> -- -
      incrementInterval model
    _ ->
      model

updateOffsetI : Model -> String -> Model
updateOffsetI model str =
  { model | iOffset = strToInt str }

updateOffsetJ : Model -> String -> Model
updateOffsetJ model str =
  { model | jOffset = strToInt str }

updateTick : Model -> Time -> Model
updateTick model newTime =
  if shouldRedraw newTime model then
    { model | board = newDict model, lastUpdate = newTime }
  else
    model

-- private

incrementInterval : Model -> Model
incrementInterval model =
  let
    mayb = List.minimum [model.interval + intervalStep, intervalMax]
    val = Maybe.withDefault intervalMax mayb
  in
    { model | interval = val}

decrementInterval : Model -> Model
decrementInterval model =
  let
    mayb = List.maximum [model.interval - intervalStep, intervalMin]
    val = Maybe.withDefault intervalMin mayb
  in
    { model | interval = val }

updatedPos : Pair -> Board -> Bool
updatedPos pair board =
  let
    count = occupiedNeighbors pair board
  in
    if (isAlive board pair) then
      count == 2 || count == 3
    else
      count == 3

newDict : Model -> Board
newDict model =
  let
    board = model.board
    list = List.filter (\p -> updatedPos p board) fullBoard
  in
    Set.fromList list

shouldRedraw : Time -> Model -> Bool
shouldRedraw newTime model =
  let
    isRecent = (newTime - model.lastUpdate) < model.interval
  in
    not (model.paused || isRecent)

createTempBoard : Model -> Pair -> Board
createTempBoard model pair =
  let
    patternBoard = Set.fromList model.pattern
  in
    Set.map (\p ->
      let
        i = Tuple.first pair + Tuple.first p
        j = Tuple.second pair + Tuple.second p
      in
        (i % boardSize, j % boardSize)
    ) patternBoard

strToInt : String -> Int
strToInt str =
  Result.withDefault 0 (String.toInt str)
