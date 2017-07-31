module Types exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Time exposing (Time, second)
import Keyboard exposing (KeyCode)

import Pattern exposing (Pattern)

type alias Pair = (Int, Int)
type alias Board = Set Pair
type alias BoardRowIndexes = List Pair
type alias BoardIndexes = List BoardRowIndexes

type Msg
  = TogglePause
  | Tick Time
  | UpdateInterval String
  | ClearBoard
  | SetPattern Pattern
  | SetTempBoard Pair
  | ClearTempBoard
  | SetTempToBoard Pair
  | UpdateOffsetI String
  | UpdateOffsetJ String
  | KeyMsg KeyCode
  | SetEraser
  | SaveBoard
