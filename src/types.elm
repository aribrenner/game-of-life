module Types exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Time exposing (Time, second)
import Keyboard exposing (KeyCode)

import Pattern exposing (Pattern)

type alias Pair = (Int, Int)
type alias Board = Set Pair
type alias BoardRowIndexes = List Pair

type Msg
  = ClearBoard
  | ClearTempBoard
  | SaveBoard
  | SetEraser
  | SetPattern Pattern
  | SetTempBoard Pair
  | SetTempToBoard Pair
  | TogglePause
  | UpdateInterval String
  | UpdateKeyPress KeyCode
  | UpdateOffsetI String
  | UpdateOffsetJ String
  | UpdateTick Time

type alias EncodedGame =
  { board : List Pair
  , iOffset : Int
  , jOffset : Int
  , interval : Float
  }

type alias MaybeEncodedGame = Maybe EncodedGame
