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

type alias EncodedGame =
  { board : List Pair
  , iOffset : Int
  , jOffset : Int
  , interval : Float
  }

type alias MaybeEncodedGame = Maybe EncodedGame
