module Update exposing (update)
import Types exposing (..)
import Constants exposing (..)
import Set exposing (Set)
import Time exposing (Time, second)
import Ports exposing (..)
import Helpers exposing (..)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (updatedModel msg model, getCommand msg model.board)

updatedModel : Msg -> Model -> Model
updatedModel msg model =
  case msg of
    TogglePause ->
      { model | paused = not model.paused }
    Tick newTime ->
      let
        isRecent = (newTime - model.lastUpdate) < model.interval
        shouldRedraw = not (model.paused || isRecent)
      in
        if shouldRedraw then
          { model | board = newDict model, lastUpdate = newTime }
        else
          model
    UpdateInterval str ->
      {model | interval = Result.withDefault second (String.toFloat str)}
    ClearBoard ->
      {model | board = Set.empty}
    SetTempToBoard pair ->
      if model.isEraser then
        {model | board = Set.remove pair model.board}
      else
        {model | board = Set.union (model.tempBoard) model.board}
    SetPattern pattern ->
      if model.pattern == pattern && not model.isEraser then
        {model | isEraser = True}
      else
        {model | pattern = pattern, isEraser = False}
    SetTempBoard pair ->
      if model.isEraser then
        model
      else
        {model | tempBoard = createTempBoard model pair}
    ClearTempBoard ->
      {model | tempBoard = Set.empty}
    UpdateOffsetI str ->
      {model | iOffset = Result.withDefault 0 (String.toInt str)}
    UpdateOffsetJ str ->
      {model | jOffset = Result.withDefault 0 (String.toInt str)}
    SetEraser ->
      { model | isEraser = not model.isEraser }
    SaveBoard ->
      model
    KeyMsg keyCode ->
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

getCommand : Msg -> Board -> Cmd Msg
getCommand msg board =
  case msg of
    SaveBoard ->
      saveBoard (encondBoard board)
    _ ->
      noCmd

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
    { model | interval = val}

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

encondBoard : Board -> String
encondBoard board =
  toString(
    List.map (\t ->
      [Tuple.first t, Tuple.second t]
    ) (Set.toList board)
  )
