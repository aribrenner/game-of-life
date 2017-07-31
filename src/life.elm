port module Main exposing (..)

import Html exposing (Html, div, text, span, button, input, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOver, onMouseOut)
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time, second)
import Pattern exposing (..)
import Keyboard exposing (KeyCode)


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Pair = (Int, Int)
type alias Board = Set Pair
type alias BoardRowIndexes = List Pair
type alias BoardIndexes = List BoardRowIndexes
type alias Flags = { board : List Pair }

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

boardSize = 40
intervalMin = 30
intervalMax = 500
intervalStep = 10

noCmd = Cmd.none

encondBoard : Board -> String
encondBoard board =
  toString(
    List.map (\t ->
      [Tuple.first t, Tuple.second t]
    ) (Set.toList board)
  )

init : Flags -> (Model, Cmd Msg)
init flags =
  (createModel (Set.fromList flags.board), noCmd)


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



-- UPDATE

port saveBoard : String -> Cmd msg


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


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 23 Tick
    , Keyboard.downs KeyMsg
    ]


nums : Int -> List Int
nums int =
  if int == 0 then
    []
  else
    List.append (nums (int - 1)) [int - 1]

isAlive : Board -> Pair -> Bool
isAlive board pair =
  Set.member pair board

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

occupiedNeighbors : Pair -> Board -> Int
occupiedNeighbors pair board =
  let
    neighbors = allNeighbors pair
  in
    List.length (List.filter (\p -> isAlive board p) neighbors)

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


onBoard : Pair -> Bool
onBoard pair =
  let
    i = Tuple.first pair
    j = Tuple.second pair
  in
    i >= 0 && j >= 0 && i < boardSize && j < boardSize

-- VIEW


view : Model -> Html Msg
view model =
  div [class "container clearfix"]
    [ pageHeader
    , boardStuff model
    , controls model
    ]

boardStuff : Model -> Html Msg
boardStuff model =
  span [class "board-stuff"]
    [ offsetSlider model.iOffset UpdateOffsetI "vertical"
    , drawBoard model
    , offsetSlider model.jOffset UpdateOffsetJ "horizontal"
    ]


pageHeader : Html Msg
pageHeader =
 div [class "page-header"] [text "Conway's Game of Life"]

controls : Model -> Html Msg
controls model =
  span [class "controls"]
    [ pauseButton model.paused
    , clearButton model
    , intervalSlider model.interval
    , patternButtons model
    , eraserButton model
    , saveBoardButton
    ]

eraserButton : Model -> Html Msg
eraserButton model =
  let
    klass = if model.isEraser then "selected" else ""
    klasses = "pattern-button " ++ klass
  in
    button [onClick SetEraser, class klasses]
      [(img [src "assets/images/eraser.png"] [])]


drawBoard : Model -> Html Msg
drawBoard model =
  let
    board = model.board
    klass1 = if model.paused then "paused" else ""
    klass2 = if model.isEraser then "eraser" else ""
  in
    div [class ("board " ++ klass1 ++ " " ++ klass2)] (List.map (drawRow model) (nums boardSize))

drawRow : Model -> Int -> Html Msg
drawRow model i =
  div [class "row"] (List.map (drawCell model i) (nums boardSize))

drawCell : Model -> Int -> Int -> Html Msg
drawCell model i j =
  let
    iVal = (i + model.iOffset) % boardSize
    jVal = (j + model.jOffset) % boardSize

    pair = (iVal, jVal)
    hasTempLife = isAlive model.tempBoard pair
    hasLife = isAlive model.board pair
    klass1 = if hasTempLife then "temp-life" else ""
    klass2 = if hasLife then "life" else ""
    stylePairs = if hasTempLife || hasLife then [] else [("background-color", rgb (jVal - iVal))]
  in
    span [ class "cell-container"
         , onClick (SetTempToBoard pair)
         , onMouseOver (SetTempBoard pair)
         , onMouseOut ClearTempBoard
    ] [
      div
        [ class ("cell " ++ klass1 ++ " " ++ klass2)
        , style stylePairs
        ] []
    ]

rgb : Int -> String
rgb val =
  let
    h = toString ((360 * val) // (boardSize // 1))
  in
    "hsl(" ++ h ++ ",100%,80%)"

pauseButton : Bool -> Html Msg
pauseButton isPaused =
  let
    str = if isPaused then "Unpause" else "Pause"
  in
    button [ onClick TogglePause, class "control-button"] [ text str ]

clearButton : Model -> Html Msg
clearButton model =
  let
    boardIsEmpty = Set.isEmpty model.board
  in
    button [class "control-button", onClick ClearBoard, disabled boardIsEmpty] [text "Clear"]

patternButton : Model -> Pattern -> Html Msg
patternButton model pattern =
  let
    klass = if (model.pattern == pattern && not model.isEraser) then "selected" else ""
    klasses = "pattern-button " ++ klass
  in
    button [onClick (SetPattern pattern), class klasses] [patternPreview pattern]

intervalSlider : Float -> Html Msg
intervalSlider interval =
  input
    [ type_ "range"
    , Html.Attributes.min (toString intervalMin)
    , Html.Attributes.max (toString intervalMax)
    , Html.Attributes.step (toString intervalStep)
    , value (toString interval)
    , onInput UpdateInterval
    , class "interval-slider"
    ] []

offsetSlider : Int -> (String -> Msg) -> String -> Html Msg
offsetSlider val updateFunc klass =
  input
    [ type_ "range"
    , Html.Attributes.min "0"
    , Html.Attributes.max (toString (boardSize - 1))
    , Html.Attributes.step "1"
    , value (toString (val % boardSize))
    , onInput updateFunc
    , class klass
    ] []

patternButtons : Model -> Html Msg
patternButtons model =
  span [class "pattern-buttons"] (
    List.map (\pattern -> patternButton model pattern) Pattern.patterns
  )

patternPreview : Pattern -> Html Msg
patternPreview pattern =
  let
    set = Set.fromList pattern
  in
    span [] (List.map (\i->
      div [class "row"] (List.map (\j->
        let
          klass = if isAlive set (i, j) then "life" else ""
        in
          span [class ("cell " ++ klass)] []
      ) (nums 5))
    ) (nums 5))

saveBoardButton : Html Msg
saveBoardButton =
  button [onClick SaveBoard, class "control-button save-button"] [text "Save Board"]
