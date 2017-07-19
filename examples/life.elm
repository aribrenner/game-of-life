import Html exposing (Html, div, text, node, span, button, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOver, onMouseOut)
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time, second)




main =
  Html.program
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

type alias Model = {
  board : Board,
  paused : Bool,
  fullBoard : BoardRowIndexes,
  interval : Float,
  lastUpdate : Float,
  tempBoard : Board,
  pattern : String,
  iOffset : Int,
  jOffset : Int
}

model : Model
model =
  { board = Set.empty
  , paused = True
  , fullBoard = List.concat fullBoard
  , interval = second
  , lastUpdate = 0
  , tempBoard = Set.empty
  , pattern = "dot"
  , iOffset = 0
  , jOffset = 0
  }

boardSize = 40
noCmd = Cmd.none

libraryList =
  [ ("blinker", pairsToSet [(0,0), (0,1), (0,2)])
  , ("glider",  pairsToSet [(2,0), (2,1), (2,2), (1,2), (0,1)])
  , ("llws",  pairsToSet [(0,0), (3,0), (4,1), (4,2), (4,3), (3,3), (2,3), (1,3), (0,2)])
  , ("dot",  pairsToSet [(0,0)])
  ]

library =
  Dict.fromList libraryList

pairsToSet : List Pair -> Board
pairsToSet list =
  Set.fromList list


init : (Model, Cmd Msg)
init =
  (model, noCmd)


fullBoard : BoardIndexes
fullBoard =
  buildBoard (boardSize - 1)
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


type Msg
  = TogglePause
  | Tick Time
  | UpdateInterval String
  | ClearBoard
  | SetPattern String
  | SetTempBoard Pair
  | ClearTempBoard
  | SetTempToBoard
  | UpdateOffsetI String
  | UpdateOffsetJ String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TogglePause ->
      ({ model | paused = not model.paused }, noCmd)
    Tick newTime ->
      let
        isRecent = (newTime - model.lastUpdate) < model.interval
        shouldRedraw = not (model.paused || isRecent)
      in
        if shouldRedraw then
          ({ model | board = newDict model, lastUpdate = newTime }, noCmd)
        else
          (model, noCmd)
    UpdateInterval str ->
      ({model | interval = Result.withDefault second (String.toFloat str)}, noCmd)
    ClearBoard ->
      ({model | board = Set.empty}, noCmd)
    SetTempToBoard ->
      ({model | board = Set.union (model.tempBoard) model.board}, noCmd)
    SetPattern pattern ->
      ({model | pattern = pattern}, noCmd)
    SetTempBoard pair ->
      ({model | tempBoard = createTempBoard model pair}, noCmd)
    ClearTempBoard ->
      ({model | tempBoard = Set.empty}, noCmd)
    UpdateOffsetI str ->
      ({model | iOffset = Result.withDefault 0 (String.toInt str)}, noCmd)
    UpdateOffsetJ str ->
      ({model | jOffset = Result.withDefault 0 (String.toInt str)}, noCmd)


createTempBoard : Model -> Pair -> Board
createTempBoard model pair =
  let
    patternBoard = boardFromLib model.pattern
  in
    Set.map (\p ->
      let
        first = Tuple.first pair + Tuple.first p
        second = Tuple.second pair + Tuple.second p
      in
        (first, second)
    ) patternBoard



boardFromLib : String -> Board
boardFromLib key =
  Maybe.withDefault Set.empty (Dict.get key library)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 23 Tick

toggleCell : Board -> Pair -> Board
toggleCell board pair =
  let
    cur = isAlive board pair
  in
    if cur then
      Set.remove pair board
    else
      Set.insert pair board


nums : Int -> List Int
nums int =
  if int == 0 then
    [0]
  else
    List.append (nums (int - 1)) [int]

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
    [
      (i1, j1), (i1, j), (i1, j2)
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
    list = List.filter (\p -> updatedPos p board) model.fullBoard
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
  div []
    [ div [] [stylesheet "life.css"]
    , pauseButton model.paused
    , intervalSlider model.interval
    , clearButton
    , patternButtons model
    , drawBoard model
    , offsetSlider model.iOffset UpdateOffsetI
    , offsetSlider model.jOffset UpdateOffsetJ
    ]


drawBoard model =
  let
    board = model.board
    klass = if model.paused then "paused" else ""
  in
    div [class ("board " ++ klass)] (List.map (drawRow model) (nums (boardSize - 1)))

drawRow model i =
  div [class "row"] (List.map (drawCell model i) (nums (boardSize - 1)))

drawCell model i j =
  let
    iVal = (i + model.iOffset) % boardSize
    jVal = (j + model.jOffset) % boardSize
    pair = (iVal, jVal)
    klass1 = if isAlive model.tempBoard pair then "temp-life" else ""
    klass2 = if isAlive model.board pair then "life" else ""
  in
    div
      [ class ("cell " ++ klass1 ++ " " ++ klass2)
      , onClick (SetTempToBoard)
      , onMouseOver (SetTempBoard pair)
      , onMouseOut ClearTempBoard
      ] []

pauseButton isPaused =
  let
    str = if isPaused then "Unpause" else "Pause"
  in
    button [ onClick TogglePause, class "pause-button"] [ text str ]

clearButton =
  button [class "clear-button", onClick ClearBoard] [text "Clear"]

patternButton model pattern =
  let
    isDisabled = model.pattern == pattern
  in
    button [onClick (SetPattern pattern), class "pattern-button", disabled isDisabled] [text pattern]

intervalSlider interval =
  input
    [ type_ "range"
    , Html.Attributes.min "30"
    , Html.Attributes.max "1000"
    , Html.Attributes.step "10"
    , value (toString interval)
    , onInput UpdateInterval
    ] []

offsetSlider val updateFunc =
  input
    [ type_ "range"
    , Html.Attributes.min "0"
    , Html.Attributes.max (toString (boardSize - 1))
    , Html.Attributes.step "1"
    , value (toString val)
    , onInput updateFunc
    ] []

patternButtons model =
  let
    keys = List.map (\p -> Tuple.first p) libraryList
  in
    span [] (
      List.map (\key -> patternButton model key) keys
    )


-- https://gist.github.com/coreytrampe/a120fac4959db7852c0f
stylesheet href =
  let
    tag = "link"
    attrs =
        [ attribute "rel"       "stylesheet"
        , attribute "property"  "stylesheet"
        , attribute "href"      href
        ]
    children = []
  in
    node tag attrs children
