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

type alias Pattern = List Pair

type alias Model = {
  board : Board,
  paused : Bool,
  fullBoard : BoardRowIndexes,
  interval : Float,
  lastUpdate : Float,
  tempBoard : Board,
  pattern : Pattern,
  iOffset : Int,
  jOffset : Int
}

model : Model
model =
  { board = Set.empty
  , paused = True
  , fullBoard = List.concat fullBoard
  , interval = second / 10
  , lastUpdate = 0
  , tempBoard = Set.empty
  , pattern = blinker
  , iOffset = 0
  , jOffset = 0
  }

boardSize = 40
noCmd = Cmd.none

blinker : Pattern
blinker = [(0,0), (0,1), (0,2)]

glider : Pattern
glider = [(2,0), (2,1), (2,2), (1,2), (0,1)]

llws : Pattern
llws = [(0,0), (3,0), (4,1), (4,2), (4,3), (3,3), (2,3), (1,3), (0,2)]

dot : Pattern
dot = [(0,0)]

block : Pattern
block = [(0,0), (0,1), (1,0), (1,1)]

pentomino : Pattern
pentomino = [(0,1), (0,2), (1,0), (1,1), (2, 1)]

patterns = [blinker, glider, llws, dot, block, pentomino]

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
  | SetPattern Pattern
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
  Time.every 23 Tick


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
    , offsetSlider model.iOffset UpdateOffsetI ""
    , offsetSlider model.jOffset UpdateOffsetJ "vertical"
    ]

drawBoard : Model -> Html Msg
drawBoard model =
  let
    board = model.board
    klass = if model.paused then "paused" else ""
  in
    div [class ("board " ++ klass)] (List.map (drawRow model) (nums (boardSize - 1)))

drawRow : Model -> Int -> Html Msg
drawRow model i =
  div [class "row"] (List.map (drawCell model i) (nums (boardSize - 1)))

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
    div
      [ class ("cell " ++ klass1 ++ " " ++ klass2)
      , onClick (SetTempToBoard)
      , onMouseOver (SetTempBoard pair)
      , onMouseOut ClearTempBoard
      , style stylePairs
      ] []

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
    button [ onClick TogglePause, class "pause-button"] [ text str ]

clearButton : Html Msg
clearButton =
  button [class "clear-button", onClick ClearBoard] [text "Clear"]

patternButton : Model -> Pattern -> Html Msg
patternButton model pattern =
  let
    isDisabled = model.pattern == pattern
  in
    button [onClick (SetPattern pattern), class "pattern-button", disabled isDisabled] [patternPreview pattern]

intervalSlider : Float -> Html Msg
intervalSlider interval =
  input
    [ type_ "range"
    , Html.Attributes.min "30"
    , Html.Attributes.max "500"
    , Html.Attributes.step "10"
    , value (toString interval)
    , onInput UpdateInterval
    ] []

offsetSlider : Int -> (String -> Msg) -> String -> Html Msg
offsetSlider val updateFunc klass =
  input
    [ type_ "range"
    , Html.Attributes.min "0"
    , Html.Attributes.max (toString (boardSize - 1))
    , Html.Attributes.step "1"
    , value (toString val)
    , onInput updateFunc
    , class klass
    ] []

patternButtons : Model -> Html Msg
patternButtons model =
  span [] (
    List.map (\pattern -> patternButton model pattern) patterns
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
      ) (nums 4))
    ) (nums 4))

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
