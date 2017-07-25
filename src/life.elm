import Html exposing (Html, div, text, span, button, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOver, onMouseOut)
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time, second)
import Pattern exposing (..)
import Stylesheet exposing (stylesheet)
import Keyboard


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
  pattern : Pattern,
  iOffset : Int,
  jOffset : Int,
  isEraser: Bool
}

model : Model
model =
  { board = Set.empty
  , paused = True
  , fullBoard = List.concat fullBoard
  , interval = second / 10
  , lastUpdate = 0
  , tempBoard = Set.empty
  , pattern = Pattern.blinker
  , iOffset = 0
  , jOffset = 0
  , isEraser = False
  }

boardSize = 40
noCmd = Cmd.none


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
  | SetTempToBoard Pair
  | UpdateOffsetI String
  | UpdateOffsetJ String
  | KeyMsg Keyboard.KeyCode
  | SetEraser


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (case msg of
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
      { model | isEraser = True }
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
        _ ->
          model
  , noCmd)


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
  div [class "container clearfix"]
    [ stylesheet "life.css"
    , stylesheet "slider.css"
    , pageHeader
    , drawBoard model
    , controls model
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
    , eraserButton
    , offsetSliders model
    ]

eraserButton : Html Msg
eraserButton =
  button [onClick SetEraser] [text "Eraser"]

offsetSliders : Model -> Html Msg
offsetSliders model =
  div [class "offset-sliders"]
    [ offsetSlider model.iOffset UpdateOffsetI "vertical"
    , offsetSlider model.jOffset UpdateOffsetJ "horizontal"
    ]

drawBoard : Model -> Html Msg
drawBoard model =
  let
    board = model.board
    klass1 = if model.paused then "paused" else ""
    klass2 = if model.isEraser then "eraser" else ""
  in
    div [class ("board " ++ klass1 ++ " " ++ klass2)] (List.map (drawRow model) (nums (boardSize - 1)))

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
      ) (nums 4))
    ) (nums 4))
