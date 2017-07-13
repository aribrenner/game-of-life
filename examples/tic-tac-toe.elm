import Html exposing (Html, div, text, node, span, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)

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


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

rows =
  [
    [(0,0), (0,1), (0,2)],
    [(1,0), (1,1), (1,2)],
    [(2,0), (2,1), (2,2)]
  ]

cols =
  [
    [(0,0), (1,0), (2,0)],
    [(0,1), (1,1), (2,1)],
    [(0,2), (1,2), (2,2)]
  ]

diags =
  [
    [(0,0), (1,1), (2,2)],
    [(2,0), (1,1), (0,2)]
  ]

-- MODEL


type alias Model = {
  board : Dict (Int, Int) Int,
  turn : Int,
  winner : Bool,
  done : Bool,
  score : (Int, Int)
}

emptyModel : Model
emptyModel = { board = Dict.empty , turn = -1, winner = False, done = False, score = (0,0) }
model : Model
model = emptyModel

pairIsInt dict int pair =
  (Dict.get pair dict) == int

allXorO dict int list =
  List.all (pairIsInt dict int) list

isWinner board turn =
  isWinnerRow board turn || isWinnerCol board turn || isWinnerDiag board turn

isWinnerRow board turn =
  List.any (allXorO board (Just turn)) rows

isWinnerCol board turn =
  List.any (allXorO board (Just turn)) cols

isWinnerDiag board turn =
  List.any (allXorO board (Just turn)) diags

-- isGameOver board =
--   board.s


-- UPDATE


type Msg
  = CellPos (Int, Int) | PlayAgain


update : Msg -> Model -> Model
update msg model =
  case msg of
    CellPos pos ->
      let
        newBoard = Dict.insert pos model.turn model.board
        newWinner = isWinner newBoard model.turn
        newDone = newWinner || (Dict.size newBoard == 9)
        newScore = if newWinner then updateScore(model) else model.score
      in
        { model |
          board = newBoard,
          turn = -model.turn,
          winner = newWinner,
          done = newDone,
          score = newScore
        }
    PlayAgain ->
      { emptyModel | score = model.score }

updateScore model =
  let
    t = model.turn
    xScore = Tuple.first model.score
    oScore = Tuple.second model.score
  in
    if t == 1 then (xScore, oScore + 1) else (xScore + 1, oScore)

-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    div [id "outer"] [stylesheet "ttt.css"],
    scoreboard (model.score),
    if model.done then userMessage model else span [] [],
    tttBoard model
  ]

tttBoard model =
  div [class "board"] [
    tttRow 0 model,
    tttRow 1 model,
    tttRow 2 model
  ]

tttRow row model =
  div [class "row"][
    tttCell model (row, 0), tttCell model (row, 1), tttCell model (row, 2)
  ]

tttCell model cellPos =
  let
    val = (Dict.get cellPos model.board)
  in
    case val of
      Nothing ->
        span [class "cell clickable", onClick (CellPos cellPos)] [text ("_")]
      Just num ->
        span [class "cell"] [text (valToXO num)]

valToXO val =
  if val == -1 then "X" else "O"

userMessage model =
  let
    finalMessage = if model.winner then valToXO(-model.turn) ++ " wins!" else "Draw"
  in
    div [class "gameover-message-container"][
      div [class "gameover-message"] [text finalMessage],
      button [onClick PlayAgain, class "clickable"] [text "play again"]
    ]

scoreboard score =
  let
    xScore = Tuple.first score
    oScore = Tuple.second score
  in
    div [] [
      div [] [text ("X: " ++ toString(xScore))],
      div [] [text ("O: " ++ toString(oScore))]
    ]
