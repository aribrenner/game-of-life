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

rows : Section
rows =
  [
    [(0,0), (0,1), (0,2)],
    [(1,0), (1,1), (1,2)],
    [(2,0), (2,1), (2,2)]
  ]

cols : Section
cols =
  [
    [(0,0), (1,0), (2,0)],
    [(0,1), (1,1), (2,1)],
    [(0,2), (1,2), (2,2)]
  ]

diags : Section
diags =
  [
    [(0,0), (1,1), (2,2)],
    [(2,0), (1,1), (0,2)]
  ]

-- MODEL
type alias Pair = (Int, Int)
type alias SubSection = (List Pair)
type alias Section = List SubSection
type alias Board = Dict Pair Int
type alias Model = {
  board : Board,
  turn : Int,
  winner : Bool,
  done : Bool,
  score : (Int, Int)
}

emptyModel : Model
emptyModel = { board = Dict.empty , turn = -1, winner = False, done = False, score = (0,0) }
model : Model
model = emptyModel

pairIsInt : Board -> Int -> Pair -> Bool
pairIsInt dict int pair =
  (Dict.get pair dict) == Just int

allXorO : Board -> Int -> SubSection -> Bool
allXorO dict int list =
  List.all (pairIsInt dict int) list

isWinner : Board -> Int -> Bool
isWinner board turn =
  List.any (\f -> f board turn) [isWinnerRow, isWinnerCol, isWinnerDiag]

isWinnerRow : Board -> Int -> Bool
isWinnerRow board turn =
  isWinnerSection board turn rows

isWinnerCol : Board -> Int -> Bool
isWinnerCol board turn =
  isWinnerSection board turn cols

isWinnerDiag : Board -> Int -> Bool
isWinnerDiag board turn =
  isWinnerSection board turn diags

isWinnerSection : Board -> Int -> Section -> Bool
isWinnerSection board turn section =
  List.any (allXorO board turn) section


-- UPDATE


type Msg
  = CellPos Pair | PlayAgain


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

tttBoard : Model -> Html Msg
tttBoard model =
  div [class "board"] (
    List.map (\n -> tttRow n model) [0, 1, 2]
  )

tttRow : Int -> Model -> Html Msg
tttRow row model =
  div [class "row"](
    List.map (\n -> tttCell model (row, n)) [0, 1, 2]
  )

tttCell : Model -> Pair -> Html Msg
tttCell model cellPos =
  case Dict.get cellPos model.board of
    Nothing ->
      span [
        class ("cell clickable open-cell pre-cell-" ++ (valToXO model.turn)),
        onClick (CellPos cellPos)
      ] []
    Just num ->
      span [class ("cell cell-" ++ (valToXO num))] []

valToXO : Int -> String
valToXO val =
  if val == -1 then "X" else "O"

userMessage : Model -> Html Msg
userMessage model =
  let
    finalMessage = if model.winner then valToXO(-model.turn) ++ " wins!" else "Draw"
  in
    div [class "gameover-message-container"][
      div [class "gameover-message"] [
        div [] [text finalMessage],
        button [onClick PlayAgain, class "clickable"] [text "play again"]
      ]
    ]

scoreboard : Pair -> Html Msg
scoreboard score =
  let
    xScore = Tuple.first score
    oScore = Tuple.second score
  in
    div [] [
      div [class ("score x-score")] [text (toString xScore)],
      div [class ("score o-score")] [text (toString oScore)]
    ]
