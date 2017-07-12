import Html exposing (Html, div, text, node, span)
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



-- MODEL


type alias Model = { board : Dict (Int, Int) Int, turn : Int, winner : Bool }


model : Model
model =
  { board = Dict.empty , turn = -1, winner = False }

pairIsInt dict int pair =
  (Dict.get pair dict) == int

allXorO dict int list =
  List.all (pairIsInt dict int) list

isWinner board turn =
  isWinnerRow board turn || isWinnerCol board turn || isWinnerDiag board turn

isWinnerRow board turn =
  let
    rows = [
      [(0,0), (0,1), (0,2)],
      [(1,0), (1,1), (1,2)],
      [(2,0), (2,1), (2,2)]
    ]
  in
    List.any (allXorO board (Just turn)) rows

isWinnerCol board turn =
  let
    cols = [
      [(0,0), (1,0), (2,0)],
      [(0,1), (1,1), (2,1)],
      [(0,2), (1,2), (2,2)]
    ]
  in
    List.any (allXorO board (Just turn)) cols

isWinnerDiag board turn =
  let
    diags = [
      [(0,0), (1,1), (2,2)],
      [(2,0), (1,1), (0,2)]
    ]
  in
    List.any (allXorO board (Just turn)) diags


-- UPDATE


type Msg
  = CellPos (Int, Int)


update : Msg -> Model -> Model
update msg model =
  case msg of
    CellPos pos ->
      let
        newBoard = Dict.insert pos model.turn model.board
      in
        { model |
          board = newBoard,
          turn = -model.turn,
          winner = isWinner newBoard model.turn
        }



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    div [id "outer"] [stylesheet "ttt.css"],
    userMessage model,
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
  if model.winner then
    div [class "winning-message"] [text (valToXO(-model.turn) ++ " wins!")]
  else
    span [] []
