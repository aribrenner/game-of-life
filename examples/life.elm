import Html exposing (Html, div, text, node, span, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
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
type alias Board = Dict Pair Bool


type alias Model = {
  board : Board,
  paused : Bool,
  time : Time
}

boardSize = 20

model : Model
model =
  { board = Dict.empty
  , paused = True
  , time = 0
  }


init : (Model, Cmd Msg)
init =
  (model, Cmd.none)





-- UPDATE


type Msg
  = TogglePause
  | Tick Time
  | ToggleCell Pair


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleCell pair ->
      ({ model | board = toggleCell model.board pair }, Cmd.none)
    TogglePause ->
      ({ model | paused = not model.paused }, Cmd.none)
    Tick newTime ->
      ({ model | time = newTime, board = updateBoard model.board }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

toggleCell : Board -> Pair -> Board
toggleCell board pair =
  let
    cur = Maybe.withDefault False (Dict.get pair board)
  in
    Dict.insert pair (not cur) board



updateBoard : Board -> Board
updateBoard board =
  board

nums : Int -> List Int
nums int =
  if int == 0 then
    [0]
  else
    List.append (nums (int - 1)) [int-1]

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick TogglePause ] [ text "toggle pause" ]
    , div [] [ text (toString model.paused) ]
    , div [] [ text (toString model.time) ]
    , div [] [ text (toString model.board) ]
    , drawBoard model.board
    ]


drawBoard board =
  div [class "board"] (List.map drawRow (nums boardSize))

drawRow i =
  div [class "row"] (List.map (drawCell i) (nums boardSize))

drawCell i j =
  span [class "cell", onClick (ToggleCell (i, j))] [(i, j) |> toString |> text]
