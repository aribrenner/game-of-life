import Html exposing (Html, div, text, span)
import Dict exposing (Dict)
import Time exposing (Time, second)

boardSize = 3

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model = {board: Dict (Int, Int) Int, info: String, fullBoard : List (List (Int, Int))}


init : (Model, Cmd Msg)
init =
  ({
    board = Dict.empty,
    info = "",
    fullBoard = fullBoard
    }, Cmd.none)


fullBoard : List (List (Int, Int))
fullBoard =
  buildBoard (boardSize - 1)
--
--
buildBoard : Int -> List (List (Int, Int))
buildBoard cur =
  let
    miniBoard = [fullRow (boardSize - 1) cur]
  in
    if cur == 0 then miniBoard else List.append miniBoard (buildBoard (cur - 1))

fullRow : Int -> Int -> List (Int, Int)
fullRow i j =
  let
    pair = [(i, j)]
  in
    if i == 0 then pair else List.append pair (fullRow (i-1) j)

operation : Int -> Int -> Int
operation i j =
  i * j

-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | info = toString newTime}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    div [] [text model.info],
    boardView model.fullBoard
  ]

boardView board =
  div []
    (List.map rowView board)

rowView row =
  div [] (List.map cellView row)

cellView cell =
  let
    i = Tuple.first cell
    j = Tuple.second cell
  in
    span [] [text (toString (operation i j))]
