import Html exposing (Html, div, text, span, node)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
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

type alias Model = {board: Dict (Int, Int) Bool, info: String, fullBoard : List (List (Int, Int))}


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
  = Tick Time | ToggleCell (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | info = toString newTime}, Cmd.none)
    ToggleCell cell ->
      let
        visible = not (Maybe.withDefault False (Dict.get cell model.board))
      in
        -- ({ model | board = Dict.update cell maybeReverse model.board}, Cmd.none)
        ({ model | board = Dict.insert cell visible model.board}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    div [] [stylesheet "snake.css"],
    div [] [text model.info],
    boardView model
  ]

boardView model =
  div []
    (List.map (rowView model) model.fullBoard)

rowView model row =
  div [] (List.map (cellView model) row)

cellView model cell =
  let
    i = Tuple.first cell
    j = Tuple.second cell
    visible = Maybe.withDefault False (Dict.get cell model.board)
    str = if visible then "" else "hidden"
  in
    span [class "cell", onClick (ToggleCell cell)] [
      span [class str] [text (toString (operation i j))]
    ]



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
