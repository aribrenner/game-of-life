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


type alias Model = { board : Dict (Int, Int) Int, turn : Int }


model : Model
model =
  { board = Dict.empty , turn = -1 }


-- UPDATE


type Msg
  = CellPos (Int, Int)


update : Msg -> Model -> Model
update msg model =
  case msg of
    CellPos pos ->
      {
        model | board = Dict.insert pos model.turn model.board,
        turn = -model.turn
      }



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    div [id "outer"] [stylesheet "ttt.css"],
    div [] [text (toString model.board)],
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
    val = Dict.get cellPos model.board
  in
    span [class "cell", onClick (CellPos cellPos)] [text (toString val)]
