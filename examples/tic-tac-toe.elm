import Html exposing (Html, div, text, node, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Set exposing (Set)

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


type alias Model = { board : Set (Int, Int), turn : Int }


model : Model
model =
  { board = Set.empty, turn = -1 }


-- UPDATE


type Msg
  = Reset


update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset ->
      model



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    div [id "outer"] [stylesheet "ttt.css"],
    tttBoard model
  ]

tttBoard model =
  div [class "board"] [
    tttRow model,
    tttRow model,
    tttRow model
  ]

tttRow model  =
  div [class "row"][
    tttCell model, tttCell model, tttCell model
  ]

tttCell model =
  span [class "cell"] []
