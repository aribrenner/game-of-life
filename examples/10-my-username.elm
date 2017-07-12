import Html exposing (Html, div, input, text, img, span, button, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Char
import Debug exposing (log)
import Set exposing (Set)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { chars : List Int,
    lastChar : Int,
    vals : Set Int
  }


model : Model
model =
  { chars = [], lastChar = 0, vals = Set.empty }



-- UPDATE

type Msg
  = Change1 | Change2 Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change1 ->
      { model | chars = List.append model.chars [model.lastChar] }
    Change2 int ->
      let
        newVals = toggleVal int model.vals
      in
        { model | vals = newVals, lastChar = getVal newVals }

-- valToChar int =


-- VIEW
toggleVal int vals =
  if Set.member int vals then
    Set.remove int vals
  else
    Set.insert int vals

getVal vals =
  pow2thing 0 vals +
  pow2thing 1 vals +
  pow2thing 2 vals +
  pow2thing 3 vals +
  pow2thing 4 vals +
  pow2thing 5 vals +
  pow2thing 6 vals



pow2thing int vals =
  (if Set.member int vals then 2 ^ int else 0)

view : Model -> Html Msg
view model =
  div []
  [
    viewSingleChar model ,
    div [] [text (toString(getVal model.vals))],
    checkbox "" 6,
    checkbox "" 5,
    checkbox "" 4,
    checkbox "" 3,
    checkbox "" 2,
    checkbox "" 1,
    checkbox "" 0,
    button [onClick Change1] [text "click me"],
    div [] [text ("(" ++ intToString(model.lastChar) ++ ")")],
    div [] [],
    img [ attribute "src" "http://www.asciitable.com/index/asciifull.gif" ] []
  ]

intToSpan : Int -> Html Msg
intToSpan int =
  span [] [ text (intToString int) ]


intToString : Int -> String
intToString int =
  String.fromChar(Char.fromCode int)

viewSingleChar : Model -> Html Msg
viewSingleChar model =
    div []
    (List.map intToSpan model.chars)


checkbox : String -> Int -> Html Msg
checkbox name int =
  label
    [ style [("padding", "2px")]
    ]
    [ input [ type_ "checkbox", onClick (Change2 int) ] []
    , text name
    ]
