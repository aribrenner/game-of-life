import Html exposing (Html, div, input, text, img, span, button, label, node)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Char
import Debug exposing (log)
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
intList int =
  if int == 0 then [0] else List.append [int] (intList (int-1))

bits = intList 6 -- [6, 5, 4, 3, 2, 1, 0]

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


-- VIEW
toggleVal int vals =
  if Set.member int vals then
    Set.remove int vals
  else
    Set.insert int vals


getVal vals =
  let
    f int =
      pow2thing int vals
  in
    List.sum(List.map f bits)


pow2thing int vals =
  (if Set.member int vals then 2 ^ int else 0)

view : Model -> Html Msg
view model =
  div []
  [
    div [id "outer"] [stylesheet "style.css"],
    viewChars model,
    checkboxes,
    div [] [text (toString(getVal model.vals))],
    binaryNumber model ,
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

viewChars : Model -> Html Msg
viewChars model =
    let
      chars = model.chars
    in
      if List.isEmpty chars then
        div [class "full-string", style [("background", "gray"), ("color", "white")]]
            [text "_"]
      else
        div [class "full-string", style [("background", "red")]]
            (List.map intToSpan chars)


checkbox : Int -> Html Msg
checkbox int =
  label
    [ style [("padding", "2px"), ("border", "1px solid red")]
    ]
    [ span [] [text (toString (2 ^ int))],
      input [ type_ "checkbox", onClick (Change2 int) ] []
    ]

checkboxes =
  div []
  (List.map checkbox bits)

binaryNumber model =
  div [] (List.map (binaryNumberSpan model) bits)

binaryNumberSpan model int =
  let
    string = if (Set.member int model.vals) then "1" else "0"
  in
    span [] [text string]
