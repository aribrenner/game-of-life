import Html exposing (Html, div, input, text, img, span, button, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Char
import Debug exposing (log)
import Array


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
    bits : (Int, Int, Int, Int, Int, Int, Int, Int)
  }


model : Model
model =
  { chars = [], lastChar = 0, bits = (0, 0, 0, 0, 0, 0, 0, 0) }



-- UPDATE

type Msg
  = Change String | Change1 | Change2 Int | Change3 Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | lastChar =
        String.toInt newContent
        |> Result.toMaybe
        |> Maybe.withDefault 0
      }
    Change1 ->
      { model | chars = List.append model.chars [model.lastChar] }
    Change2 int ->
      { model | chars = List.append model.chars [int] }
    Change3 int ->
      { model | bits = (0, 0, 0, 0, 0, 0, 0, 0) }


-- VIEW

shit i =
  case i of
    f ->
      { model | chars = List.append model.chars [i] }

view : Model -> Html Msg
view model =
  div []
  [
    viewSingleChar model ,
    checkbox "" 0,
    checkbox "" 1,
    checkbox "" 2,
    input [ attribute "type" "number", placeholder "Ascii key", onInput Change ] [],
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
