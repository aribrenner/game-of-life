module Main exposing (..)

import Html exposing (Html)
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time, second)
import Pattern exposing (Pattern)
import Keyboard exposing (KeyCode)
import Types exposing (..)
import Constants exposing (..)
import Model exposing (..)
import Update exposing (update)
import View exposing (view)

main : Program Flags Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Flags = MaybeEncodedGame

init : Flags -> (Model, Cmd Msg)
init flags =
  (createModel
    (Maybe.withDefault defaults flags), noCmd)


defaults : EncodedGame
defaults =
  { board   = []
  , iOffset = boardSize // 2
  , jOffset = boardSize // 2
  , interval = second / 10
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every millisecondUpdate Tick
    , Keyboard.downs KeyMsg
    ]
