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

type alias Flags = { board : List Pair }

init : Flags -> (Model, Cmd Msg)
init flags =
  (createModel (Set.fromList flags.board), noCmd)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 23 Tick
    , Keyboard.downs KeyMsg
    ]
