module Main exposing (..)

import Html exposing (Html)
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time, second)
import Pattern exposing (..)
import Keyboard exposing (KeyCode)
import Types exposing (..)
import Constants exposing (..)
import Helpers exposing (..)
import Update exposing (update)
import View exposing (view)

main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : Flags -> (Model, Cmd Msg)
init flags =
  (createModel (Set.fromList flags.board), noCmd)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 23 Tick
    , Keyboard.downs KeyMsg
    ]
