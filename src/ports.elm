port module Ports exposing (saveBoard)
import Types exposing(EncodedGame)

port saveBoard : EncodedGame -> Cmd msg
