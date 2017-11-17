module Update exposing (update)
import UpdateHelper exposing (..)
import Types exposing (..)
import Constants exposing (noCmd)
import Set exposing (Set)
import Ports
import Model exposing (Model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (updatedModel msg model, getCommand msg model)

updatedModel : Msg -> Model -> Model
updatedModel msg model =
  case msg of
    ClearBoard          -> clearBoard model
    ClearTempBoard      -> clearTempBoard model
    SaveBoard           -> saveBoard model
    SetEraser           -> setEraser model
    SetPattern pattern  -> setPattern model pattern
    SetTempBoard pair   -> setTempBoard model pair
    SetTempToBoard pair -> setTempToBoard model pair
    TogglePause         -> togglePause model
    UpdateInterval str  -> updateInterval model str
    UpdateKeyPress key  -> updateKeyPress key model
    UpdateOffsetI str   -> updateOffsetI model str
    UpdateOffsetJ str   -> updateOffsetJ model str
    UpdateTick newTime  -> updateTick model newTime

getCommand : Msg -> Model -> Cmd Msg
getCommand msg model =
  case msg of
    SaveBoard ->
      Ports.saveBoard (encodeGame model)
    _ ->
      noCmd

encodeGame : Model -> EncodedGame
encodeGame model =
  { board = Set.toList model.board
  , iOffset = model.iOffset
  , jOffset = model.jOffset
  , interval = model.interval
  }
