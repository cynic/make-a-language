port module Ports exposing (..)
import Json.Encode as E

port saveToStorage : E.Value -> Cmd msg