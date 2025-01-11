-- create default skeleton
module Main exposing (..)
import Html as H exposing (Html)
import Graph exposing (Graph)
import Platform.Cmd as Cmd
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Browser
import Html.Attributes as HA
import Html.Events as HE
import Dict exposing (Dict)
import Json.Encode as E
import Json.Decode as D
import Time

type alias Model = ()


type alias Message = ()


update : Message -> Model -> (Model, Cmd Message)
update msg model = 
  (model, Cmd.none)



view : Model -> Html Message
view model =
  H.div
    []
    []



subscriptions : Model -> Sub Message
subscriptions model =
  Sub.none



main : Program () Model Message
main =
  Browser.element
    { init =
        \_ ->
          ( ()
          , Cmd.none
          )
    , update = update
    , view = view
    , subscriptions = subscriptions
    }