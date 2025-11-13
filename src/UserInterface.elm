module UserInterface exposing (..)
import Html.Styled exposing
  (Html, div, text, button)
import Html.Styled.Events as HE
import Json.Decode as D
import Automata.Data exposing (..)
import Html.Styled.Attributes as HA
import Css
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes
import Automata.Data exposing (..)

type alias Msg = Main_Msg
type alias Model = Main_Model

