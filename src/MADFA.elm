module MADFA exposing (..)
import Automata.Data exposing (..)
import Automata.Debugging exposing (..)
import Automata.MADFA as MADFA
{--
  User-facing functions (and a few helpers thereof)
--}

fromWords : List String -> AutomatonGraph
fromWords =
  MADFA.fromWords