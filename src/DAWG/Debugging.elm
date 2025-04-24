module DAWG.Debugging exposing (..)
import DAWG.Data exposing (..)
import Set
import Graph

debug_log : String -> a -> a
debug_log s x =
  Debug.log s x
  -- x

debugGraph : String -> DAWGGraph -> DAWGGraph
debugGraph txt graph =
  debug_log txt (graphToString graph)
  |> \_ -> graph

debugDAWG : String -> DAWG -> DAWG
debugDAWG txt dawg =
  debug_log txt
    (graphToString dawg.graph)
  |> \_ -> dawg

println : String -> a -> a
println txt x =
  debug_log txt () |> \_ -> x

debugLog : String -> (a -> b) -> a -> a
debugLog s f v =
  debug_log s (f v) |> \_ -> v
