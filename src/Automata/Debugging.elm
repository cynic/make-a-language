module Automata.Debugging exposing (..)
import Automata.Data exposing (..)
import Graph exposing (Graph)

debug_log : String -> a -> a
debug_log s x =
  Debug.log s x
  -- x

debugGraph : String -> Graph a Connection -> Graph a Connection
debugGraph txt graph =
  debug_log txt (graphToString (\_ -> Nothing) graph)
  |> \_ -> graph

printAutomatonGraph : AutomatonGraph -> String
printAutomatonGraph g =
  let
    roundABit x = toFloat (round (x * 100.0)) / 100.0 
    printNode : Entity -> Maybe String
    printNode n =
      Just <|
        "@" ++ String.fromFloat (roundABit n.x) ++
        "," ++ String.fromFloat (roundABit n.y)
  in
    ("📍" ++ String.fromInt g.root ++ " " ++ graphToString printNode g.graph)

debugAutomatonGraph : String -> AutomatonGraph -> AutomatonGraph
debugAutomatonGraph txt g =
  debug_log txt (printAutomatonGraph g)
  |> \_ -> g

println : String -> a -> a
println txt x =
  debug_log txt () |> \_ -> x

debugLog : String -> (a -> b) -> a -> a
debugLog s f v =
  debug_log s (f v) |> \_ -> v
