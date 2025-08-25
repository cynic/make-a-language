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

printAutomatonGraph : AutomatonGraph a -> String
printAutomatonGraph g =
  ("📍" ++ String.fromInt g.root ++ " " ++ graphToString (\_ -> Nothing) g.graph)

printEntityGraph : AutomatonGraph Entity -> String
printEntityGraph g =
  let
    roundABit x = toFloat (round (x * 100.0)) / 100.0 
    printNode : Entity -> Maybe String
    printNode n =
      Just <|
        "#" ++ String.fromInt n.id ++
        "@" ++ String.fromFloat (roundABit n.x) ++
        "," ++ String.fromFloat (roundABit n.y)
  in
    ("📍" ++ String.fromInt g.root ++ " " ++ graphToString printNode g.graph)

debugEntityGraph : String -> AutomatonGraph Entity -> AutomatonGraph Entity
debugEntityGraph txt g =
  debug_log txt (printEntityGraph g)
  |> \_ -> g

debugAutomatonGraph : String -> AutomatonGraph a -> AutomatonGraph a
debugAutomatonGraph txt g =
  debug_log txt (printAutomatonGraph g)
  |> \_ -> g

println : String -> a -> a
println txt x =
  debug_log txt () |> \_ -> x

debugLog : String -> (a -> b) -> a -> a
debugLog s f v =
  debug_log s (f v) |> \_ -> v
