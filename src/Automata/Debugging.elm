module Automata.Debugging exposing (..)
import Automata.Data exposing (..)
import Graph exposing (Graph)
import IntDict

debug_log : String -> a -> a
debug_log s x =
  Debug.log s x
  -- x

debugGraph : String -> Graph a Connection -> Graph a Connection
debugGraph txt graph =
  debug_log txt (graphToString (\_ -> Nothing) graph)
  |> \_ -> graph

printFan : IntDict.IntDict Connection -> String
printFan fan =
  IntDict.toList fan
  |> List.map
    (\(k, v) -> String.fromInt k ++ " (" ++ connectionToString v ++ ")")
  |> String.join " | "

printNodeContext : Graph.NodeContext Entity Connection -> String
printNodeContext {node, incoming, outgoing} =
  "[" ++ printFan incoming ++ "]→" ++ String.fromInt node.id ++ "→[" ++ printFan outgoing ++ "]"

printAutomatonGraph : AutomatonGraph -> String
printAutomatonGraph g =
  let
    -- roundABit x = toFloat (round (x * 100.0)) / 100.0 
    printNode : Entity -> Maybe String
    printNode _ =
      Nothing
      -- Just <|
      --   "@" ++ String.fromFloat (roundABit n.x) ++
      --   "," ++ String.fromFloat (roundABit n.y)
  in
    ("📍" ++ String.fromInt g.root ++ " " ++ graphToString printNode g.graph)

debugAutomatonGraph : String -> AutomatonGraph -> AutomatonGraph
debugAutomatonGraph txt g =
  debug_log (txt ++ "\n  ") (printAutomatonGraph g)
  |> \_ -> g

println : String -> a -> a
println txt x =
  debug_log txt () |> \_ -> x

-- debugLog : String -> (a -> b) -> a -> a
-- debugLog s f v =
--   debug_log s (f v) |> \_ -> v

debugLog : (a -> String) -> a -> a
debugLog f a =
  Debug.log (f a) () |> \_ -> a

debugLog_ : String -> (a -> b) -> a -> a
debugLog_ s f a =
  let
    transformed = f a
  in
    Debug.log s transformed |> \_ -> a