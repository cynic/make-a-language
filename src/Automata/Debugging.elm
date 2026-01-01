module Automata.Debugging exposing (..)
import Automata.Data exposing (..)
import Graph exposing (Graph)
import Html.Styled exposing (div, Html, text)
import Html.Styled.Attributes as HA
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
    ("📍" ++ String.fromInt g.root ++ " " ++ graphToString (\_ -> Nothing) g.graph)

printAutomatonGraphWithXY : AutomatonGraph -> String
printAutomatonGraphWithXY g =
  let
    roundABit x = toFloat (round (x * 100.0)) / 100.0 
    printNode : Entity -> Maybe String
    printNode {x, y} =
      Just <|
        "@" ++ String.fromFloat (roundABit x) ++
        "," ++ String.fromFloat (roundABit y)
  in
    ("📍" ++ String.fromInt g.root ++ " " ++ graphToString printNode g.graph)

debugAutomatonGraph : String -> AutomatonGraph -> AutomatonGraph
debugAutomatonGraph txt g =
  debug_log (txt ++ "\n  ") (printAutomatonGraph g)
  |> \_ -> g

debugAutomatonGraphXY : String -> AutomatonGraph -> AutomatonGraph
debugAutomatonGraphXY txt g =
  debug_log (txt ++ "\n  ") (printAutomatonGraphWithXY g)
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

debugViewDimensions : Bool
debugViewDimensions = True

debugElement : String -> String -> Html a
debugElement otherClass s =
  if debugViewDimensions then
    div
      [ HA.class "debug dimensions-parent" ]
      [ div
        [ HA.class <| "dimensions " ++ otherClass ]
        [ text s ]
      ]
  else
    div
      []
      []

debugDimensions : Dimension -> Html a
debugDimensions {w, h} =
  debugElement "width height" (String.fromFloat w ++ "×" ++ String.fromFloat h)

debugHeight : Float -> Html a
debugHeight h =
  debugElement "height" (String.fromFloat h)

debugWidth : Float -> Html a
debugWidth w =
  debugElement "width" (String.fromFloat w)
