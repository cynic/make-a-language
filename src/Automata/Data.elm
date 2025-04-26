module Automata.Data exposing (..)
import IntDict exposing (IntDict(..))
import Set exposing (Set)
import Graph exposing (Graph, NodeContext, NodeId)

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias Transition = (Char, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias Connection = Set Transition -- a Connection is a link between two nodes.
type alias Node = NodeContext () Connection -- a Node itself does not carry any data, hence the ()
type alias AutomatonGraph =
  { graph : Graph () Connection -- finally, the complete graph.
    {- The maximum ID-value in this Automaton graph -}
  , maxId : NodeId
  , root : NodeId
  }

empty : AutomatonGraph
empty =
  let
    initial =
      { node = Graph.Node 0 ()
      , incoming = IntDict.empty
      , outgoing = IntDict.empty
      }
  in
    { graph = Graph.insert initial Graph.empty
    , maxId = 0
    , root = 0
    }

graphEdgeToString : Graph.Edge Connection -> String
graphEdgeToString {from, to, label} =
  "#" ++ String.fromInt from ++ "➜#" ++ String.fromInt to ++ " (" ++ connectionToString label ++ ")"

transitionsToString : List Transition -> String
transitionsToString transitions =
  String.join "," (List.map transitionToString transitions)

transitionToString : Transition -> String
transitionToString transition =
  case transition of
    (ch, 0) ->
      String.fromChar ch
    (ch, _) ->
      "\u{0307}" ++ String.fromChar ch

connectionToString : Connection -> String
connectionToString =
  Set.map transitionToString
  >> Set.toList
  >> String.join "\u{FEFF}" -- zero-width space. Stops terminality-marker from disappearing on subsequent characters.

graphToString : Graph () Connection -> String
graphToString graph =
  Graph.toString
    (\_ -> Nothing)
    (Just << connectionToString)
    graph
