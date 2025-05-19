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
type alias Node a = NodeContext a Connection -- a Node itself does not carry any data, hence the ()
type alias AutomatonGraph a =
  { graph : Graph a Connection -- finally, the complete graph.
    {- The maximum ID-value in this Automaton graph -}
  , maxId : NodeId
  , root : NodeId
  }

empty : AutomatonGraph a
empty =
    { graph = Graph.empty
    , maxId = 0
    , root = 0
    }

{-| True if at least one transition terminates at this node -}
isTerminalNode : NodeContext a Connection -> Bool
isTerminalNode node =
  -- IntDict.isEmpty node.outgoing &&
  ( IntDict.foldl
    (\_ conn state ->
      state ||
        Set.foldl
          (\(_, isFinal) state_ -> state_ || isFinal == 1)
          False
          conn
    )
    False
    (node.incoming)
  )

isTerminal : NodeId -> Graph x Connection -> Bool
isTerminal id graph =
  Graph.get id graph
  |> Maybe.map isTerminalNode
  |> Maybe.withDefault False

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

graphToString : Graph a Connection -> String
graphToString graph =
  Graph.toString
    (\_ -> Nothing)
    (Just << connectionToString)
    graph
