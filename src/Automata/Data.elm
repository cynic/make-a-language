module Automata.Data exposing (..)
import IntDict exposing (IntDict(..))
import Set exposing (Set)
import Graph exposing (Graph, NodeContext, NodeId)
import Dict exposing (Dict)

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

{- So with an DFA, I can basically do anything that's deterministic.

Looping back to previous nodes? Go ahead.
Looping back to ourselves? Definitely!

Now with that said, nodes without outgoing edges SHOULD be terminal nodes.
And everything is connected.  And outgoing nodes have deterministic transitions.
-}

type alias DFARecord extending label =
  { extending |
    states : IntDict label -- the () is the label.
  , transition_function: IntDict (Dict Char NodeId) -- NodeId × Char → NodeId
  , start : NodeId
  , finals : Set NodeId
  }

type alias ExtDFA label =
  { states : IntDict label
  , transition_function: IntDict (Dict Char NodeId)
  , start : NodeId
  , finals : Set NodeId
  , register : Set NodeId
  , clone_start : NodeId
  , queue_or_clone : List NodeId
  , unusedId : NodeId
  }
type alias ExecutionData =
  { transitionsTaken : List (Char, Int)
  , remainingData : List Char
  , currentNode : NodeId
  }

type ExecutionState
  = Accepted ExecutionData
  | Rejected ExecutionData
  | RequestedNodeDoesNotExist ExecutionData -- this is an internal error
  | NoPossibleTransition ExecutionData

type ExecutionResult
  = EndOfInput ExecutionState
  | EndOfComputation ExecutionState
  | CanContinue ExecutionState
  | InternalError

empty : AutomatonGraph a
empty =
    { graph = Graph.empty
    , maxId = 0
    , root = 0
    }

graphToAutomatonGraph : NodeId -> Graph a Connection -> AutomatonGraph a
graphToAutomatonGraph start graph =
  { graph = graph
  , maxId = Graph.nodeIdRange graph |> Maybe.map Tuple.second |> Maybe.withDefault 0
  , root = start
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
  >> String.join "\u{2008}" -- punctuation space. Stops terminality-marker from disappearing on subsequent characters.

graphToString : Graph a Connection -> String
graphToString graph =
  Graph.toString
    (\_ -> Nothing)
    (Just << connectionToString)
    graph
