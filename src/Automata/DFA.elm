module Automata.DFA exposing (..)
import IntDict exposing (IntDict)
import Set exposing (Set)
import List.Extra as List
import Dict exposing (Dict)
import Automata.Data exposing (..)
import Graph exposing (Graph, NodeContext, Node, NodeId, Edge)
import Automata.Debugging

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias MTransition = Char
type alias MConnection = Set MTransition -- a MConnection is a link between two nodes.
-- type alias Connections = IntDict MConnection
type alias Node = NodeContext Bool MConnection -- a Node indicates terminality
type alias FAGraph = Graph Bool MConnection -- finally, the Finite Automaton.
type alias RegisterValue = ( Int, List ( NodeId, List MTransition ) ) -- (isFinal, list-of-outgoing)

{- So with an DFA, I can basically do anything that's deterministic.

Looping back to previous nodes? Go ahead.
Looping back to ourselves? Definitely!

Now with that said, nodes without outgoing edges SHOULD be terminal nodes.
And everything is connected.  And outgoing nodes have deterministic transitions.
-}

type alias NodeId = Int
type alias ATransition = ( (NodeId, Char), NodeId )

type alias DFARecord =
  { states : IntDict () -- the () is the label.
  , transition_function: IntDict (Dict Char NodeId) -- NodeId × Char → NodeId
  , start : NodeId
  , finals : Set NodeId
  }

create : List (NodeId, ()) -> List (NodeId, Char, NodeId) -> NodeId -> Set NodeId -> DFARecord
create nodes edges start finals =
  let
    states = IntDict.fromList nodes
  in
  { states = states
  , transition_function =
      List.foldl
        (\(from, transition, to) state ->
            if IntDict.member from states && IntDict.member to states then
              IntDict.update from
                (\possibly ->
                  case possibly of
                    Nothing ->
                      -- first entry, that's fun.
                      Just (Dict.singleton transition to)
                    Just dict ->
                      case Dict.get transition dict of
                        Nothing ->
                          -- first transition leading from here, okay.
                          Just (Dict.insert transition to dict)
                        Just _ ->
                          -- this would be deterministic.  Instead, overwrite.
                          Debug.log ("[create] Overwriting transition " ++ String.fromChar transition ++ " from " ++ String.fromInt from ++ " to " ++ String.fromInt to ++ ".") () |> \_ ->
                          Just (Dict.insert transition to dict)
                )
                state
            else
              Debug.log ("[create] Skipping edge " ++ String.fromInt from ++ " → " ++ String.fromInt to ++ ", because of non-existence.")
                (IntDict.member from states, IntDict.member to states) |> \_ ->
              state
        )
        IntDict.empty
        edges
  , start = start
  , finals = finals
  }

toGraph : DFARecord -> AutomatonGraph
toGraph dfa =
  let
    stateList = IntDict.toList dfa.states |> List.reverse
    graph =
      Graph.fromNodesAndEdges
        (stateList |> List.map (\(id, label) -> Node id label))
        (IntDict.toList dfa.transition_function
        |> List.foldl
          (\(from, dict) state ->
            Dict.toList dict
            |> List.foldl
              (\(transition, to) state_ ->
                let
                  t = if Set.member to dfa.finals then (transition, 1) else (transition, 0)
                in
                case Dict.get (from, to) state_ of
                  Nothing ->
                    Dict.insert (from, to) (Set.singleton t) state_
                  Just conn ->
                    Dict.insert (from, to) (Set.insert t conn) state_
              )
              state
          )
          Dict.empty
        |> Dict.toList
        |> List.map (\((from, to), set) -> Edge from to set)
        )
  in
    case stateList of
      [] ->
        Automata.Data.empty
      h::_ ->
        { graph = graph
        , maxId = Tuple.first h
        , root = dfa.start
        }

transitionToString : MTransition -> String
transitionToString =
  String.fromChar

connectionToString : MConnection -> String
connectionToString =
  Set.map transitionToString
  >> Set.toList
  >> String.concat