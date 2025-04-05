module DAWG.Data exposing (..)
import IntDict exposing (IntDict(..))
import Set exposing (Set)
import Graph exposing (Graph, NodeContext, Adjacency, NodeId)

type ExprAST
  = M (List ExprAST)
  | A (List ExprAST)
  | V Transition

type alias EdgeRecord = Graph.Edge Transition

type alias ToDawgRecord =
  { unused : Int
  , incoming : IntDict.IntDict (List (Int, Transition)) -- source nodes, keyed by destination
  , outgoing : IntDict.IntDict (Set (Int, Transition)) -- destination nodes, keyed by source
  }

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias Transition = (Char, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias Connection = Set Transition -- a Connection is a link between two nodes.
type alias Connections = IntDict.IntDict Connection
type alias Node = NodeContext () Connection -- a Node itself does not carry any data, hence the ()
type alias DAWGGraph = Graph () Connection -- finally, the complete directed acyclic word graph.
type alias DAWG =
  { graph : DAWGGraph
    {- The maximum ID-value in this DAWG graph -}
  , maxId : NodeId
  , root : NodeId
  , final : Maybe NodeId
  }

empty : DAWG
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
    , final = Nothing
    }

exprASTToRTString : ExprAST -> String
exprASTToRTString e =
  case e of
    V transition ->
      case transition of
        (ch, 0) -> String.fromChar ch
        (ch, _) -> "!" ++ String.fromChar ch
    M xs ->
      List.map exprASTToRTString xs |> String.join ""
    A xs ->
      "(" ++ (List.map exprASTToRTString xs |> String.join " + ") ++ ")"

exprASTToString : ExprAST -> String
exprASTToString e =
  case e of
    V t ->
      transitionToString t
    M xs ->
      -- "[× " ++ (List.map exprASTToString xs |> String.join ", ") ++ "]"
      List.map exprASTToString xs |> String.join ""
    A xs ->
      -- "[+ " ++ (List.map exprASTToString xs |> String.join ", ") ++ "]"
      "(" ++ (List.map exprASTToString xs |> String.join " + ") ++ ")"

exprASTsToString : List ExprAST -> String
exprASTsToString es =
  List.map exprASTToString es |> String.join "; "

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

graphToString : DAWGGraph -> String
graphToString graph =
  Graph.toString
    (\_ -> Nothing)
    (Just << connectionToString)
    graph
