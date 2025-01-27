module DAWG exposing (..)
import Graph exposing (Graph, NodeContext, NodeId, Node, Adjacency)
import Set exposing (Set)
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe
import Basics.Extra exposing (..)
import IntDict

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias Transition = (Char, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias Connection = Set Transition
type alias Node = NodeContext () Connection
type alias DAWGGraph = Graph () Connection
type alias DAWG =
  { graph : DAWGGraph
    {- The maximum ID-value in this DAWG graph -}
  , maxId : NodeId
  , root : NodeId
  , final : NodeId
  }

notFinalState : Int
notFinalState = 0
isFinalState : Int
isFinalState = 1

empty : DAWG
empty =
  { graph = Graph.fromNodesAndEdges [Node 0 ()] []
  , maxId = 0
  , root = 0
  , final = 0
  }

isEmpty : DAWG -> Bool
isEmpty d =
  d.maxId == 0

isConfluence : Connection -> Bool
isConfluence connection =
  Set.size connection > 1

isConfluenceConnection : Node -> Node -> Bool
isConfluenceConnection node1 node2 =
  IntDict.get node2.node.id node1.outgoing
  |> Maybe.map isConfluence
  |> Maybe.withDefault False

isForwardSplit : Node -> Bool
isForwardSplit node =
  IntDict.size node.outgoing > 1

isBackwardSplit : Node -> Bool
isBackwardSplit node =
  IntDict.size node.incoming > 1

isLeaf : Node -> Bool
isLeaf node =
  IntDict.size node.outgoing == 0

{-| True if at least one transition terminates at this node -}
isTerminalNode : Node -> Bool
isTerminalNode node =
  IntDict.foldl
    (\_ conn state ->
      state || 
        Set.foldl
          (\(_, isFinal) state_ -> state_ || isFinal == 1)
          False
          conn
    )
    False
    node.incoming

isSingle : Node -> Bool
isSingle node =
  IntDict.size node.outgoing == 1 &&
    List.all (Set.size >> (==) 1) (IntDict.values node.outgoing)

forallConnections : (Connection -> Bool) -> Adjacency Connection -> Bool
forallConnections predicate adjacent =
  IntDict.foldl
    (\_ conn (state, seenOne) ->
      ( (not seenOne || state) && predicate conn
      , True
      )
    )
    (False, False)
    adjacent
  |> Tuple.first

forAllTransitions : (Transition -> Bool) -> Connection -> Bool
forAllTransitions predicate connection =
  Set.foldl
    (\transition (state, seenOne) ->
      ( (not seenOne || state) && predicate transition
      , True
      )
    )
    (False, False)
    connection
  |> Tuple.first

isFinalNode : Node -> Bool
isFinalNode node =
  isLeaf node &&
    -- check: forall connections, forall transitions, transition is final.
    forallConnections
      (forAllTransitions (\(_, isFinal) -> isFinal == 1))
      node.incoming

forwardsFollowable : Char -> Node -> DAWGGraph -> Maybe Node
forwardsFollowable ch node graph =
  IntDict.foldl
    (\k conn state ->
      state
      |> Maybe.orElseLazy
        (\() ->
          if Set.member (ch, 0) conn || Set.member (ch, 1) conn then
            Just k
          else
            Nothing
        )
    )
    Nothing
    node.outgoing
  |> Maybe.andThen (flip Graph.get graph)

backwardsFollowable : Node -> Char -> DAWGGraph -> List Node
backwardsFollowable node ch graph =
  IntDict.foldl
    (\k conn state ->
      if Set.member (ch, 0) conn || Set.member (ch, 1) conn then
        k :: state
      else
        state
    )
    []
    node.outgoing
  |> List.filterMap (flip Graph.get graph)

compatibleBackwardsFollowable : Node -> Transition -> DAWGGraph -> List Node
compatibleBackwardsFollowable node transition graph =
  IntDict.foldl
    (\k conn state ->
      if Set.member transition conn then
        k :: state
      else
        state
    )
    []
    node.outgoing
  |> List.filterMap (flip Graph.get graph)

{--
  DAWG-modification functions
-}

type alias SuffixMergeData =
  { prefixEnd : Node
  , remainingTransitions : List Transition
  }

type alias PrefixMergeResult =
  { dawg : DAWG -- with a new Final node, if necessary.
  , suffixMergeData : Maybe SuffixMergeData
    -- the nodes to redirect, and the node-id of the original "final" node.
  , redirectNodes : Maybe (Adjacency Connection, NodeId)
  }

startingPrefixMergeResult : DAWG -> PrefixMergeResult
startingPrefixMergeResult dawg =
  { dawg = dawg
  , suffixMergeData = Nothing
  , redirectNodes = Nothing
  }

updateTransitionToFinal : Char -> Node -> Node -> DAWG -> DAWG
updateTransitionToFinal ch currentNode finalNode dawg =
  { dawg
    | graph =
        Graph.insert
          { currentNode
            | outgoing = 
                IntDict.update finalNode.node.id
                  -- I should not need to remove beforehand.  This node is
                  -- final ($d_ω$), so all transitions to it should be terminal.
                  (Maybe.map <| Set.insert (ch, 1))
                  currentNode.outgoing
          }
          dawg.graph
  }

-- Create a non-final transition to a new node, returning the DAWG and the new node.
createNewTransitionReturningNode : Char -> Node -> DAWG -> (DAWG, Node)
createNewTransitionReturningNode ch currentNode dawg =
  let
    newNode =
      { node = Node (dawg.maxId + 1) ()
      , incoming = IntDict.singleton currentNode.node.id (Set.singleton (ch, 0))
      , outgoing = IntDict.empty
      }
  in
    ( { dawg
        | graph = Graph.insert newNode dawg.graph
        , maxId = dawg.maxId + 1
      }
    , newNode
    )

-- Create a new node which will then become the official "final" node.
createNewFinalTransition : Char -> Node -> DAWG -> DAWG
createNewFinalTransition ch currentNode dawg =
  { dawg
    | graph =
        Graph.insert
          { currentNode
            | outgoing =
                IntDict.singleton
                  (dawg.maxId + 1)
                  (Set.singleton (ch, 1))
          }
          dawg.graph
    , maxId = dawg.maxId + 1
    , final = dawg.maxId + 1
  }

{-| Create a transition from the existing node to the final.
    No new node is created in the process.
-}
createTransitionToFinal : Char -> Node -> DAWG -> DAWG
createTransitionToFinal ch currentNode dawg =
  { dawg
    | graph =
        Graph.insert
          { currentNode
            | outgoing =
                IntDict.insert
                  dawg.final
                  (Set.singleton (ch, 1))
                  currentNode.outgoing
          }
          dawg.graph
  }

{-| Internal function.  Merges a series of transitions into the graph prefix.

ASSUMPTIONS: the last Transition is terminal; all others are non-terminal.
-}
prefixMerge : List Transition -> Node -> PrefixMergeResult -> PrefixMergeResult
prefixMerge transitions currentNode accumulator =
  case transitions of
    [] -> -- we are at the end.
      accumulator
    (w, isFinal)::transitions_remaining ->
      case forwardsFollowable w currentNode accumulator.dawg.graph of
        Just someNode ->
          if isFinalNode someNode then
            if isFinal == 1 then
              { accumulator
                | dawg = updateTransitionToFinal w currentNode someNode accumulator.dawg
              }
            else
              prefixMerge
                transitions_remaining
                someNode
                { accumulator
                  | redirectNodes = Just (someNode.incoming, someNode.node.id)
                }
          else if isBackwardSplit someNode || isConfluenceConnection currentNode someNode then
            { accumulator
              | suffixMergeData =
                  Just <|
                    { prefixEnd = currentNode
                    , remainingTransitions = List.reverse transitions
                    }
            }
          else
            prefixMerge transitions_remaining someNode accumulator
        Nothing ->
          if isFinal == 1 then
            case accumulator.redirectNodes of
              Just _ ->
                { accumulator
                  | dawg = createNewFinalTransition w currentNode accumulator.dawg
                }
              Nothing ->
                { accumulator
                  | dawg = createTransitionToFinal w currentNode accumulator.dawg
                }
          else
            let
              (updated_dawg, newNode) =
                createNewTransitionReturningNode w currentNode accumulator.dawg
            in
              prefixMerge
                transitions_remaining
                newNode
                { accumulator | dawg = updated_dawg }

{-| Take all the previous incoming-connections of the old final-node and
    redirect them to the new final-node.
-}
redirectNodes : (Adjacency Connection, NodeId) -> DAWG -> DAWG
redirectNodes (adjacency, oldFinalNodeId) dawg =
  { dawg
    | graph =
        IntDict.foldl
          (\sourceNodeId conn graph ->
              Graph.get sourceNodeId graph
              |> Maybe.map
                  (\sourceNode ->
                      Graph.insert
                        { sourceNode
                          | outgoing =
                              IntDict.remove oldFinalNodeId sourceNode.outgoing
                              |> IntDict.insert dawg.final conn
                        }
                        graph
                    )
              |> Maybe.withDefault graph
          )
          dawg.graph
          adjacency
  }

postPrefixCleanup : PrefixMergeResult -> DAWG
postPrefixCleanup prefixResult =
  let
    redirected =
      case prefixResult.redirectNodes of
        Nothing ->
          prefixResult
        Just redirection ->
          { prefixResult
            | dawg = redirectNodes redirection prefixResult.dawg
            , redirectNodes = Nothing
          }
  in
    case redirected.suffixMergeData of
      Nothing ->
        redirected.dawg
      Just suffixMergeData ->
        Maybe.map
          (\finalNode ->
            mergeSuffixes
              suffixMergeData.remainingTransitions
              suffixMergeData.prefixEnd
              finalNode
              redirected.dawg
          )
          (Graph.get redirected.dawg.final redirected.dawg.graph)
        |> Maybe.withDefault redirected.dawg -- this indicates a problem in the algorithm—it should never happen!

outgoingConnectionBetween : Node -> Node -> Maybe Connection
outgoingConnectionBetween prefixEnd joinPoint =
  IntDict.get joinPoint.node.id prefixEnd.outgoing

appendTransitionToOutgoingConnection : Transition -> Node -> Node -> Connection -> DAWG -> DAWG
appendTransitionToOutgoingConnection transition prefixEnd joinPoint conn dawg =
  { dawg
    | graph =
        Graph.insert
          { prefixEnd
            | outgoing =
                IntDict.insert
                  joinPoint.node.id
                  (Set.insert transition conn)
                  prefixEnd.outgoing
          }
          dawg.graph
  }

-- "from" = prefixEnd
-- "to" = joinPoint
createTransitionBetween : Transition -> Node -> Node -> DAWG -> DAWG
createTransitionBetween transition prefixEnd joinPoint dawg =
  { dawg
    | graph =
        Graph.insert
          { prefixEnd
            | outgoing =
                IntDict.insert
                  joinPoint.node.id
                  (Set.singleton transition)
                  prefixEnd.outgoing
          }
          dawg.graph
  }

createJoiningTransitionChain : List Transition -> Node -> Node -> DAWG -> DAWG
createJoiningTransitionChain transitions prefixEnd joinPoint dawg =
  -- Remember that `transitions` are in reverse order.
  transitions
  |> List.unconsLast
  |> Maybe.map
      (\(firstTransition, otherTransitions) ->
          List.foldl
            (\transition (successor, dawg_) ->
              -- create a new node
              let
                newNode =
                  { node = Node (dawg_.maxId + 1) ()
                  , incoming = IntDict.empty
                  , outgoing = IntDict.singleton successor.node.id (Set.singleton transition)
                  }
              in
                ( newNode
                , { dawg_
                    | graph = Graph.insert newNode dawg_.graph
                    , maxId = dawg_.maxId + 1
                  }
                )
            )
            (joinPoint, dawg)
            otherTransitions
          |> \(firstInChain, dawg_) ->
              { dawg_
                | graph =
                    Graph.insert
                      { prefixEnd
                        | outgoing =
                            IntDict.insert
                              firstInChain.node.id
                              (Set.singleton firstTransition)
                              prefixEnd.outgoing
                      }
                      dawg_.graph
              }
      )
  |> Maybe.withDefault dawg -- this indicates an error has occurred!!

subProcedure : List Transition -> Node -> Node -> DAWG -> DAWG
subProcedure transitions_remaining prefixEnd joinPoint dawg =
  case transitions_remaining of
    [] ->
      dawg -- I shouldn't be here, algorithmically speaking!!
      -- But if I am… then, there is nothing to do??
    [transition] ->
      case outgoingConnectionBetween prefixEnd joinPoint of
        Just conn ->
          appendTransitionToOutgoingConnection transition prefixEnd joinPoint conn dawg
        Nothing ->
          createTransitionBetween transition prefixEnd joinPoint dawg
    transitions ->
      createJoiningTransitionChain transitions prefixEnd joinPoint dawg

mergeSuffixes : List Transition -> Node -> Node -> DAWG -> DAWG
mergeSuffixes transitions prefixEnd currentNode dawg =
  case transitions of
    [] ->
      dawg
    (w, isFinal)::transitions_remaining ->
      case compatibleBackwardsFollowable currentNode (w, isFinal) dawg.graph of
        [] ->
          subProcedure transitions prefixEnd currentNode dawg
        candidateBackwardNodes ->
          case List.partition isSingle candidateBackwardNodes of
            ([single], _) ->
              mergeSuffixes transitions_remaining prefixEnd single dawg
            _ ->
              subProcedure transitions prefixEnd currentNode dawg


{-| Convenience method to add entire strings to the DAWG.
-}
-- addString : String -> DAWG -> DAWG

-- fromWords : List String -> DAWG

graphToString : DAWGGraph -> String
graphToString graph =
  Graph.toString
    (\_ -> Nothing)
    (\connections ->
      Set.map
        (\element ->
          case element of
            (ch, 0) ->
              String.fromChar ch
            (ch, _) ->
              "(" ++ String.fromChar ch ++ ")"
        )
        connections
      |> Set.toList
      |> String.join ""
      |> Just
    )
    graph

debugGraph : String -> DAWGGraph -> DAWGGraph
debugGraph txt graph =
  Debug.log txt (graphToString graph)
  |> \_ -> graph

debugDAWG : String -> DAWG -> DAWG
debugDAWG txt dawg =
  Debug.log txt
    (graphToString dawg.graph)
  |> \_ -> dawg

{--
  User-facing functions
--}

addString : String -> DAWG -> DAWG
addString txt dawg =
  Maybe.map2
    (\(last, rest) rootNode ->
      List.map (\ch -> (ch, 0)) rest
      |> \transitions ->
        prefixMerge (transitions ++ [(last, 1)]) rootNode (startingPrefixMergeResult dawg)
        |> postPrefixCleanup
    )
    (txt |> String.toList |> List.unconsLast)
    (Graph.get dawg.root dawg.graph)
  |> Maybe.withDefault dawg -- don't accept an empty-string as valid.


numNodes : DAWG -> Int
numNodes dawg =
  Graph.size dawg.graph

-- wordVisitor : List Node -> Int -> List String -> List String
-- wordVisitor path_to_root _ words =
--   -- I _think_ the most recent element will be the one that is
--   -- first on path_to_root.  Then we follow it back to the earliest
--   -- element.
--   case path_to_root of
--     [] -> words
--     last::path_remaining ->
--       path_to_connections path_to_root
--       |> List.filterMap terminal_transitions_of_connection
--       |> 


-- recognizableWords : DAWG -> List String
-- recognizableWords dawg =
--   Graph.bfs wordVisitor [] dawg.graph