module DAWG exposing (..)
import Graph exposing (Graph, NodeContext, NodeId, Node, Adjacency)
import Set exposing (Set)
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe
import Basics.Extra exposing (..)
import IntDict
import Tuple.Extra

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
  , root : Node
  , final : Maybe Node
  }

notFinalState : Int
notFinalState = 0
isFinalState : Int
isFinalState = 1

empty : DAWG
empty =
  let
    initial =
      { node = Node 0 ()
      , incoming = IntDict.empty
      , outgoing = IntDict.empty
      }
  in
    { graph = Graph.insert initial Graph.empty
    , maxId = 0
    , root = initial
    , final = Nothing
    }

isRoot : Node -> Bool
isRoot node =
  node.node.id == 0

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
    (node.incoming)

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

isFinalNode : Node -> DAWG -> Bool
isFinalNode node dawg =
  Maybe.map (\finalNode -> node.node.id == finalNode.node.id) dawg.final
  |> Maybe.withDefault False
  -- isLeaf node &&
  --   -- check: forall connections, forall transitions, transition is final.
  --   forallConnections
  --     (forAllTransitions (\(_, isFinal) -> isFinal == 1))
  --     node.incoming

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

{-| Create a transition to a new node, returning the DAWG and the new node.
-}
createNewSuccessorNode : Transition -> Node -> DAWG -> (DAWG, Node)
createNewSuccessorNode transition currentNode dawg =
  let
    newNode =
      { node = Node (dawg.maxId + 1) ()
      , incoming = IntDict.singleton currentNode.node.id (Set.singleton transition)
      , outgoing = IntDict.empty
      }
  in
    ( { dawg
        | graph = Graph.insert newNode dawg.graph
        , maxId = dawg.maxId + 1
      }
    , newNode
    )

{-| Create a transition to a new node, returning the DAWG and the new node.
-}
createNewPrecursorNode : Transition -> NodeId -> DAWG -> (DAWG, Node)
createNewPrecursorNode transition destNode dawg =
  let
    newNode =
      { node = Node (dawg.maxId + 1) ()
      , outgoing = IntDict.singleton destNode (Set.singleton transition)
      , incoming = IntDict.empty
      }
  in
    ( { dawg
        | graph = Graph.insert newNode dawg.graph
        , maxId = dawg.maxId + 1
      }
    , newNode
    )

{-| Create a new node which is set as the official "final" node.
-}
redefineFinalTransition : Char -> Node -> DAWG -> DAWG
redefineFinalTransition ch currentNode dawg =
  let
    newFinalNode =
      { node = Node (dawg.maxId + 1) ()
      , incoming =
          IntDict.singleton currentNode.node.id (Set.singleton (ch, 1))
      , outgoing = IntDict.empty
      }
  in
  { dawg
    | graph = Graph.insert newFinalNode dawg.graph
    , maxId = dawg.maxId + 1
    , final = Just newFinalNode
  }

-- {-| Create a transition from the existing node to the final.
--     No new node is created in the process.
-- -}
-- createTransitionToFinal : Char -> Node -> DAWG -> DAWG
-- createTransitionToFinal ch currentNode dawg =
--   { dawg
--     | graph =
--         Graph.insert
--           { currentNode
--             | outgoing =
--                 IntDict.insert
--                   dawg.final
--                   (Set.singleton (ch, 1))
--                   currentNode.outgoing
--           }
--           dawg.graph
--   }

{-| If there is already a defined "final" node, create a transition
    from this node to that one.  Otherwise, create a new final node
    and transition to it.
-}
createOrUpdateTransitionToFinal : Char -> Node -> DAWG -> DAWG
createOrUpdateTransitionToFinal ch currentNode dawg =
  case dawg.final of
    Just finalNode ->
      updateTransitionToFinal ch currentNode finalNode dawg
    Nothing ->
      redefineFinalTransition ch currentNode dawg

println : String -> a -> a
println txt x =
  Debug.log txt () |> \_ -> x

{-| Internal function.  Merges a series of transitions into the graph prefix.

ASSUMPTIONS: the last Transition is terminal; all others are non-terminal.
-}
prefixMerge : List Transition -> Node -> DAWG -> DAWG
prefixMerge transitions currentNode dawg =
  case Debug.log ("When currentNode = " ++ String.fromInt currentNode.node.id ++ ", transitions") transitions of

    [] -> -- we are at the end.
      dawg

    (w, isFinal)::transitions_remaining ->
{-
  To cover:
  - 2.1.1 ✅
  - 2.1.2 ✅
  - 2.2 ✅
  - 2.3 ✅
  - 2.4.1 ✅
  - 2.4.2 ✅
  - 3.1 ✅
  - 3.2 ✅
-}
      case forwardsFollowable w currentNode dawg.graph of

        Just someNode ->
          case (isFinalNode someNode dawg, isFinal == 1) of

            (False, False) -> -- d' != d_ω; w_i is not final
              -- there is a perfect prefix match; we can continue.
              println ("[Prefix 2.4.2] Graph node #" ++ String.fromInt someNode.node.id ++ " is not final; transition is not final.  Continuing prefix-merge.")
              prefixMerge transitions_remaining someNode dawg

            (False, True) -> -- d' ≠ d_ω; w_i is final
              -- whether backwards-split, confluence, or straight, we merge from the end.
              case dawg.final of 
                Just f ->
                  println ("[Prefix 2.2/2.3/2.4.1] Graph node #" ++ String.fromInt someNode.node.id ++ " is not final; transition is final.  Go to suffix-merging with final-node #" ++ String.fromInt f.node.id)
                  mergeSuffixes (List.reverse transitions) currentNode (MergeToExistingFinal f) dawg
                Nothing ->
                  println ("[Prefix 2.2/2.3/2.4.1] Graph node #" ++ String.fromInt someNode.node.id ++ " is not final; transition is final.  Go to suffix-merging WITHOUT a defined final-node.")
                  mergeSuffixes (List.reverse transitions) currentNode (CreateNewFinal (IntDict.empty, 0)) dawg

            (True, False) -> -- d' = d_ω; w_i is not final
              println ("[Prefix 2.1.2] Graph node #" ++ String.fromInt someNode.node.id ++ " is not final; transition is final.  Go to suffix-merging WITHOUT a defined final-node.")
              mergeSuffixes
                (List.reverse transitions)
                currentNode
                (CreateNewFinal (IntDict.remove currentNode.node.id someNode.incoming, someNode.node.id))
                dawg

            (True, True) -> -- d' = d_ω; w_i is final
              println ("[Prefix 2.1.1] Graph node #" ++ String.fromInt someNode.node.id ++ " is final; transition is final.  Updating existing transition to be final & exiting unconditionally.")
              updateTransitionToFinal w currentNode someNode dawg

        Nothing -> -- there is nothing to follow forward.  Start merging from the other side.
          case dawg.final of
            Just f ->
              println ("[Prefix 3.1/3.2] No follow-forward for " ++ transitionToString (w, isFinal) ++ " exists.  Go to suffix-merging with final-node #" ++ String.fromInt f.node.id)
              mergeSuffixes (List.reverse transitions) currentNode (MergeToExistingFinal f) dawg
            Nothing ->
              println ("[Prefix 3.1/3.2] No follow-forward for " ++ transitionToString (w, isFinal) ++ " exists.  Go to suffix-merging WITHOUT a defined final-node.")
              mergeSuffixes (List.reverse transitions) currentNode (CreateNewFinal (IntDict.empty, 0)) dawg

{-| Take all the previous incoming-connections of the old final-node and
    redirect them to the new final-node.
-}
redirectNodes : (Adjacency Connection, NodeId) -> DAWG -> DAWG
redirectNodes (adjacency, oldFinalNodeId) dawg =
  { dawg
    | graph =
        IntDict.foldl
          (\sourceNodeId conn graph ->              
              Maybe.map2
                (\sourceNode finalNode ->
                    Graph.insert
                      { sourceNode
                        | outgoing =
                            IntDict.remove oldFinalNodeId sourceNode.outgoing
                            |> IntDict.insert finalNode.node.id conn
                            |> Debug.log ("Redirected Outgoing connections of " ++ String.fromInt sourceNodeId)
                      }
                      graph
                )
                (Graph.get sourceNodeId graph)
                dawg.final
              |> Maybe.withDefault graph -- this… is an error.
          )
          dawg.graph
          adjacency
  }

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

{-| Create a connection between two existing nodes, with the specified
    transition.  If such a connection already exists, it is updated with
    the specified transition.
-}
createTransitionBetween : Transition -> NodeId -> NodeId -> DAWG -> DAWG
createTransitionBetween transition from to dawg =
  { dawg
    | graph =
        Graph.update from
          (Maybe.map
            (\fromNode ->
                NodeContext
                  fromNode.node
                  fromNode.incoming
                  ( IntDict.update to
                    ( Maybe.map (Set.insert transition)
                      >> Maybe.orElseLazy (\() -> Just <| Set.singleton transition)
                    )
                    fromNode.outgoing
                  )
            )
          )
          dawg.graph
  }

type MergeSuffixesCategorisation
  = Single Node
  | Confluence (Set Node)
  | ForwardSplit (Set Node)

type MergeType
  = CreateNewFinal (Adjacency Connection, NodeId) -- (incoming nodes to redirect, old final)
  | MergeToExistingFinal Node

followSuffixes : List Transition -> Node -> Node -> DAWG -> DAWG
followSuffixes transitions prefixEnd currentNode dawg =
  case transitions of
    [] ->
      dawg
    (w, isFinal)::transitions_remaining ->
      case compatibleBackwardsFollowable currentNode (w, isFinal) dawg.graph of
        [] ->
          println ("[Suffix 3] No compatible backwards nodes from #" ++ String.fromInt currentNode.node.id ++ " for " ++ transitionToString (w, isFinal) ++ ": D_all = Ø")
          createBackwardsChain transitions currentNode.node.id prefixEnd.node.id dawg
        candidateBackwardNodes ->
          case List.partition isSingle candidateBackwardNodes of
            ([single], _) ->
              println ("[Suffix 4] Single backwards-node (#" ++ String.fromInt single.node.id ++ ") found for " ++ transitionToString (w, isFinal) ++ ".  Following back.")
              followSuffixes transitions_remaining prefixEnd single dawg
            (x::xs, _) ->
              Debug.log ("[Suffix 4] BUG! Multiple backwards nodes found for " ++ transitionToString (w, isFinal) ++ " from #" ++ String.fromInt currentNode.node.id ++ ".  Why weren't they merged?")
                (x::xs)
              |> \_ -> dawg
            ([], dcdf) ->
              Debug.log ("[Suffix 6/7] Confluence/forward-split found for backwards-split from " ++ String.fromInt currentNode.node.id ++ " via " ++ transitionToString (w, isFinal) ++ "; stopping backtrack here.")
                dcdf
              |> \_ -> createBackwardsChain transitions currentNode.node.id prefixEnd.node.id dawg

addFinalNode : DAWG -> (Node, DAWG)
addFinalNode dawg =
  let
    newFinalNode =
      { node = Node (dawg.maxId + 1) ()
      , incoming = IntDict.empty
      , outgoing = IntDict.empty
      }
  in
    ( newFinalNode
    , { dawg
        | graph = Graph.insert newFinalNode dawg.graph
        , maxId = dawg.maxId + 1
        , final = Just newFinalNode
      }
    )

createBackwardsChain : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
createBackwardsChain transitions finalNode prefixEnd dawg =
  case transitions of
    [] ->
      println ("[Suffix ??] NO TRANSITIONS?? Probably a bug! Was trying to back-chain (final=" ++ String.fromInt finalNode ++ ", initial=" ++ String.fromInt prefixEnd ++ "), now what do I do here?")
      dawg
    [firstTransition] ->
      -- for now, we will do the v.simple thing
      println ("[Suffix 3.1] Only one backwards transition; no new nodes, just a " ++ transitionToString firstTransition ++ " link from #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt finalNode)
      createTransitionBetween firstTransition prefixEnd finalNode dawg
    head::rest ->
      println ("[Suffix 3.2] More than one backwards transition; creating a precursor node before #" ++ String.fromInt finalNode)
      createNewPrecursorNode head finalNode dawg
      |> \(dawg_, successor) -> createBackwardsChain rest successor.node.id prefixEnd dawg_

mergeSuffixes : List Transition -> Node -> MergeType -> DAWG -> DAWG
mergeSuffixes transitions prefixEnd mergeType dawg =
{-
  To cover:
  - 3
  - 5
  - 6
  - 7

  lol, let's not even pretend I'm following it!!
-}
  case mergeType of
    CreateNewFinal (incoming, oldFinal) ->
      -- create a final-terminated chain going backwards, culminating at `prefixEnd`
      addFinalNode dawg
      |> \(finalnode, dawg_) ->
        println ("New \"final\" node " ++ String.fromInt finalnode.node.id ++ " added.")
        redirectNodes (incoming, oldFinal) dawg_
        |> debugDAWG ("After node redirection to newly-defined final node #" ++ String.fromInt finalnode.node.id)
        |> createBackwardsChain transitions finalnode.node.id prefixEnd.node.id

    MergeToExistingFinal finalNode ->
      followSuffixes transitions prefixEnd finalNode dawg

{-| Convenience method to add entire strings to the DAWG.
-}
-- addString : String -> DAWG -> DAWG

-- fromWords : List String -> DAWG

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
  >> String.join ""

graphToString : DAWGGraph -> String
graphToString graph =
  Graph.toString
    (\_ -> Nothing)
    (Just << connectionToString)
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
  Maybe.map
    (\(last, rest) ->
      List.map (\ch -> (ch, 0)) rest
      |> \transitions ->
        prefixMerge (transitions ++ [(last, 1)]) dawg.root dawg
    )
    (txt |> String.toList |> List.unconsLast)
  |> Maybe.withDefault dawg -- don't accept an empty-string as valid.

fromWords : List String -> DAWG
fromWords =
  List.foldl (\s a -> addString s a |> debugDAWG ("🔻 Post-insertion of '" ++ s ++ "'")) empty

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