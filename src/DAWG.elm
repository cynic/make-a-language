module DAWG exposing (..)
import Graph exposing (Graph, NodeContext, NodeId, Node, Adjacency)
import Set exposing (Set)
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe
import Basics.Extra exposing (..)
import IntDict
import Set exposing (Set)
import Result.Extra

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias Transition = (Char, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias Connection = Set Transition -- a Connection is a link between two nodes.
type alias Node = NodeContext () Connection -- a Node itself does not carry any data, hence the ()
type alias DAWGGraph = Graph () Connection -- finally, the complete directed acyclic word graph.
type alias DAWG =
  { graph : DAWGGraph
    {- The maximum ID-value in this DAWG graph -}
  , maxId : NodeId
  , root : NodeId
  , final : Maybe NodeId
  }

notFinalState : Int
notFinalState = 0
isFinalState : Int
isFinalState = 1

isRoot : Node -> Bool
isRoot node =
  node.node.id == 0

isConfluence : Connection -> Bool
isConfluence connection =
  Set.size connection > 1

isConfluenceConnection : NodeId -> NodeId -> DAWG -> Bool
isConfluenceConnection node1 node2 dawg =
  Maybe.map2
    (\a b ->
      IntDict.get b.node.id a.outgoing
      |> Maybe.map isConfluence
      |> Maybe.withDefault False
    )
    (Graph.get node1 dawg.graph)
    (Graph.get node2 dawg.graph)
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

isFinalNode : NodeId -> DAWG -> Bool
isFinalNode nodeid dawg =
  dawg.final
  |> Maybe.andThen (flip Graph.get dawg.graph)
  |> Maybe.map (\finalNode -> nodeid == finalNode.node.id)
  |> Maybe.withDefault False

forwardsFollowable : Char -> NodeId -> DAWGGraph -> Maybe Node
forwardsFollowable ch nodeid graph =
  Maybe.andThen
    (\node ->
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
    )
    (Graph.get nodeid graph)

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
    node.incoming
  |> List.filterMap (flip Graph.get graph)

compatibleBackwardsFollowable : NodeId -> Transition -> DAWGGraph -> List Node
compatibleBackwardsFollowable nodeid transition graph =
  Maybe.map
    ( .incoming
      >> IntDict.foldl
        (\k conn state ->
          if Set.member transition conn then
            k :: state
          else
            state
        )
        []
      >> List.filterMap (flip Graph.get graph)
    )
    (Graph.get nodeid graph)
  |> Maybe.withDefault []
{--
  DAWG-modification functions
-}

removeTransitionFromConnection : Transition -> Connection -> Maybe Connection
removeTransitionFromConnection transition connection =
  let
    s = Set.remove transition connection
  in
    if Set.isEmpty s then Nothing
    else Just s

addTransitionToConnection : Transition -> Connection -> Connection
addTransitionToConnection transition connection =
  Set.insert transition connection

updateConnectionWith : Transition -> Connection -> Connection
updateConnectionWith transition conn =
  case transition of
    (ch, 1) ->
      addTransitionToConnection transition conn
      |> Set.remove (ch, 0)
    (ch, _) ->
      if Set.member (ch, 1) conn then
        conn
      else
        addTransitionToConnection transition conn

{-| Merge connections, returning the merged connection.  When a
    transition character occurs in both with different terminal status,
    the transition with the terminal value is preserved.
-}
mergeConnectionWith : Connection -> Connection -> Connection
mergeConnectionWith =
  Set.foldl updateConnectionWith

createOrUpdateOutgoingConnection : Transition -> Node -> NodeId -> IntDict.IntDict Connection
createOrUpdateOutgoingConnection transition from to =
  IntDict.update to
    ( Maybe.map (updateConnectionWith transition)
      >> Maybe.orElseLazy (\() -> Just (Set.singleton transition))
    )
    from.outgoing

createOrUpdateIncomingConnection : Transition -> NodeId -> Node -> IntDict.IntDict Connection
createOrUpdateIncomingConnection transition from to =
  IntDict.update from
    ( Maybe.map (updateConnectionWith transition)
      >> Maybe.orElseLazy (\() -> Just (Set.singleton transition))
    )
    to.incoming

createOrMergeOutgoingConnections : IntDict.IntDict Connection -> IntDict.IntDict Connection -> IntDict.IntDict Connection
createOrMergeOutgoingConnections a b =
  Debug.log "[createOrMergeOutgoingConnections] a" a
  |> \_ -> Debug.log "[createOrMergeOutgoingConnections] b" b
  |> \_ -> IntDict.uniteWith (\_ -> mergeConnectionWith) a b

cloneAndMergeOutgoingConnectionsOfNode : NodeId -> NodeId -> DAWG -> DAWG
cloneAndMergeOutgoingConnectionsOfNode from to dawg = -- from = dr, to = ds
  let
    fromOutgoing_ = Graph.get from dawg.graph |> Maybe.map .outgoing
  in
    Maybe.map
      (\fromOutgoing ->
        { dawg
          | graph =
              Graph.update to
                (Maybe.map
                  (\toNode -> { toNode | outgoing = createOrMergeOutgoingConnections fromOutgoing toNode.outgoing })
                )
                dawg.graph
        }
      )
      fromOutgoing_
    |> Maybe.withDefault dawg

{-| Connect to a particular node with a particular transition, returning
    the updated `from` node

  This is a fairly high-level function. It will
  - create the connection if it needs to, and update it otherwise.
  - ensure that if the transition is final, there isn't a competing
    non-final transition.
-}
connectTo : NodeId -> Transition -> Node -> Node
connectTo to transition from =
  { from
    | outgoing = createOrUpdateOutgoingConnection transition from to
  }

{-| Connect from a particular node with a particular transition, returning
    the updated `to` node

  This is a fairly high-level function. It will
  - create the connection if it needs to, and update it otherwise.
  - ensure that if the transition is final, there isn't a competing
    non-final transition.
-}
connectFrom : NodeId -> Transition -> Node -> Node
connectFrom from transition to =
  { to
    | incoming = createOrUpdateIncomingConnection transition from to
  }


disconnectFrom : NodeId -> Node -> Node
disconnectFrom to from =
  { from
    | outgoing = IntDict.remove to from.outgoing
  }

obtainConnectionFrom : NodeId -> Node -> Maybe (Node, NodeId, Connection)
obtainConnectionFrom old from =
  from.outgoing
  |> IntDict.get old
  |> Maybe.map (\conn -> (from, old, conn))

redirectConnectionTo : NodeId -> Maybe (Node, NodeId, Connection) -> Maybe Node
redirectConnectionTo to maybeRedirect =
  maybeRedirect
  |> Maybe.map
    (\(from, old, conn) ->
        { from
          | outgoing =
              IntDict.remove old from.outgoing
              |> IntDict.insert to conn
        }
    )

mergeConnectionTo : NodeId -> Maybe (Node, NodeId, Connection) -> Maybe Node
mergeConnectionTo to maybeRedirect =
  maybeRedirect
  |> Maybe.map
    (\(from, _, conn) ->
        { from
          | outgoing =
              IntDict.uniteWith
                (\_ a b -> Set.foldl (updateConnectionWith) a b)
                (IntDict.singleton to conn)
                from.outgoing
        }
    )

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

{-| Internal function.  Merges a series of transitions into the graph prefix.

ASSUMPTIONS: the last Transition is terminal; all others are non-terminal.
-}
prefixMerge : List Transition -> NodeId -> DAWG -> DAWG
prefixMerge transitions currentNode dawg =
  case Debug.log ("When currentNode = " ++ String.fromInt currentNode ++ ", transitions") transitions of

    [] -> -- we are at the end.
      dawg

    (w, isFinal)::transitions_remaining ->
{-
  To cover:
  - 2.1 ✅
  - 2.2.1 ✅
  - 2.2.2 ✅
  - 2.2.3 ✅
  - 3.1 ✅
  - 3.2 ✅
-}
      case forwardsFollowable w currentNode dawg.graph of

        Just someNode ->
          if isFinal == 1 then
            println ("[Prefix 2.1.1] Created/updated terminal transition to #" ++ String.fromInt someNode.node.id ++ ".  Updating existing transition to be final & exiting unconditionally.")
            createTransitionBetween (w, 1) currentNode someNode.node.id dawg
          else
            if isFinalNode someNode.node.id dawg then
              println ("[Prefix 2.2.1] Graph node #" ++ String.fromInt someNode.node.id ++ " is final.  Go to suffix-merging WITHOUT a defined final-node.")
              mergeSuffixes
                (List.reverse transitions_remaining)
                someNode.node.id
                (CreateNewFinal (IntDict.remove currentNode someNode.incoming, someNode.node.id))
                dawg
            else if isBackwardSplit someNode || isConfluenceConnection currentNode someNode.node.id dawg then
              case dawg.final of
                Just f ->
                  println ("[Prefix 2.2.2] Graph node #" ++ String.fromInt someNode.node.id ++ " is backward-split, or the #" ++ String.fromInt currentNode ++ " → #" ++ String.fromInt someNode.node.id ++ " connection is a confluence.  Going to suffix-merging with final-node #" ++ String.fromInt f)
                  mergeSuffixes (List.reverse transitions) currentNode (MergeToExistingFinal f) dawg
                Nothing ->
                  println ("[Prefix 2.2.2] Graph node #" ++ String.fromInt someNode.node.id ++ " is backward-split, or the #" ++ String.fromInt currentNode ++ " → #" ++ String.fromInt someNode.node.id ++ " connection is a confluence.  Going to suffix-merging WITHOUT a defined final-node.")
                  mergeSuffixes (List.reverse transitions) currentNode (CreateNewFinal (IntDict.empty, 0)) dawg
            else
              println ("[Prefix 2.2.3] Graph node #" ++ String.fromInt someNode.node.id ++ " is single.  Continuing prefix-merge.")
              prefixMerge transitions_remaining someNode.node.id dawg
        Nothing -> -- there is nothing to follow forward.  Start merging from the other side.
          case dawg.final of
            Just f ->
              println ("[Prefix 3.1/3.2] No follow-forward for " ++ transitionToString (w, isFinal) ++ " exists.  Go to suffix-merging with final-node #" ++ String.fromInt f)
              mergeSuffixes (List.reverse transitions) currentNode (MergeToExistingFinal f) dawg
            Nothing ->
              println ("[Prefix 3.1/3.2] No follow-forward for " ++ transitionToString (w, isFinal) ++ " exists.  Go to suffix-merging WITHOUT a defined final-node.")
              mergeSuffixes (List.reverse transitions) currentNode (CreateNewFinal (IntDict.empty, 0)) dawg

mergeNodes : (Adjacency Connection, NodeId) -> NodeId -> DAWG -> DAWG
mergeNodes (adjacency, oldDestination) newDestination dawg =
  { dawg
    | graph =
        IntDict.foldl
          (\sourceNodeId _ graph ->
              Graph.update sourceNodeId
                (Maybe.andThen (obtainConnectionFrom oldDestination >> mergeConnectionTo newDestination))
                graph
          )
          dawg.graph
          adjacency
  }

redirectNodes : (Adjacency Connection, NodeId) -> NodeId -> DAWG -> DAWG
redirectNodes (adjacency, oldDestination) newDestination dawg =
  { dawg
    | graph =
        IntDict.foldl
          (\sourceNodeId _ graph ->
              Graph.update sourceNodeId
                (Maybe.andThen (obtainConnectionFrom oldDestination >> redirectConnectionTo newDestination))
                graph
          )
          dawg.graph
          adjacency
  }

{-| Take all the previous incoming-connections of the old final-node and
    redirect them to the new final-node.
-}
redirectNodesToFinal : (Adjacency Connection, NodeId) -> DAWG -> DAWG
redirectNodesToFinal redirection dawg =
  dawg.final
  |> Maybe.map (flip (redirectNodes redirection) dawg)
  |> Maybe.withDefault dawg

{-| Create a connection between two existing nodes, with the specified
    transition.  If such a connection already exists, it is updated with
    the specified transition.
-}
createTransitionBetween : Transition -> NodeId -> NodeId -> DAWG -> DAWG
createTransitionBetween transition from to dawg =
  { dawg
    | graph =
        Graph.update from
          (Maybe.map (connectTo to transition))
          dawg.graph
  }

type MergeType
  = CreateNewFinal (Adjacency Connection, NodeId) -- (incoming nodes to redirect, old final)
  | MergeToExistingFinal NodeId

createConfluence : NodeId -> NodeId -> DAWG -> DAWG
createConfluence danglingid actualid dawg =
  Maybe.map
    (\dangling ->
      mergeNodes (dangling.incoming, dangling.node.id) actualid dawg
    )
    (Graph.get danglingid dawg.graph)
  |> Maybe.withDefault dawg

isDangling : NodeId -> DAWG -> Bool
isDangling nodeid dawg =
  Graph.get nodeid dawg.graph
  |> Maybe.map isLeaf
  |> Maybe.withDefault False

followSuffixes : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
followSuffixes transitions prefixEnd currentNode dawg =
  case Debug.log ("[Suffix 2] Suffixes to follow from #" ++ String.fromInt currentNode ++ ", moving back towards #" ++ String.fromInt prefixEnd) transitions of
    [] ->
      dawg
    (w, isFinal)::transitions_remaining ->
      case compatibleBackwardsFollowable currentNode (w, isFinal) dawg.graph of
        [] ->
          if currentNode == prefixEnd && not (List.isEmpty transitions_remaining) then
            createBackwardsChain transitions currentNode prefixEnd dawg
          else
            println ("[Suffix 2.1.2] No compatible backwards nodes from #" ++ String.fromInt currentNode ++ " for " ++ transitionToString (w, isFinal) ++ ": D_all = Ø")
            createBackwardsChain transitions currentNode prefixEnd dawg
        candidateBackwardNodes ->
          case List.partition isSingle candidateBackwardNodes of
            ([single], _) ->
              case transitions_remaining of
                [] ->
                  if prefixEnd == single.node.id then
                    println ("[Suffix 3.1] No more transitions to follow; ending as expected at prefix-node #" ++ String.fromInt prefixEnd ++ ".")
                    dawg
                  else if isDangling prefixEnd dawg then
                    println ("[Suffix 3.2.1] Dangling prefix-node #" ++ String.fromInt prefixEnd ++ " detected (current-node = #" ++ String.fromInt single.node.id ++ ").  Creating confluence.")
                    createConfluence prefixEnd single.node.id dawg
                    |> \dawg_ -> { dawg_ | graph = Graph.remove prefixEnd dawg_.graph }
                  else
                    -- in other words, it's NOT the end but it's also NOT dangling
                    Debug.log "single" single |> \_ ->
                    println ("[Suffix 3.2.2] Shorter word than graph (back = #" ++ String.fromInt single.node.id ++ ").  Connecting #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt currentNode ++ ".")
                    createTransitionBetween (w, isFinal) prefixEnd currentNode dawg
                _ ->
                  if prefixEnd == single.node.id then
                    println ("[Suffix 2.1.1] Word is longer than graph; we must add in nodes.")
                    createBackwardsChain transitions currentNode prefixEnd dawg
                  else
                    println ("[Suffix 2.2.1] Single backwards-node (#" ++ String.fromInt single.node.id ++ ") found for " ++ transitionToString (w, isFinal) ++ ".  Following back.")
                    followSuffixes transitions_remaining prefixEnd single.node.id dawg
            (x::xs, _) ->
              Debug.log ("[Suffix] BUG! 👽 Multiple backwards nodes found for " ++ transitionToString (w, isFinal) ++ " from #" ++ String.fromInt currentNode ++ ".  Why weren't they merged?")
                (x::xs)
              |> \_ -> dawg
            ([], dcdf) ->
              Debug.log ("[Suffix 2.2.2] Confluence/forward-split found for backwards-split from " ++ String.fromInt currentNode ++ " via " ++ transitionToString (w, isFinal) ++ "; stopping backtrack here.")
                dcdf
              |> \_ -> createBackwardsChain transitions currentNode prefixEnd dawg

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
        , final = Just <| dawg.maxId + 1
      }
    )

{-| Find the outgoing node from the given node for a given transition-character. -}
outgoingConnectionWith : Char -> NodeId -> DAWG -> Maybe (Node, Transition)
outgoingConnectionWith w nodeid dawg =
  Graph.get nodeid dawg.graph
  |> Maybe.andThen
    (\node ->
      IntDict.filter
        (\_ v -> Set.member (w, 0) v || Set.member (w, 1) v)
        node.outgoing
      |> IntDict.toList
      |> List.filterMap
        (\(k, conn) ->
          Graph.get k dawg.graph
          |> Maybe.map (\existing -> (existing, if Set.member (w, 0) conn then (w, 0) else (w, 1)))
        )
      |> List.head
    )

{-|If `dc_id`→`dp_id` is a confluence, split it such that that transition
  is removed from `dc_id` and connected to a new node.  The new node is then
  returned along with the updated DAWG.

  If `dc_id`→`dp_id` is not a confluence, then `(dp_id, dawg)` is returned.
-}
splitConfluence : Transition -> NodeId -> NodeId -> DAWG -> (NodeId, DAWG)
splitConfluence transition dc_id dp_id dawg =
  if isConfluenceConnection dc_id dp_id dawg then
    Maybe.map
      (\dc ->
        createNewSuccessorNode transition dc dawg
        |>  \(dawg_, dp) ->
              { dc
                | outgoing =
                    IntDict.update
                      dp_id
                      (Maybe.andThen <| removeTransitionFromConnection transition)
                      dc.outgoing
              }
              |> connectTo dp.node.id transition
              |> \updated_node -> (dp.node.id, { dawg_ | graph = Graph.insert updated_node dawg_.graph })
      )
      (Graph.get dc_id dawg.graph)
    |> Maybe.withDefault (dp_id, dawg)
  else
    (dp_id, dawg)

createBackwardsChain : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
createBackwardsChain transitions finalNode prefixEnd dawg =
  case transitions of
    [] ->
      println ("[Suffix-Chain 1] NO TRANSITIONS?? Probably a bug! Was trying to back-chain (final=" ++ String.fromInt finalNode ++ ", initial=" ++ String.fromInt prefixEnd ++ "), now what do I do here?")
      dawg
    [firstTransition] ->
      -- for now, we will do the v.simple thing
      println ("[Suffix-Chain 2] Only one backwards transition; no new nodes, just a " ++ transitionToString firstTransition ++ " link from #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt finalNode)
      createTransitionBetween firstTransition prefixEnd finalNode dawg
      -- hmmm.  But… before this link is made, we've done the redirection.
      -- And because we've done the redirection, we SHOULD be able to 
    head::rest ->
      println ("[Suffix-Chain 2.1] More than one backwards transition; creating a precursor node before #" ++ String.fromInt finalNode)
      createNewPrecursorNode head finalNode dawg
      |> \(dawg_, successor) ->
        println ("[Suffix-Chain 2.2] Created precursor node #" ++ String.fromInt successor.node.id ++ "; will continue to build chain.")
        createBackwardsChain rest successor.node.id prefixEnd dawg_

mergeSuffixes : List Transition -> NodeId -> MergeType -> DAWG -> DAWG
mergeSuffixes transitions prefixEnd mergeType dawg =
  case mergeType of
    CreateNewFinal (incoming, oldFinal) ->
      -- create a final-terminated chain going backwards, culminating at `prefixEnd`
      println ("[Suffix 1] Creating new final, then merging back to prefix-node #" ++ String.fromInt prefixEnd)
      addFinalNode dawg
      |> \(finalnode, dawg_) ->
        dawg_
        |> debugDAWG ("[Suffix 1] New \"final\" node #" ++ String.fromInt finalnode.node.id ++ " added.  Before redirecting       ")
        |> redirectNodesToFinal (incoming, oldFinal)
        |> debugDAWG ("[Suffix 1] After node redirection to newly-defined final node #" ++ String.fromInt finalnode.node.id)
        |> mergeSuffixes transitions prefixEnd (MergeToExistingFinal finalnode.node.id)
        --|> createBackwardsChain transitions finalnode.node.id prefixEnd

    MergeToExistingFinal finalNode ->
      println ("[Suffix 2] Using final #" ++ String.fromInt finalNode ++ ", merging back to prefix-node #" ++ String.fromInt prefixEnd)
      followSuffixes transitions prefixEnd finalNode dawg

{--
  Output/debugging functions
--}

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

println : String -> a -> a
println txt x =
  Debug.log txt () |> \_ -> x

{--
  User-facing functions (and a few helpers thereof)
--}

wordToTransitions : String -> List Transition
wordToTransitions txt =
  Maybe.map
    (\(last, rest) ->
      List.map (\ch -> (ch, 0)) rest
      |> \transitions -> transitions ++ [(last, 1)]
    )
    (txt |> String.toList |> List.unconsLast)
  |> Maybe.withDefault [] -- don't accept an empty-string as valid.

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
    , root = 0
    , final = Nothing
    }

isEmpty : DAWG -> Bool
isEmpty d =
  d.maxId == 0

addString : String -> DAWG -> DAWG
addString txt dawg =
  wordToTransitions txt
  |> \transitions -> prefixMerge transitions dawg.root dawg

fromWords : List String -> DAWG
fromWords =
  List.foldl (\s a -> addString s a |> debugDAWG ("🔻 Post-insertion of '" ++ s ++ "'")) empty

numNodes : DAWG -> Int
numNodes dawg =
  Graph.size dawg.graph

numEdges : DAWG -> Int
numEdges dawg =
  List.length <| Graph.edges dawg.graph

explore : Node -> String -> DAWGGraph -> List (Node, String, Bool)
explore node s graph =
  node.outgoing
  |> IntDict.map
      (\k conn ->
          Graph.get k graph
          |> Maybe.map
              (\outnode ->
                  Set.toList conn
                  |> List.map
                    (\(ch, isFinal) ->
                        (outnode, s ++ String.fromChar ch, isFinal == 1)
                    )
              )
      )
  |> IntDict.values
  |> List.filterMap identity
  |> List.concat

processStack : List (Node, String, Bool) -> List String -> DAWGGraph -> List String
processStack stack acc graph =
  case stack of
    [] -> acc
    (n, s, f)::rest ->
      processStack
        (explore n s graph ++ rest)
        (if f then s::acc else acc)
        graph

recognizedWordsFrom : DAWG -> Node -> Result String (List String)
recognizedWordsFrom dawg root =
  case Graph.checkAcyclic dawg.graph of
    Err edge ->
      Err <| "Edge " ++ String.fromInt edge.from ++ "→" ++ String.fromInt edge.to ++ " creates a cycle; this is not a DAWG."
    Ok _ ->
      Ok <| processStack [(root, "", False)] [] dawg.graph

-- Entry point function
recognizedWords : DAWG -> List String
recognizedWords dawg =
  Maybe.map
    (recognizedWordsFrom dawg >> Result.map List.sort >> Result.mapError identity)
    (Graph.get dawg.root dawg.graph)
  |> Maybe.withDefault (Err "Couldn't find the root in the DAWG…!  What on earth is going on?!")
  |> Result.Extra.extract (\e -> [e])