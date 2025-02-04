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

isBackwardSplit : NodeId -> DAWG -> Bool
isBackwardSplit nodeid dawg =
  Graph.get nodeid dawg.graph
  |> Maybe.map (\node -> IntDict.size node.incoming > 1)
  |> Maybe.withDefault False

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

dawgUpdate : NodeId -> (Node -> Node) -> DAWG -> DAWG
dawgUpdate nodeid f dawg =
  { dawg
    | graph = Graph.update nodeid (Maybe.map f) dawg.graph
  }

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

createOrUpdateIncomingConnection : Transition -> NodeId -> Node -> Connections
createOrUpdateIncomingConnection transition from to =
  IntDict.update from
    ( Maybe.map (updateConnectionWith transition)
      >> Maybe.orElseLazy (\() -> Just (Set.singleton transition))
    )
    to.incoming

createOrMergeOutgoingConnections : Connections -> Connections -> Connections
createOrMergeOutgoingConnections a b =
  IntDict.uniteWith (\_ -> mergeConnectionWith) a b

cloneAndMergeOutgoingConnectionsOfNode : NodeId -> NodeId -> DAWG -> DAWG
cloneAndMergeOutgoingConnectionsOfNode from to dawg = -- from = dr, to = ds
  let
    fromOutgoing_ = Graph.get from dawg.graph |> Maybe.map .outgoing
  in
    Maybe.map
      (\fromOutgoing ->
        dawgUpdate to
          (\toNode -> { toNode | outgoing = createOrMergeOutgoingConnections fromOutgoing toNode.outgoing })
          dawg
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
createNewSuccessorNode : Transition -> NodeId -> DAWG -> (DAWG, NodeId)
createNewSuccessorNode transition srcNode dawg =
  let
    newNode =
      { node = Node (dawg.maxId + 1) ()
      , incoming = IntDict.singleton srcNode (Set.singleton transition)
      , outgoing = IntDict.empty
      }
  in
    ( { dawg
        | graph = Graph.insert newNode dawg.graph
        , maxId = dawg.maxId + 1
      }
    , newNode.node.id
    )

{-| Create a transition to a new node, returning the DAWG and the new node.
-}
createNewPrecursorNode : Transition -> NodeId -> DAWG -> (DAWG, NodeId)
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
    , newNode.node.id
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
            if isBackwardSplit someNode.node.id dawg || isConfluenceConnection currentNode someNode.node.id dawg then
              -- check for this BEFORE final, or we'll be caught by x-y-xx
              case dawg.final of
                Just f ->
                  println ("[Prefix 2.2.2] Graph node #" ++ String.fromInt someNode.node.id ++ " is backward-split, or the #" ++ String.fromInt currentNode ++ " → #" ++ String.fromInt someNode.node.id ++ " connection is a confluence.  Going to suffix-merging with final-node #" ++ String.fromInt f)
                  mergeSuffixes (List.reverse transitions) currentNode (MergeToExistingFinal f) dawg
                Nothing ->
                  println ("[Prefix 2.2.2] Graph node #" ++ String.fromInt someNode.node.id ++ " is backward-split, or the #" ++ String.fromInt currentNode ++ " → #" ++ String.fromInt someNode.node.id ++ " connection is a confluence.  Going to suffix-merging WITHOUT a defined final-node.")
                  mergeSuffixes (List.reverse transitions) currentNode (CreateNewFinal (IntDict.empty, 0)) dawg
            else if isFinalNode someNode.node.id dawg then
              println ("[Prefix 2.2.1] Graph node #" ++ String.fromInt someNode.node.id ++ " is final.  Go to suffix-merging WITHOUT a defined final-node.")
              mergeSuffixes
                (List.reverse transitions_remaining)
                someNode.node.id
                (CreateNewFinal (IntDict.remove currentNode someNode.incoming, someNode.node.id))
                dawg
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
createTransitionBetween transition from to =
  dawgUpdate from (connectTo to transition)

{-| Unconditionally creates a chain, using all the specified transitions and
    creating nodes as necessary, that begins at `from` and terminates at `to`.
-}
createTransitionChainBetween : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
createTransitionChainBetween transitions from to dawg =
  case transitions of
    [] ->
      dawg
    [t] ->
      createTransitionBetween t from to dawg
    _ -> -- we know that `rest` is NOT [] here; otherwise, it'd be caught at [t]
      Maybe.map2
        (\(lastTransition, rest) fromNode ->
          List.foldl
            (\t (dawg_, prevNode) ->
              createNewSuccessorNode t prevNode dawg_
            )
            (dawg, fromNode.node.id)
            rest
          |> \(dawg_, prevNode) -> createTransitionBetween lastTransition prevNode to dawg_
        )
        (List.unconsLast transitions)
        (Graph.get from dawg.graph)
      |> Maybe.withDefaultLazy (\() -> debugDAWG "👽 BUG!! 👽 in createTransitionChainBetween." dawg)

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
            createChain (List.reverse transitions) prefixEnd currentNode dawg
          else
            println ("[Suffix 2.1.2] No compatible backwards nodes from #" ++ String.fromInt currentNode ++ " for " ++ transitionToString (w, isFinal) ++ ": D_all = Ø")
            createChain (List.reverse transitions) prefixEnd currentNode dawg
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
                    createChain (List.reverse transitions) prefixEnd currentNode dawg
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
              |> \_ -> createChain (List.reverse transitions) prefixEnd currentNode dawg

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

joinToConfluenceOrBackwardSplit : Transition -> NodeId -> NodeId -> Node -> DAWG -> DAWG
joinToConfluenceOrBackwardSplit (w, isFinal) dp_id ds_id dr dawg =
  println ("[Suffix-Chain 2.2] Confluence/backward-split join: transition = " ++ transitionToString (w, isFinal) ++ ", dp = #" ++ String.fromInt dp_id ++ ", ds = #" ++ String.fromInt ds_id ++ ", dr = #" ++ String.fromInt dr.node.id)
  createTransitionBetween (w, isFinal) dp_id ds_id dawg
  |> debugDAWG ("[Suffix-Chain 2.2.2] Added transition " ++ transitionToString (w, isFinal) ++ " from #" ++ String.fromInt dp_id ++ " to #" ++ String.fromInt ds_id)
  |> cloneAndMergeOutgoingConnectionsOfNode dr.node.id dp_id
  |> debugDAWG ("[Suffix-Chain 2.2.3] Created/updated #" ++ String.fromInt ds_id ++ " with connections from #" ++ String.fromInt dr.node.id)

maxTerminality : Transition -> Transition -> Transition
maxTerminality (ch, t0) (_, t1) =
  if t0 > t1 then (ch, t0) else (ch, t1)

transitionMatching : Char -> Connection -> Bool
transitionMatching ch connection =
  Set.member (ch, 0) connection || Set.member (ch, 1) connection

getConnection : NodeId -> NodeId -> DAWG -> Maybe Connection
getConnection from to dawg =
  Graph.get from dawg.graph
  |> Maybe.andThen (\{ outgoing } -> IntDict.get to outgoing)

{-| This is a SET of the Connection; it is NOT a MERGE!! -}
setConnection : NodeId -> NodeId -> Connection -> DAWG -> DAWG
setConnection from to conn =
  dawgUpdate from (\node -> { node | outgoing = IntDict.insert to conn node.outgoing })

popCharFromConnection : Char -> Connection -> Maybe (Connection, Transition)
popCharFromConnection ch conn =
  if Set.member (ch, 0) conn then
    Just (Set.remove (ch, 0) conn, (ch, 0))
  else if Set.member (ch, 1) conn then
    Just (Set.remove (ch, 1) conn, (ch, 1))
  else
    Nothing

{-|Splits a confluence connection, returning the max-terminal transition
   popped off, and a DAWG with that connection updated.

   If you pass in something that is NOT a confluence, bad things might happen?
-}
-- splitConfluence : Transition -> NodeId -> NodeId -> DAWG -> DAWG
-- splitConfluence (w, isFinal) from to dawg =
--   getConnection from to dawg
--   |> Maybe.andThen (popCharFromConnection w)
--   |> Maybe.map
--     (\(poppedConn, transition) ->
--       ( maxTerminality (w, isFinal) transition
--       , setConnection from to poppedConn dawg
--       )
--     )
--   |> Maybe.withDefaultLazy (\() -> Debug.log "👽 BUG 👽 in splitConfluence?" ((w, isFinal), dawg))

disconnectBackwardSplit : Transition -> NodeId -> NodeId -> DAWG -> (Transition, DAWG)
disconnectBackwardSplit transition from to dawg =
  Maybe.andThen
    (\{ incoming } ->
        IntDict.get from incoming |> Maybe.andThen (Set.toList >> List.head)
        |> Maybe.map
          (\t ->
            ( maxTerminality transition t
            , dawgUpdate from (\node -> { node | incoming = IntDict.remove from node.incoming }) dawg
            )
          )
    )
    (Graph.get to dawg.graph)
  |> Maybe.withDefaultLazy (\() -> Debug.log "👽 BUG 👽 in disconnectBackwardSplit?" (transition, dawg))

duplicateOutgoingConnectionsExcluding : NodeId -> NodeId -> NodeId -> DAWG -> DAWG
duplicateOutgoingConnectionsExcluding excluded from to dawg =
  Maybe.map2
    (\fromNode toNode ->
      { dawg
        | graph =
            Graph.update to
              (\_ -> Just <|
                { toNode
                  | outgoing =
                      createOrMergeOutgoingConnections
                        (IntDict.remove excluded fromNode.outgoing)
                        toNode.outgoing
                }
              )
              dawg.graph
      }
    )
    (Graph.get from dawg.graph)
    (Graph.get to dawg.graph)
  |> Maybe.withDefaultLazy (\() -> Debug.log "👽 BUG 👽 in duplicateOutgoingConnectionsExcluding?" dawg)

duplicateOutgoingConnections : NodeId -> NodeId -> DAWG -> DAWG
duplicateOutgoingConnections from to dawg =
  Maybe.map2
    (\fromNode toNode ->
      { dawg
        | graph =
            Graph.update to
              (\_ -> Just <|
                { toNode
                  | outgoing = createOrMergeOutgoingConnections fromNode.outgoing toNode.outgoing
                }
              )
              dawg.graph
      }
    )
    (Graph.get from dawg.graph)
    (Graph.get to dawg.graph)
  |> Maybe.withDefaultLazy (\() -> Debug.log "👽 BUG 👽 in duplicateOutgoingConnections?" dawg)

type alias CurrentNodeData =
  { chosenTransition : Transition -- tC in text. Maximum-terminality among available options.
  , otherOutgoingConnectionsOfPrefix : Connections
  , id : NodeId -- d' in text
  , incomingWithoutTransition : Connections
  }

type alias LinkingForwardData =
  { graphPrefixEnd : NodeId -- dP in text
  -- there is always precisely one connection from graphPrefixEnd (dP) to currentNode (d')
  , lastConstructed : Maybe NodeId -- c in text
  , graphSuffixEnd : NodeId -- dS in text
  , splitPath : Bool -- have I split any paths on my journey forward?
  }

{-| Create a forwards-chain going from dP (the prefix-node) to dS (the
    suffix-node).  The suffix-node might be the final (dω).
-}
createForwardsChain : List Transition -> LinkingForwardData -> DAWG -> DAWG
createForwardsChain transitions linking dawg =
  let
    getCurrentNodeData : NodeId -> Transition -> DAWG -> Maybe CurrentNodeData
    getCurrentNodeData nodeid (w, isFinal) dawg_ =
      Graph.get nodeid dawg_.graph
      |> Maybe.andThen
        (\{ outgoing } ->
          IntDict.filter (\_ -> transitionMatching w) outgoing
          |> IntDict.toList
          |> List.head
          |> Maybe.andThen
            (\(k, v) ->
              Maybe.map
                (\d_ ->
                  let
                    t = if Set.member (w, 0) v then (w, 0) else (w, 1)
                  in
                    { chosenTransition = maxTerminality t (w, isFinal)
                    , id = k
                    , otherOutgoingConnectionsOfPrefix =
                        IntDict.remove k d_.outgoing
                    , incomingWithoutTransition = --d_.incoming
                        IntDict.update nodeid
                          (Maybe.andThen <| (\conn ->
                            Set.remove t conn
                            |> \removed ->
                                if Set.isEmpty removed then Nothing
                                else Just removed
                          ))
                          d_.incoming
                    }
                )
                (Graph.get k dawg_.graph)
            )
        )
  in
  case Debug.log "[Chaining] Remaining transitions" transitions of
    [] ->
      Debug.log ("[Chaining 1] NO TRANSITIONS?? Probably a bug!") linking
      |> \_ -> dawg
    (w, isFinal)::rest ->
      case getCurrentNodeData linking.graphPrefixEnd (w, isFinal) dawg of
        Nothing ->
          case linking.lastConstructed of
            Nothing ->
              println ("[Chaining 2.1.2] No separate chain created; making one between #" ++ String.fromInt linking.graphPrefixEnd ++ " and #" ++ String.fromInt linking.graphSuffixEnd)
              createTransitionChainBetween transitions linking.graphPrefixEnd linking.graphSuffixEnd dawg
            Just c ->
              let
                dawg_ =
                  duplicateOutgoingConnections linking.graphPrefixEnd c dawg
                  |> debugDAWG ("[Chaining 2.1.1.1] Duplicated outgoing connections from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt c)
              in
                if List.isEmpty rest then
                  createTransitionBetween (w, isFinal) c linking.graphSuffixEnd dawg_
                  |> debugDAWG ("[Chaining 2.1.1.2] Created node-chain between #" ++ String.fromInt c ++ " and #" ++ String.fromInt linking.graphSuffixEnd)
                else
                  createTransitionChainBetween transitions c linking.graphSuffixEnd dawg_
                  |> debugDAWG ("[Chaining 2.1.1.3] Created single " ++ transitionToString (w, isFinal) ++ " transition between #" ++ String.fromInt c ++ " and #" ++ String.fromInt linking.graphSuffixEnd)
        Just d ->
          if isConfluenceConnection linking.graphPrefixEnd d.id dawg then
            println ("[Chaining 2.2] Found #" ++ String.fromInt linking.graphPrefixEnd ++ "→#" ++ String.fromInt d.id ++ " confluence.  Chosen transition is " ++ transitionToString d.chosenTransition ++ ".")
            -- remove the transition from the confluence node
            dawgUpdate d.id (\d_ -> { d_ | incoming = d.incomingWithoutTransition }) dawg
            |> performUpdateAndRecurse rest { linking | splitPath = True } d
          else if isBackwardSplit d.id dawg then
            -- by now, we are sure that we DON'T have a confluence.  This makes the logic easier!
            println ("[Chaining 2.3] Found backward-split centered on #" ++ String.fromInt d.id ++ ".  Chosen transition is " ++ transitionToString d.chosenTransition ++ ".")
            -- remove the transition from the backward-split
            dawgUpdate d.id (\d_ -> { d_ | incoming = d.incomingWithoutTransition }) dawg
            |> performUpdateAndRecurse rest { linking | splitPath = True } d
          else
            performUpdateAndRecurse rest linking d dawg
            -- case linking.lastConstructed of
            --   Nothing ->
            --     println ("[Chaining 2.4.1] #" ++ String.fromInt d.id ++ " is neither confluence nor backward-split, and there is no constructed path.  Proceeding to next node via " ++ transitionToString (w, isFinal) ++ ".")
            --     createForwardsChain rest { linking | graphPrefixEnd = d.id } dawg
            --   Just c ->
            --     createNewSuccessorNode d.chosenTransition c dawg
            --     |> \(dawg_, successor) ->
            --       println ("[Chaining 2.4.2.1/2] Created new node #" ++ String.fromInt successor ++ ", linked from #" ++ String.fromInt d.id ++ ".  Proceeding.")
            --       duplicateOutgoingConnectionsExcluding d.id linking.graphPrefixEnd c dawg_
            --       |> debugDAWG ("[Chaining 2.4.2.3] Duplicated outgoing connections from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt c)
            --       |> createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just successor }

performUpdateAndRecurse : List Transition -> LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
performUpdateAndRecurse remaining_transitions linking d dawg =
  case remaining_transitions of
    [] -> -- only one transition remaining.
      case linking.lastConstructed of
        Nothing ->
          println ("[Chaining 2.2.4.1/2] Joining main-line #" ++ String.fromInt linking.graphPrefixEnd ++ " to main-line #" ++ String.fromInt linking.graphSuffixEnd ++ ".")
          createTransitionBetween d.chosenTransition linking.graphPrefixEnd (if linking.splitPath then d.id else linking.graphSuffixEnd) dawg
        Just c ->
          println ("[Chaining 2.2.4.1/2] Joining alt-path #" ++ String.fromInt c ++ " to main-line #" ++ String.fromInt linking.graphSuffixEnd ++ ".")
          duplicateOutgoingConnectionsExcluding d.id linking.graphPrefixEnd c dawg
          |> createTransitionBetween d.chosenTransition c (if linking.splitPath then d.id else linking.graphSuffixEnd)
    _ ->
      case linking.lastConstructed of
        Nothing ->
          createNewSuccessorNode d.chosenTransition linking.graphPrefixEnd dawg
          |> \(dawg_, successor) ->
              println ("[Chaining 2.2.2.1/2] Created new node #" ++ String.fromInt successor ++ ", linked from graph prefix #" ++ String.fromInt d.id ++ ".")
              createForwardsChain remaining_transitions { linking | graphPrefixEnd = d.id, lastConstructed = Just successor } dawg_
        Just c ->
          createNewSuccessorNode d.chosenTransition c dawg
          |> \(dawg_, successor) ->
              println ("[Chaining 2.2.2.1/2] Created new node #" ++ String.fromInt successor ++ ", linked from #" ++ String.fromInt d.id ++ ".  Proceeding.")
              duplicateOutgoingConnectionsExcluding d.id linking.graphPrefixEnd c dawg_
              |> debugDAWG ("[Chaining 2.2.2.3] Duplicated outgoing connections from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt c)
              |> createForwardsChain remaining_transitions { linking | graphPrefixEnd = d.id, lastConstructed = Just successor }

createChain : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
createChain transitions prefixEnd suffixEnd dawg =
  Debug.log ("[Chaining] Creating chain from #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt suffixEnd ++ " with transitions ") transitions
  |> \_ -> createForwardsChain
    transitions
    { graphPrefixEnd = prefixEnd
    , lastConstructed = Nothing
    , graphSuffixEnd = suffixEnd
    , splitPath = False
    }
    dawg

-- createBackwardsChain : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
-- createBackwardsChain transitions finalNode prefixEnd dawg =
--   case transitions of
--     [] ->
--       println ("[Suffix-Chain 1] NO TRANSITIONS?? Probably a bug! Was trying to back-chain (final=" ++ String.fromInt finalNode ++ ", initial=" ++ String.fromInt prefixEnd ++ "), now what do I do here?")
--       dawg
--     [firstTransition] ->
--       -- case outgoingConnectionWith (Tuple.first firstTransition) prefixEnd dawg of
--       --   Just (existing, transition) -> -- we want the exact transition because it might be terminal.
--       --     println ("[Suffix-Chain 2] I've run into a confluence or potential forward-split")
--       --     joinToConfluenceOrBackwardSplit transition prefixEnd finalNode existing dawg
--       --   Nothing ->
--           println ("[Suffix-Chain 2] Only one backwards transition; no new nodes, just a " ++ transitionToString firstTransition ++ " link from #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt finalNode)
--           createTransitionBetween firstTransition prefixEnd finalNode dawg
--     (head_w, head_terminality)::[(first_w, first_terminality)] ->
--       case outgoingConnectionWith first_w prefixEnd dawg of
--         Just (existing, transition) -> -- we want the exact transition because it might be terminal.
--           let
--             dr =
--               forwardsFollowable head_w existing.node.id dawg.graph
--               |> Maybe.withDefaultLazy (\() -> Debug.log "oooops!!!" existing)
--             (dp_id, dawg_) =
--               splitConfluence transition prefixEnd existing.node.id dawg
--               |> \(a, b) ->
--                   debugDAWG ("[Suffix-Chain 2.2.1] After splitting potential confluence (#" ++ String.fromInt prefixEnd ++ " ---" ++ transitionToString transition ++ "--> #" ++ String.fromInt existing.node.id ++ ")") b
--                   |> \_ -> (a, b)
--           in
--             println ("[Suffix-Chain 2.1] Redirecting to 2.2.2+; I've run into a confluence or potential forward-split")
--             joinToConfluenceOrBackwardSplit (head_w, head_terminality) dp_id finalNode dr dawg_
--         Nothing ->
--           println ("[Suffix-Chain 2.1] More than one backwards transition; creating a precursor node before #" ++ String.fromInt finalNode)
--           createNewPrecursorNode (head_w, head_terminality) finalNode dawg
--           |> \(dawg_, successor) ->
--             println ("[Suffix-Chain 2.2] Created precursor node #" ++ String.fromInt successor.node.id ++ "; will continue to build chain.")
--             createBackwardsChain [(first_w, first_terminality)] successor.node.id prefixEnd dawg_
--     head::rest ->
--       println ("[Suffix-Chain 2.1] More than one backwards transition; creating a precursor node before #" ++ String.fromInt finalNode)
--       createNewPrecursorNode head finalNode dawg
--       |> \(dawg_, successor) ->
--         println ("[Suffix-Chain 2.2] Created precursor node #" ++ String.fromInt successor.node.id ++ "; will continue to build chain.")
--         createBackwardsChain rest successor.node.id prefixEnd dawg_

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