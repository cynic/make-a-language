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

tryDawgUpdate : NodeId -> (Node -> Maybe Node) -> DAWG -> DAWG
tryDawgUpdate nodeid f dawg =
  { dawg
    | graph = Graph.update nodeid (Maybe.andThen f) dawg.graph
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
    | outgoing = createOrUpdateOutgoingConnection transition from to |> Debug.log "After connection"
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
    | outgoing = IntDict.remove to (Debug.log ("Before disconnection of " ++ String.fromInt to) from.outgoing) |> Debug.log "After disconnection"
  }

obtainConnectionFrom : NodeId -> Node -> Maybe (Node, NodeId, Connection)
obtainConnectionFrom connectionDestination from =
  from.outgoing
  |> IntDict.get connectionDestination
  |> Maybe.map (\conn -> (from, connectionDestination, conn))

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
            if isFinalNode someNode.node.id dawg then
              println ("[Prefix 2.2.1] Graph node #" ++ String.fromInt someNode.node.id ++ " is final.  Go to suffix-merging WITHOUT a defined final-node.")
              mergeSuffixes
                (List.reverse transitions)
                currentNode
                -- we remove the `currentNode` from consideration because otherwise, the link to the final node would be lost
                --(CreateNewFinal (IntDict.remove currentNode someNode.incoming, someNode.node.id))
                (MergeToExistingFinal someNode.node.id)
                dawg
            else if isBackwardSplit someNode.node.id dawg || isConfluenceConnection currentNode someNode.node.id dawg then
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
  println ("[Suffix 2] Suffixes to follow from #" ++ String.fromInt currentNode ++ ", moving back towards #" ++ String.fromInt prefixEnd ++ ": " ++ transitionsToString transitions) |> \_ ->
  case transitions of
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
                    -- Debug.log "single" single |> \_ ->
                    -- println ("[Suffix 3.2.2] Shorter word than graph (back = #" ++ String.fromInt single.node.id ++ ").  Connecting #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt currentNode ++ ".")
                    -- createTransitionBetween (w, isFinal) prefixEnd currentNode dawg
                    createChain (List.reverse transitions) prefixEnd currentNode dawg
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
  , completeIncoming : Connections
  , isFinal : Bool
  }

type alias LinkingForwardData =
  { graphPrefixEnd : NodeId -- dP in text
  -- there is always precisely one connection from graphPrefixEnd (dP) to currentNode (d')
  , lastConstructed : Maybe NodeId -- c in text
  , graphSuffixEnd : NodeId -- dS in text
  }

{-| Updates incoming nodes to exclude a particular transition to a target.
    If no incoming nodes remain, then the connection itself is removed.
-}
incomingWithoutTransitionFrom : Transition -> NodeId -> Connections -> Connections
incomingWithoutTransitionFrom (w, _) target incoming =
  IntDict.update target
    (Maybe.andThen <| (\conn ->
      Set.remove (w, 0) conn
      |> Set.remove (w, 1)
      |> \removed ->
          if Set.isEmpty removed then Nothing
          else Just removed
    ))
    incoming


{-| A helper function for `createForwardsChain` -}
getForwardNodeData : NodeId -> Transition -> DAWG -> Maybe CurrentNodeData
getForwardNodeData nodeid (w, isFinal) dawg_ =
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
                , completeIncoming = d_.incoming
                , isFinal = IntDict.isEmpty d_.outgoing
                }
            )
            (Graph.get k dawg_.graph)
        )
    )

connectsToFinalNode : NodeId -> DAWG -> Maybe NodeId
connectsToFinalNode nodeid dawg =
  dawg.final
  |> Maybe.andThen
    (\final ->
      Graph.get nodeid dawg.graph
      |> Maybe.andThen
        (\node ->
          IntDict.get final node.outgoing
          |> Maybe.map (\_ -> final)
        )
    )

type SplitPathResult
  = SplitOff NodeId
  | Straight NodeId

{-| Accepts a `from` and `to` node, which MUST already be Connected by one or more transitions
   including `transition`.  Accepts a continuation to pass forward to.
   
   - If `from`→`to` is a confluence, it will be split off and the new node will be passed forward.
   - If `to` is a backwards-split, it will be split off and the new node will be passed forward.
   - Otherwise, `to` will be passed forward verbatim.
-}
splitAwayPathThenContinue : NodeId -> NodeId -> Transition -> (SplitPathResult -> DAWG -> DAWG) -> DAWG -> DAWG
splitAwayPathThenContinue from to transition continuation dawg =
  if isConfluenceConnection from to dawg then
    println ("Found #" ++ String.fromInt from ++ "→#" ++ String.fromInt to ++ " confluence.  Chosen transition is " ++ transitionToString transition ++ ".")
    -- remove the transition from the confluence node
    dawgUpdate to (\d_ -> { d_ | incoming = incomingWithoutTransitionFrom transition from d_.incoming }) dawg
    |> createNewSuccessorNode transition from
    |> \(dawg_, successor) -> continuation (SplitOff successor) dawg_
  else if isBackwardSplit to dawg then
    -- by now, we are sure that we DON'T have a confluence.  This makes the logic easier!
    println ("Found backward-split centered on #" ++ String.fromInt to ++ ".  Chosen transition is " ++ transitionToString transition ++ ".")
    -- remove the transition from the backward-split
    dawgUpdate to (\d_ -> { d_ | incoming = incomingWithoutTransitionFrom transition from d_.incoming }) dawg
    |> createNewSuccessorNode transition from
    |> \(dawg_, successor) -> continuation (SplitOff successor) dawg_
  else
    continuation (Straight to) dawg

{-| Called when there IS a path forward from `.graphPrefixEnd` to `d`. -}
traceForwardChainTo : Transition -> List Transition -> LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
traceForwardChainTo transition rest linking d dawg =
  -- if there is a connection to final BUT `d` is NOT the final node, that is a separate case!
  case ( connectsToFinalNode linking.graphPrefixEnd dawg, linking.lastConstructed, d.isFinal ) of
    ( Nothing, Nothing, False ) ->
      case rest of
        [] ->
          splitAwayPathThenContinue linking.graphPrefixEnd d.id transition
            (\splitResult dawg_ ->
              case splitResult of
                Straight g -> -- e.g. zv-kv-zv
                  println ("Straightforward join of graph #" ++ String.fromInt linking.graphPrefixEnd ++ " to suffix #" ++ String.fromInt linking.graphSuffixEnd)
                  dawgUpdate linking.graphPrefixEnd (connectTo d.id transition) dawg
                SplitOff c -> -- e.g. kp-gx-ax-gp
                    -- so at this point, I have an independent prefix & suffix, but
                    -- I also have no transitions that I can use to make a connection.
                    -- So, I will connect `c` to the outgoing connections of `graphPrefixEnd`
                    -- _and_ the outgoing connections of `d.id` to reflect the connection
                    -- to requested-suffix and the connection to existing graph-suffix respectively.
                  let
                    suffixConnections = Graph.get linking.graphSuffixEnd dawg_.graph |> Maybe.map .outgoing
                    existingConnections = Graph.get d.id dawg_.graph |> Maybe.map .outgoing
                    combined =
                      Maybe.map2 (IntDict.uniteWith (\_ -> mergeConnectionWith)) suffixConnections existingConnections
                      |> Maybe.withDefaultLazy (\() -> println "👽 BUG 👽 in case E of traceForwardChainTo??" IntDict.empty)
                  in
                    println ("Straight prefix to here; last transition; but this is a confluence NOT connected to suffix. Splitting, then linking back to #" ++ String.fromInt linking.graphSuffixEnd)
                    dawgUpdate c (\node -> { node | outgoing = combined }) dawg_
            )
            dawg
        _ -> -- av-kv-rv-kva
          splitAwayPathThenContinue linking.graphPrefixEnd d.id transition
            (\splitResult dawg_ ->
              case splitResult of
                Straight g ->
                  -- println ("Continuing to follow the graph, remaining transitions are " ++ transitionsToString rest)
                  -- createForwardsChain rest { linking | graphPrefixEnd = g } dawg_
                  dawg_
                SplitOff c -> -- e.g. zv-kv-rv-kva
                  println ("Splitting the path and continuing to follow; remaining transitions are " ++ transitionsToString rest)
                  createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just c } dawg_
            )
            dawg
    ( Nothing, Nothing, True ) ->
      debugDAWG "F" dawg
    ( Nothing, Just c, False ) -> -- e.g. ato-cto-atoz
      -- A trace-forward with an alt-path.  Let's move forward, and replicate all the outgoing
      -- connections from `graphPrefixEnd` for `c`'s predecessor
      case rest of
        [] -> -- e.g. ato-cto-at
          println ("Inserted word is a prefix of an existing word. Connecting alt-path #" ++ String.fromInt c ++ " to encountered node #" ++ String.fromInt d.id ++ " and exiting.")
          dawgUpdate c (connectTo d.id transition) dawg
        _ ->
          createNewSuccessorNode d.chosenTransition c dawg
          |> \(dawg_, successor) ->
            println ("Trace-forward with an alt-path.  Duplicating past nodes of #" ++ String.fromInt linking.graphPrefixEnd ++" to #" ++ String.fromInt c ++ ", creating new alt-path node #" ++ String.fromInt successor ++ ", linked from #" ++ String.fromInt c ++ ", then continuing.")
            duplicateOutgoingConnectionsExcluding d.id linking.graphPrefixEnd c dawg_
            |> createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just successor }
      -- debugDAWG "G" dawg
    ( Nothing, Just c, True ) ->
      debugDAWG "H" dawg
    ( Just final, Nothing, False ) -> -- e.g. an-tn-x-tx , x-b-bc-ac-bx
      -- The graph connects to a final node, which is NOT the `graphEndSuffix`.  There is no alt-path.
      -- println ("Not sure, but I think this might be a mid-extension of a word.  I'll connect it with a separate chain from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt linking.graphSuffixEnd)
      splitAwayPathThenContinue linking.graphPrefixEnd d.id transition
        (\splitResult dawg_ ->
          case splitResult of
            Straight g ->
              debugDAWG "I-1" dawg_
            SplitOff c ->
              println "Beginning an alt-path"
              createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just c } dawg_
        )
        dawg
      
      --createTransitionChainBetween (transition::rest) linking.graphPrefixEnd linking.graphSuffixEnd dawg
      
      -- In this case, 
      -- debugDAWG "I" dawg
    ( Just final, Nothing, True ) ->
      -- e.g. a-ab
      -- I connect directly to the final node, AND `d` is the final node.  The path has not
      -- split, so I know that the prefix is exact.  If this is the final transition, then
      -- whether this is straight, confluence, or backward-split does NOT matter: the "suffix"
      -- is correct anyway because the suffix is final.

      -- however, if /w/ is longer than the graph, then there will be are additional transitions.
      -- then we must split any confluence/backward-split, and rejoin at the final node after.
      case rest of
        [] -> 
          if d.id == linking.graphSuffixEnd then -- could interchangeably use `final` in this context
            println "J-2"
            dawg
          else -- e.g. xa-y-ya
            println ("Straight prefix; connected to final; but NOT connected to suffix. Combining " ++ transitionToString transition ++ " with suffix.")
            dawgUpdate linking.graphPrefixEnd
              (\node ->
                { node
                  | outgoing =
                      node.outgoing
                      |> IntDict.update d.id
                          (Maybe.andThen (removeTransitionFromConnection transition))
                      |> IntDict.update linking.graphSuffixEnd
                          (Maybe.map (addTransitionToConnection transition)
                          >> Maybe.orElseLazy (\() -> Just <| Set.singleton transition)
                          )
                }
              )
              dawg
        _ ->
          -- println ("J-2 " ++ transitionToString transition ++ " is NOT the last transition.")
          splitAwayPathThenContinue linking.graphPrefixEnd d.id transition
            (\splitResult dawg_ ->
              case splitResult of
                Straight g -> -- e.g. a-ab
                  -- We are still on a straight path past the final; I can extend straight out.
                  println ("On a straight prefix, past the final; extending straight to new final & redirecting.")
                  addFinalNode dawg_
                  |> \(newFinal, dawg__) ->
                      createTransitionChainBetween rest g newFinal.node.id dawg__
                      |> redirectNodesToFinal (IntDict.remove linking.graphPrefixEnd d.completeIncoming, d.id)
                SplitOff c -> -- e.g. xa-y-yaa
                  println ("On an alt-path now (#" ++ String.fromInt c ++ ", continuing to follow.")
                  createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just c } dawg_
            )
            dawg
          -- createTransitionChainBetween rest d.id final dawg 
    ( Just final, Just c, False ) ->
      debugDAWG "K" dawg
    ( Just final, Just c, True ) ->
      -- I am on an alt path and the graph connects to a final.  Am I also ending, though?
      -- Let me connect myself to the graphSuffixEnd, using the current transition.
      println ("On an alt-path; the graph ends here. My remaining transitions are " ++ transitionsToString rest ++ ".  Creating chain between #" ++ String.fromInt c ++ " and #" ++ String.fromInt linking.graphSuffixEnd)
      createTransitionChainBetween (transition::rest) c linking.graphSuffixEnd dawg
      -- debugDAWG "L" dawg

{-| When there is NO corresponding forward-move on the graph, we call this function
    to forge a path forward.  There is at least one forward-transition, so this will
    involve forging some alternate path/connection, at a minimum.
-}
forgeForwardChain : Transition -> List Transition -> LinkingForwardData -> DAWG -> DAWG
forgeForwardChain transition rest linking dawg =
  case ( connectsToFinalNode linking.graphPrefixEnd dawg, linking.lastConstructed ) of
    ( Nothing, Nothing ) ->
      -- e.g.: a
      -- make a new chain between prefix and suffix, and we are done.
      createTransitionChainBetween (transition::rest) linking.graphPrefixEnd linking.graphSuffixEnd dawg
      |> debugDAWG "No forward-path, no alt-path, no final-connection: connect prefix to suffix, and exit."
    ( Nothing, Just c ) ->
      duplicateOutgoingConnections linking.graphPrefixEnd c dawg
      |> createTransitionChainBetween (transition::rest) c linking.graphSuffixEnd
      |> debugDAWG "No forward-path, no final-connection, but we have an alt-path: connect alt-path to suffix, and exit."
    ( Just final, Nothing ) ->
      -- e.g. a-b
      -- Straightforward, create a confluence or connection and we are done.
      createTransitionChainBetween (transition::rest) linking.graphPrefixEnd linking.graphSuffixEnd dawg
      |> debugDAWG ("No forward-path from " ++ String.fromInt linking.graphPrefixEnd ++ " using " ++ transitionToString transition ++ " to #" ++ String.fromInt linking.graphSuffixEnd ++ "; I'll create one.")
      -- debugDAWG "C" dawg
    ( Just final, Just c) -> -- ato-cto-ati
      duplicateOutgoingConnections linking.graphPrefixEnd c dawg
      |> createTransitionChainBetween (transition::rest) c linking.graphSuffixEnd
      |> debugDAWG ("On an alt-path. No forward-path from " ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt linking.graphSuffixEnd ++ " using " ++ transitionToString transition ++ "; I'll create one.")

{-| Create a forwards-chain going from dP (the prefix-node) to dS (the
    suffix-node).  The suffix-node might be the final (dω).
-}
createForwardsChain : List Transition -> LinkingForwardData -> DAWG -> DAWG
createForwardsChain transitions linking dawg =
  Debug.log ("[Chaining] Remaining transitions " ++ transitionsToString transitions ++ " with linking ") linking |> \_ ->
  case transitions of
    [] ->
      Debug.log ("[Chaining 1] NO TRANSITIONS?? Probably a bug!") linking
      |> \_ -> dawg
    transition::rest ->
      case getForwardNodeData linking.graphPrefixEnd transition dawg of
        Nothing ->
          -- There's nothing that will take me forward.
          -- Therefore, if I'm going to connect to the suffix, I've got to
          -- forge a new forward chain.
          forgeForwardChain transition rest linking dawg
        Just d ->
          -- I have something that can take me forward, so I may be able
          -- to follow it.
          traceForwardChainTo d.chosenTransition rest linking d dawg

      -- if isFinalNode linking.graphPrefixEnd dawg then
      --   case linking.lastConstructed of
      --     Just c -> -- av-rv-rva ?
      --       println ("[Chaining 2.2.1] Extending #" ++ String.fromInt c ++ ", finishing with redirection of final-node.")
      --       -- addFinalNode dawg
      --       -- |>(\(node, dawg_) ->
      --       --     -- we know that `w` is final in the original graph—it must be, because this
      --       --     -- is the final there!—so we need to preserve that on the alternate as well.
      --       --     createTransitionChainBetween ((w, 1)::rest) c node.node.id dawg_
      --       --     |> redirectNodesToFinal (d.completeIncoming, d.id)
      --       --   )
      --       -- |> \dawg_ -> { dawg_ | graph = Graph.remove d.id dawg_.graph }
      --       dawg -- this is a placeholder until I have an example.
      --         -- and now redirect `d` as well.
      --     Nothing ->
      --       if linking.graphPrefixEnd /= linking.graphSuffixEnd then
      --         Maybe.map
      --           (\final ->
      --             println ("[Chaining 2.2.2.1] Independent prefix/suffix.  Connecting " ++ String.fromInt linking.graphPrefixEnd ++ " to " ++ String.fromInt linking.graphSuffixEnd ++ ", then extending & connecting to final #" ++ String.fromInt final)
      --             --createTransitionBetween (w, isFinal) linking.graphPrefixEnd linking.graphSuffixEnd dawg
      --             tryDawgUpdate linking.graphSuffixEnd (obtainConnectionFrom final >> redirectConnectionTo 
      --             |> createTransitionChainBetween rest linking.graphPrefixEnd final
      --           )
      --           dawg.final
      --         |> Maybe.withDefaultLazy (\() -> Debug.log "👽 BUG 👽 in createForwardsChain 2.2.2.1?" dawg)
      --       else -- xa-y-ya , xa-y-yaa , xa-y-yaaa
      --         println ("[Chaining 2.2.2.1] Straightforward extension from #" ++ String.fromInt d.id ++ " with transitions " ++ String.join "," (List.map transitionToString transitions))
      --         addFinalNode dawg
      --         |> \(node, dawg_) ->
      --           createTransitionChainBetween rest d.id node.node.id dawg_
      --           |> redirectNodesToFinal (d.incomingWithoutTransition, d.id)



      -- case getCurrentNodeData linking.graphPrefixEnd (w, isFinal) dawg of
      --   Nothing ->
      --     case linking.lastConstructed of
      --       Nothing ->
      --         -- what if this is final?
      --         if isFinalNode d.id dawg then
      --           println ("[Chaining 2.
      --         else
      --         -- if it's not final…
      --           println ("[Chaining 2.1.2] No separate chain created; making one between #" ++ String.fromInt linking.graphPrefixEnd ++ " and #" ++ String.fromInt linking.graphSuffixEnd)
      --           createTransitionChainBetween transitions linking.graphPrefixEnd linking.graphSuffixEnd dawg
      --       Just c ->
      --         let
      --           dawg_ =
      --             duplicateOutgoingConnections linking.graphPrefixEnd c dawg
      --             |> debugDAWG ("[Chaining 2.1.1.1] Duplicated outgoing connections from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt c)
      --         in
      --           -- what if this is final?
      --           -- if it's not final…
      --           -- can't possibly be the last transition, it would be caught above
      --           createTransitionChainBetween transitions c linking.graphSuffixEnd dawg_
      --           |> debugDAWG ("[Chaining 2.1.1.3] Created single " ++ transitionToString (w, isFinal) ++ " transition between #" ++ String.fromInt c ++ " and #" ++ String.fromInt linking.graphSuffixEnd)
      --   Just d ->
      --     -- what if this is final?
      --     -- if it's not final…
      --     if isConfluenceConnection linking.graphPrefixEnd d.id dawg then
      --       println ("[Chaining 2.3] Found #" ++ String.fromInt linking.graphPrefixEnd ++ "→#" ++ String.fromInt d.id ++ " confluence.  Chosen transition is " ++ transitionToString d.chosenTransition ++ ".")
      --       -- remove the transition from the confluence node
      --       dawgUpdate d.id (\d_ -> { d_ | incoming = d.incomingWithoutTransition }) dawg
      --       |> performUpdateAndRecurse rest { linking | splitPath = True } d
      --     else if isBackwardSplit d.id dawg then
      --       -- by now, we are sure that we DON'T have a confluence.  This makes the logic easier!
      --       println ("[Chaining 2.4] Found backward-split centered on #" ++ String.fromInt d.id ++ ".  Chosen transition is " ++ transitionToString d.chosenTransition ++ ".")
      --       -- remove the transition from the backward-split
      --       dawgUpdate d.id (\d_ -> { d_ | incoming = d.incomingWithoutTransition }) dawg
      --       |> performUpdateAndRecurse rest { linking | splitPath = True } d
      --     else
      --       --performUpdateAndRecurse rest linking d dawg
      --       case linking.lastConstructed of
      --         Nothing ->
      --           println ("[Chaining 2.5.1] #" ++ String.fromInt d.id ++ " is neither confluence nor backward-split, and there is no constructed path.  Proceeding to next node via " ++ transitionToString (w, isFinal) ++ ".")
      --           createForwardsChain rest { linking | graphPrefixEnd = d.id } dawg
      --         Just c ->
      --           createNewSuccessorNode d.chosenTransition c dawg
      --           |> \(dawg_, successor) ->
      --             println ("[Chaining 2.5.2.1/2] Created new node #" ++ String.fromInt successor ++ ", linked from #" ++ String.fromInt d.id ++ ".  Proceeding.")
      --             duplicateOutgoingConnectionsExcluding d.id linking.graphPrefixEnd c dawg_
      --             |> debugDAWG ("[Chaining 2.5.2.3] Duplicated outgoing connections from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt c)
      --             |> createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just successor }

-- linkSingleNode : LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
-- linkSingleNode linking d dawg =
--   let
--     join =
--       if linking.splitPath then
--         d.id |> Debug.log "[Chaining 2.3.4] Path-splitting happened. Joining to main-line #"
--       else
--         linking.graphSuffixEnd |> Debug.log "[Chaining 2.3.4] No path-splitting occurred, retaining main-line join-point #"
--   in
--     case linking.lastConstructed of
--       Nothing ->
--         println ("[Chaining 2.3.4.1/2] Joining main-line #" ++ String.fromInt linking.graphPrefixEnd ++ " to main-line #" ++ String.fromInt join ++ ".")
--         createTransitionBetween d.chosenTransition linking.graphPrefixEnd join dawg
--       Just c ->
--         println ("[Chaining 2.3.4.1/2] Joining alt-path #" ++ String.fromInt c ++ " to main-line #" ++ String.fromInt linking.graphSuffixEnd ++ ".")
--         duplicateOutgoingConnectionsExcluding d.id linking.graphPrefixEnd join dawg
--         |> createTransitionBetween d.chosenTransition c join

-- performUpdateAndRecurse : List Transition -> LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
-- performUpdateAndRecurse remaining_transitions linking d dawg =
--   case remaining_transitions of
--     [] -> -- only one transition remaining.
--       linkSingleNode linking d dawg
--     _ ->
--       case linking.lastConstructed of
--         Nothing ->
--           createNewSuccessorNode d.chosenTransition linking.graphPrefixEnd dawg
--           |> \(dawg_, successor) ->
--               println ("[Chaining 2.3.2.1/2] Created new node #" ++ String.fromInt successor ++ ", linked from graph prefix #" ++ String.fromInt linking.graphPrefixEnd ++ ".")
--               createForwardsChain remaining_transitions { linking | graphPrefixEnd = d.id, lastConstructed = Just successor } dawg_
--         Just c ->
--           createNewSuccessorNode d.chosenTransition c dawg
--           |> \(dawg_, successor) ->
--               println ("[Chaining 2.3.2.1/2] Created new node #" ++ String.fromInt successor ++ ", linked from #" ++ String.fromInt d.id ++ ".  Proceeding.")
--               duplicateOutgoingConnectionsExcluding d.id linking.graphPrefixEnd c dawg_
--               |> debugDAWG ("[Chaining 2.3.2.3] Duplicated outgoing connections from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt c)
--               |> createForwardsChain remaining_transitions { linking | graphPrefixEnd = d.id, lastConstructed = Just successor }

createChain : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
createChain transitions prefixEnd suffixEnd dawg =
  println ("[Chaining] Creating chain from #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt suffixEnd ++ " with transitions " ++ transitionsToString transitions)
  createForwardsChain
    transitions
    { graphPrefixEnd = prefixEnd
    , lastConstructed = Nothing
    , graphSuffixEnd = suffixEnd
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

{-| Explores incrementally in a breadth-first manner, returning a
    LIST of (node-found, new-string, is-final) -}
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

exploreDeterministic : Node -> DAWGGraph -> Result String (List Node)
exploreDeterministic node graph =
  let
    foundNonDeterminism =
      if IntDict.size node.outgoing <= 1 then
        Nothing
      else
        let
          allSets =
            node.outgoing
            |> IntDict.values
            |> List.map (Set.map Tuple.first) -- |> Debug.log ("CHECK for #" ++ String.fromInt node.node.id)
          allTransitions =
            List.foldl Set.union Set.empty allSets -- |> Debug.log "All transitions"
          duplicate =
            List.foldl
              (\currentSet (result, all) ->
                Set.diff all currentSet -- (Debug.log "Checking against" currentSet)
                |> (\diff -> (Set.diff (Set.union currentSet result) all, diff)) -- |> Debug.log "now")
              )
              (Set.empty, allTransitions)
              allSets
            |> Tuple.first
        in
          if Set.isEmpty duplicate then
            Nothing
          else
            Just (Set.toList duplicate)
  in
    case foundNonDeterminism of
      Nothing -> -- No intersection, or no outgoing values—same difference here.
        node.outgoing
        |> IntDict.map (\k _ -> Graph.get k graph)
        |> IntDict.values
        |> List.filterMap identity
        |> Ok
      Just found ->
        Err ("Transition(s) «" ++ String.fromList found ++ "» from node #" ++ String.fromInt node.node.id ++ " are not deterministic.")

findNonDeterministic : List Node -> DAWGGraph -> Maybe String
findNonDeterministic stack graph =
  case stack of
    [] -> Nothing
    n::rest ->
      case exploreDeterministic n graph of
        Err e -> Just e
        Ok nodes ->
          findNonDeterministic
            (nodes ++ rest)
            graph

{-| Same as recognizedWords, but also verifies that the graph is deterministic. -}
verifiedRecognizedWords : DAWG -> List String
verifiedRecognizedWords dawg =
  let
    nonDeterministic =
      Graph.get dawg.root dawg.graph
      |> Maybe.andThen (\root -> findNonDeterministic [root] dawg.graph)
  in
    case nonDeterministic of
      Nothing ->
        recognizedWords dawg
      Just e ->
        [e]