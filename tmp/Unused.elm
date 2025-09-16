
-- Original source: DFA.elm.
modifyConnection : NodeId -> NodeId -> Connection -> AutomatonGraph -> AutomatonGraph
modifyConnection source target newConn =
  -- find the correct source.  From there, I can change the connection.
  -- Changing the connection cannot possibly affect anything that is
  -- later in the graph—all of that is set—but it can affect things that
  -- are "prior" to the destination.
  -- If `newConn` is an empty set, then a link is destroyed: this may
  -- result in a disconnected portion of the graph (if it was the only
  -- link to the start).  Therefore, I need to use `remove_unreachable`
  -- on the `target`, to get rid of disconnected portions.
  -- Whether disconnected or not, there may now be new similarities in
  -- the graph.  So, we can find the nodes to examine using `wordsEndingAt`
  -- (going either from source [if the link is destroyed] or from target
  -- [otherwise]), and then use `replace_or_register` to re-check for
  -- similarities.
  -- 
  -- I will also need to put in tests for these cases…
  let
    craaazy_extend : DFARecord {} -> ExtDFA
    craaazy_extend dfa =
      { states = dfa.states
      , transition_function = dfa.transition_function
      , start =
          -- remember, .start functions as the OLD start, and is the value used in
          -- `remove_unreachable`.  After that phase is done, we set it to be the same
          -- value as .clone_start, which then functions as the NEW start.
          -- We don't actually HAVE a good value for this!
          -- If the set is empty, then .start can be the `target`.  `remove_unreachable`
          -- should get rid of it for us.
          -- But if the set is not empty, then I should not run `remove_unreachable`
          -- at all; for example, if the `source` is the starting node, then we will
          -- end up removing a lot of nodes in the end, incorrectly.
          -- So in preparation for a possible call to `remove_unreachable`, let me
          -- set this to the only sane possible value: `target`
          target
      , finals = dfa.finals
      , register =
          IntDict.keys dfa.states
          |> Set.fromList
          |> Set.remove source
          |> Set.remove target
      , clone_start =
          -- see the comment for .start.
          dfa.start
      , queue_or_clone =
          if source == target then
            [ source ]
          else
            [ source, target ]
      , unusedId = IntDict.keys dfa.states |> List.maximum |> Maybe.withDefault 0 |> (+) 1
      }
    rewriteLink : AutomatonGraph -> AutomatonGraph
    rewriteLink g =
      { g
        | graph =
            Graph.update source
              (Maybe.map (\sourceContext ->
                { sourceContext
                  | outgoing =
                      if AutoSet.isEmpty newConn then
                        IntDict.remove target sourceContext.outgoing
                      else
                        IntDict.insert target newConn sourceContext.outgoing
                }
              ))
              g.graph
      }
  in
    rewriteLink
    -- |> Automata.Debugging.debugGraph "[modifyConnection] After rewriteLink"
    >> fromAutomatonGraph
    -- |> debugDFA_ "[modifyConnection] After conversion to DFA"
    >> craaazy_extend
    -- |> debugExtDFA_ "[modifyConnection] After craaazy extension…"
    >> (\dfa -> { dfa | start = dfa.clone_start })
    -- >> replace_or_register
    -- |> debugExtDFA_ "[modifyConnection] After replace_or_register"
    >> retract
    >> toAutomatonGraph

-- Original source: DFA.elm
removeConnection : NodeId -> NodeId -> AutomatonGraph -> AutomatonGraph
removeConnection source target =
  let
    craaazy_extend : DFARecord {} -> ExtDFA
    craaazy_extend dfa =
      { states = dfa.states
      , transition_function = dfa.transition_function
      , start =
          -- remember, .start functions as the OLD start, and is the value used in
          -- `remove_unreachable`.  After that phase is done, we set it to be the same
          -- value as .clone_start, which then functions as the NEW start.
          -- We don't actually HAVE a good value for this!
          -- Because the set is empty, .start can be the `target`.  `remove_unreachable`
          -- should get rid of it for us.
          target
      , finals = dfa.finals
      , register =
          IntDict.keys dfa.states
          |> Set.fromList
          |> Set.remove source
          |> Set.remove target
      , clone_start =
          -- see the comment for .start.
          dfa.start
      , queue_or_clone =
          if source == target then
            [ source ]
          else
            [ source, target ]
      , unusedId = IntDict.keys dfa.states |> List.maximum |> Maybe.withDefault 0 |> (+) 1
      }
    removeLink : AutomatonGraph -> AutomatonGraph
    removeLink g_ =
      { g_
        | graph =
            Graph.update source
              (Maybe.map (\sourceContext ->
                { sourceContext
                  | outgoing = IntDict.remove target sourceContext.outgoing
                }
              ))
              g_.graph
      }
  in
    removeLink
    >> fromAutomatonGraph
    -- |> debugDFA_ "[removeConnection] After conversion to DFA"
    >> craaazy_extend
    -- |> debugExtDFA_ "[removeConnection] After craaazy extension…"
    >>(\dfa -> remove_unreachable (all_forward_transitions target dfa) dfa)
    >> (\dfa -> { dfa | start = dfa.clone_start })
    -- |> debugExtDFA_ "[removeConnection] After remove_unreachable"
    -- >> replace_or_register
    -- |> debugExtDFA_ "[modifyConnection] After replace_or_register"
    >> retract
    >> toAutomatonGraph

-- Original source: DFA.elm
{-
This is like union, but without the final step (register-or-replace).  That's
because, during user changes, if we join nodes too eagerly, we might end up
joining nodes that the user hasn't intended to join; fundamentally, the problem
is that we don't know (yet) if the right-language is complete. Until we know,
doing register-or-replace will be inaccurate.
-}
partial_union : DFARecord a -> DFARecord a -> DFARecord {}
partial_union w_dfa_orig m_dfa =
    extend Add_Word w_dfa_orig m_dfa
    -- |> debugExtDFA_ "[partial_union] extDFA creation from merged w_dfa + dfa"
    |> phase_1 Add_Word
    -- |> debugExtDFA_ "[partial_union] End of Phase 1 (clone-and-queue)"
    |> (\extdfa -> remove_unreachable (w_forward_transitions extdfa) extdfa)
    |> (\dfa -> { dfa | start = dfa.clone_start })
    -- |> debugExtDFA_ "[partial_union] End of Phase 2 (remove-unreachable + switch-start)"
    |> retract

-- Original source: DFA.elm
complement : DFARecord a -> DFARecord a
complement dfa =
  -- the non-final states become the final states, and vice-versa.
  { dfa
    | finals = Set.diff (IntDict.keys dfa.states |> Set.fromList) dfa.finals
  }

-- Original source: DFA.elm
exclude : DFARecord a -> DFARecord a -> DFARecord {}
exclude to_exclude m_dfa =
  extend Remove_Word to_exclude m_dfa
  |> debugExtDFA_ "[exclude] extDFA creation from merged w_dfa + dfa"
  |> phase_1 Remove_Word
  |> debugExtDFA_ "[exclude] End of Phase 1 (clone-and-queue)"
  |> (\extdfa -> remove_unreachable (w_forward_transitions extdfa) extdfa)
  |> (\dfa -> { dfa | start = dfa.clone_start })
  |> debugExtDFA_ "[exclude] End of Phase 2 (remove-unreachable + switch-start)"
  |> replace_or_register
  |> debugExtDFA_ "[exclude] End of Phase 3 (replace-or-register)"
  |> retract

-- Original source: DFA.elm
delta_star : NodeId -> List Char -> DFARecord a -> Maybe NodeId
delta_star q xs dfa =
  List.foldl (\x -> Maybe.andThen (\q_ -> delta q_ x dfa)) (Just q) xs

-- Original source: DFA.elm
transitions_from_source : NodeId -> DFARecord a -> List (NodeId, AcceptVia)
transitions_from_source q dfa =
  IntDict.get q dfa.transition_function
  |> Maybe.map (AutoDict.toList >> List.map Tuple.Extra.flip)
  |> Maybe.withDefault []

-- Original source: ForceDirectedGraph.elm
{-| Create a DFA consisting of all paths ending at the specified transition.
-}
wordsEndingAt : NodeId -> AutomatonGraph -> AutomatonGraph
wordsEndingAt nodeId g =
  let
    nodes =
      Graph.guidedBfs
        Graph.alongIncomingEdges
        (Graph.ignorePath (\context acc ->
          context.node.id :: acc
        ))
        [nodeId]
        []
        (Graph.update g.root (Maybe.map (\node -> { node | incoming = IntDict.empty })) g.graph)
      |> Tuple.first
    induced =
      Graph.inducedSubgraph nodes g.graph
  in
    graphToAutomatonGraph g.root induced

-- Original source: Data.elm
graphToAutomatonGraph : NodeId -> Graph Entity Connection -> AutomatonGraph
graphToAutomatonGraph start graph =
  { graph = graph
  , maxId = Graph.nodeIdRange graph |> Maybe.map Tuple.second |> Maybe.withDefault 0
  , root = start
  }

-- Original source: DFA.elm
type alias Partition = Set NodeId
type alias HopcroftRecord =
  { w : List Partition -- still to be processed.
  , p : List Partition -- partitions
  }

hopcroft : AutomatonGraph -> List (List Int)
hopcroft dawg =
  -- This is Hopcroft's Algorithm
  let
    edges = -- Edge (Transition)
      Graph.edges dawg.graph
      |> List.concatMap
        (\{from, to, label} ->
          AutoSet.toList label
          |> List.map (\t -> (from, to, t))
        )
    (finals, nonFinals) = -- the initial partition.
      -- those which lead to finality, and those which don't.
      List.partition (\(_, _, (_, _, isFinal)) -> isFinal == 1) edges
      |> \(a, b) -> ( List.map (\(_,v,_) -> v) a |> Set.fromList, 0::List.map (\(_,v,_) -> v) b |> Set.fromList )
      -- |> debug_log "Finals and non-finals"
    refine : HopcroftRecord -> List Partition
    refine r =
      case (r {- |> debug_log "hopcroft"-}).w of
        [] ->
          r.p
        a::w_rest ->
          let
            xs =
              List.filterMap
                (\(from, to, t) ->
                  if Set.member to a then
                    Just (t, from)
                  else
                    Nothing
                ) edges
              |> List.gatherEqualsBy Tuple.first
              |> List.map (\((transition, h), t) -> (transition, Set.fromList (h::List.map Tuple.second t))) -- Now I should have a list of (ch, {states_from_w_which_go_to_`a`})
              -- |> debug_log ("`X` set, given `A` of " ++ (Debug.toString (Set.toList a)))
            refine_for_input_and_y : Partition -> Partition -> Partition -> List Partition -> List Partition -> (List Partition, List Partition)
            refine_for_input_and_y y further_split remaining_after w p =
              ( if List.member y w then
                  (further_split :: remaining_after :: List.remove y w)
                  -- |> debug_log "Refining w, stage Ⅱa"
                else
                  if Set.size further_split <= Set.size remaining_after then
                    (further_split :: w)
                    -- |> debug_log "Refining w, stage Ⅱb"
                  else
                    (remaining_after :: w)
                    -- |> debug_log "Refining w, stage Ⅱc"
              , (further_split :: remaining_after :: List.remove y p)
                -- |> debug_log "Refining p, stage Ⅱ"
              )
            refine_for_input : Transition -> Partition -> List Partition -> List Partition -> (List Partition, List Partition)
            refine_for_input _ x w p = -- really, the transition _ is only there for potential debugging.
              let
                candidate_sets =
                  List.filterMap
                    (\potential_y ->
                      let
                        further_split = Set.intersect x potential_y -- |> debug_log ("Intersection of " ++ Debug.toString x ++ " and " ++ Debug.toString potential_y)
                        remaining_after = Set.diff potential_y x -- |> debug_log ("Subtraction: " ++ Debug.toString potential_y ++ " minus " ++ Debug.toString x)
                      in
                        if Set.isEmpty remaining_after || Set.isEmpty further_split then
                          Nothing
                        else
                          Just (potential_y, further_split, remaining_after)
                    )
                    p
              in
                case candidate_sets of
                  [] ->
                    (w, p)
                  _ ->
                    -- println ("Refining for input " ++ String.fromChar ch)
                    List.foldl (\(y, further, remaining) (w_, p_) -> refine_for_input_and_y y further remaining w_ p_) (w, p) candidate_sets
            (new_w, new_p) =
              List.foldl (\(t, x) (w, p) -> refine_for_input t x w p) (w_rest, r.p) xs
          in
            refine
              { w = new_w
              , p = new_p
              }
  in
    refine
      { w = [finals, nonFinals]
      , p = [finals, nonFinals]
      }
    -- |> debug_log "Hopcroft raw result"
    -- |> debugLog "Hopcroft result" (List.map Set.size)
    |> List.filter (\s -> Set.size s > 1)
    |> List.map Set.toList

-- Original source: DFA.elm
fromGraph : NodeId -> Graph Entity Connection -> DFARecord {}
fromGraph start graph =
  { graph = graph
  , root = start
  , maxId = List.maximum (Graph.nodes graph |> List.map .id) |> Maybe.withDefault 0
  }
  |> fromAutomatonGraph

-- Original source: DFA.elm
all_forward_transitions : NodeId -> ExtDFA -> ForwardTree
all_forward_transitions start extDFA =
  let
    helper : NodeId -> Set NodeId -> ForwardTree
    helper current seen =
      case IntDict.get current extDFA.transition_function of
        Nothing -> PathEnd
        Just dict ->
          let
            filtered =
              AutoDict.filter
                (\_ state ->
                  not (Set.member state seen)
                  && current /= state
                )
                dict
            children =
              AutoDict.map (\_ state -> helper state (Set.insert state seen)) filtered
          in
            if AutoDict.isEmpty children then PathEnd else ForwardNode children
  in
    helper start Set.empty
    -- |> Debug.log "[all_forward_transitions] result"

-- Original source: ForceDirectedGraph.elm
makeSimulation : (Float, Float) -> AutomatonGraph -> Force.State NodeId
makeSimulation (w, h) g =
  Force.simulation
    (basicForces g (round h) ++ viewportForces (w, h) g.graph)

-- Original source: ForceDirectedGraph.elm
{-| Obtain the NodeId at the end of the specified path.

If the path is invalid for the graph context, then return Nothing.
-}
followPathTo : RequestedChangePath -> AutomatonGraph -> Maybe NodeId
followPathTo path g =
  let
    followPath : NodeId -> RequestedChangePath -> Maybe NodeId
    followPath current remaining =
      case remaining of
        [] ->
          Just current
        h::t ->
          Graph.get current g.graph
          |> Maybe.andThen
            (\node ->
              IntDict.toList node.outgoing
              |> List.filter (\(_, conn) -> AutoSet.member h conn)
              -- here, I am treating the graph as if it is an NFA.
              -- And that is because indeed, a user may well just treat it as an NFA
              -- and randomly create two or more paths!!  So, we need to explore each
              -- path and see which ones might be valid.
              |> List.filterMap (\(k, _) -> followPath k t)
              |> List.head
              -- |> Maybe.andThen (\(k, _) -> followPath k t)
            )
  in
    followPath g.root path
    -- |> Debug.log ("[followPathTo] Followed " ++ Debug.toString path ++ " to arrive at")

-- Original source: ForceDirectedGraph.elm
path_for_removal : Graph Entity Connection -> NodeId -> NodeId -> NodeId -> Maybe (RequestedChangePath, RequestedChangePath)
path_for_removal graph start source destination =
  -- this is called when there is a link between the source and destination,
  -- and it must be removed.  As a result, other nodes might be disconnected.
  Maybe.andThen
    (\dest ->
      let
        acceptFunction =
          -- check that there is a link between source & dest, and that
          -- the destination can be obtained via the last transition in 1 hop
          -- (unless we are dealing with recursion).
          -- Check the recursive case first.
          if source == destination then
            \_ -> True -- |> Debug.log "No accept-check, this is recursive" -- the checks in createPathTo should already cover this.
          else
            \p ->
              -- we reverse because we want to check the LAST link in the chain for 1-hop
              case List.reverse p of
                [] ->  -- you're not recursive, so there must be AT LEAST one link!
                  False
                  -- |> Debug.log "Failed accept: no links in path, but not recursive"
                h::_ ->
                  IntDict.get source dest.incoming
                  -- |> Debug.log "Is there a 1-hop link?"
                  |> Maybe.map (AutoSet.member ({- Debug.log "Checking for transition" -} h))
                  |> Maybe.withDefault False
                  -- |> Debug.log ("Is there a 1-link hop from #" ++ String.fromInt destination ++ " to #" ++ String.fromInt source)
        path =
          createPathTo destination [source] acceptFunction graph start
      in
        Maybe.map
          (\p ->
            if source == destination then
              ( p, p )
            else
              case List.reverse p of
                [] -> -- special case, this is the root
                  ( p, p )
                _::revPath ->
                  ( List.reverse revPath, p )
          )
          path
    )
    (Graph.get destination (graph {- |> debugGraph "getting userchange-data from" -}))

-- Original source: ForceDirectedGraph.elm
offset : (Float, Float) -> (Float, Float) -> (Float, Float)
offset (offset_x, offset_y) (x, y) =
  (x - offset_x, y - offset_y)

-- Original source: ForceDirectedGraph.elm
{-| Obtain the shortest path to a specified NodeId.

If no such path exists, then return Nothing.  Can specify an
`accept` function for additional checks, if desired.
-}
createPathTo : NodeId -> List NodeId -> (RequestedChangePath -> Bool) -> Graph Entity Connection -> NodeId -> Maybe RequestedChangePath
createPathTo target waypoints accept graph start =
  let
    -- avoid backtracking; so, we have a "seen" set.
    findPath : Set NodeId -> NodeId -> RequestedChangePath -> Maybe RequestedChangePath
    findPath seen current acc =
      if Set.member current seen && current /= target then
        Nothing --|> Debug.log "Already seen this; must be on a recursive path; backtracking"
      else
        Graph.get ({-Debug.log "current"-} current) graph
        |> Maybe.andThen
          (\node ->
            let
              nodeIsStart = node.node.id == start
              seenAllNecessaryNodes =
                List.all
                  (\id ->
                    id == current ||
                    Set.member id seen --|> Debug.log ("Is #" ++ String.fromInt id ++ " the current node or in " ++ Debug.toString seen)
                  )
                  (target::waypoints)
              nodeIsAccepted = accept acc
            in
              if nodeIsStart && seenAllNecessaryNodes && nodeIsAccepted then
                  Just acc
                -- else
                --   -- if I don't encounter `target` and all specified waypoints
                --   -- on the way, then this path is useless to me.
                --   Nothing |> Debug.log "Path failed checks"
              else
                node.incoming
                |> IntDict.toList
                -- |> Debug.log ("Incoming nodes for #" ++ String.fromInt current ++ " are")
                |> List.filterMap
                  (\(k, v) ->
                    if current /= k then
                      AutoSet.toList v
                      |> List.head
                      |> Maybe.andThen
                        (\t ->
                          findPath
                            (Set.insert current seen)
                            ({- Debug.log "going to look at" -} k)
                            ({- Debug.log "current path" -} (t::acc))
                        )
                    else
                      Nothing -- ignore purely recursive links; they won't get us anywhere.
                  )
                |> List.minimumBy List.length
          )
  in
    findPath Set.empty target []
    -- |> Debug.log ("[createPathTo] Asked to find path to #" ++ String.fromInt target ++ ", found")

-- Original source: Verification.elm
{-| Same as recognizedWords, but also verifies that the graph is deterministic. -}
verifiedRecognizedWords : AutomatonGraph -> List String
verifiedRecognizedWords dfa =
  let
    nonDeterministic =
      case Graph.checkAcyclic dfa.graph of
        Err _ ->
          Just "The DAWG is not acyclic."
        Ok _ ->
          Graph.get dfa.root dfa.graph
          |> Maybe.andThen
            (\root -> findNonDeterministic [root] dfa.graph)
  in
    case nonDeterministic of
      Nothing ->
        recognizedWords dfa
      Just e ->
        [e]

-- Original source: Verification.elm
findNonDeterministic : List Node -> Graph Entity Connection -> Maybe String
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

-- Original source: Verification.elm
exploreDeterministic : Node -> Graph Entity Connection -> Result String (List Node)
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
            |> List.map (AutoSet.map acceptConditionToString .via) -- |> debug_log ("CHECK for #" ++ String.fromInt node.node.id)
          allTransitions =
            List.foldl AutoSet.union (AutoSet.empty acceptConditionToString) allSets -- |> debug_log "All transitions"
          duplicate =
            List.foldl
              (\currentSet (result, all) ->
                AutoSet.diff all currentSet -- (debug_log "Checking against" currentSet)
                |> (\diff -> (AutoSet.diff (AutoSet.union currentSet result) all, diff)) -- |> debug_log "now")
              )
              (AutoSet.empty acceptConditionToString, allTransitions)
              allSets
            |> Tuple.first
        in
          if AutoSet.isEmpty duplicate then
            Nothing
          else
            Just (AutoSet.toList duplicate)
  in
    case foundNonDeterminism of
      Nothing -> -- No intersection, or no outgoing values—same difference here.
        node.outgoing
        |> IntDict.map (\k _ -> Graph.get k graph)
        |> IntDict.values
        |> List.filterMap identity
        |> Ok
      Just found ->
        Err ("Transition(s) «"
        ++ String.join ", " (List.map printableAcceptCondition found)
        ++ "» on node #"
        ++ String.fromInt node.node.id
        ++ " are not deterministic.")

-- Original source: Verification.elm
-- Entry point function
recognizedWords : AutomatonGraph -> List String
recognizedWords dawg =
  Maybe.map
    (recognizedWordsFrom dawg >> Result.map List.sort >> Result.mapError identity)
    (Graph.get dawg.root dawg.graph)
  |> Maybe.withDefault (Err "Couldn't find the root in the DAWG…!  What on earth is going on?!")
  |> Result.Extra.extract (\e -> [e])

-- Original source: Verification.elm
recognizedWordsFrom : AutomatonGraph -> Node -> Result String (List String)
recognizedWordsFrom dawg root =
  case Graph.checkAcyclic dawg.graph of
    Err edge ->
      Err <| "Edge " ++ String.fromInt edge.from ++ "→" ++ String.fromInt edge.to ++ " creates a cycle; this is not a DAWG."
    Ok _ ->
      Ok <| processStack [(root, "", False)] [] dawg.graph

-- Original source: Verification.elm
processStack : List (Node, String, Bool) -> List String -> Graph Entity Connection -> List String
processStack stack acc graph =
  case stack of
    [] -> acc
    (n, s, f)::rest ->
      processStack
        (explore n s graph ++ rest)
        (if f then s::acc else acc)
        graph

-- Original source: Verification.elm
numNodes : AutomatonGraph -> Int
numNodes dawg =
  Graph.size dawg.graph

-- Original source: Verification.elm
numEdges : AutomatonGraph -> Int
numEdges dawg =
  List.length <| Graph.edges dawg.graph

-- Original source: Verification.elm
{-| Explores incrementally in a breadth-first manner, returning a
    LIST of (node-found, new-string, is-final) -}
explore : Node -> String -> Graph Entity Connection -> List (Node, String, Bool)
explore node s graph =
  node.outgoing
  |> IntDict.map
      (\k conn ->
          Graph.get k graph
          |> Maybe.map
              (\outnode ->
                  AutoSet.toList conn
                  |> List.map
                    (\{via, isFinal} ->
                        (outnode, s ++ printableAcceptCondition via, isFinal)
                    )
              )
      )
  |> IntDict.values
  |> List.filterMap identity
  |> List.concat
