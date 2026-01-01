module Automata.DFA exposing (..)
import AutoDict
import Data exposing (..)
import Debugging exposing ( printAutomatonGraph, debugAutomatonGraph, printNodeContext, println, printFan, debugLog_, debugAutomatonGraphXY)
import AutoSet
import Basics.Extra exposing (..)
import Css exposing (expanded)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Graph exposing (NodeContext, NodeId, Edge)
import IntDict exposing (IntDict)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe
import Set exposing (Set)
import Tuple.Extra as Tuple
import Uuid exposing (Uuid)

-- Note: Graph.NodeId is just an alias for Int. (2025).

{-
  This gives back an AutomatonGraph in which the specified transition leaving from `source`
  is FULLY resolved.
  By "FULLY resolved", we don't mean that every transition within that graph is resolved.
  Instead, we mean that the the outbounds of `source` are fully resolved to a concrete form.

  Important Parameters:
  - `start_id`: An unused node-id that can be used internally.
  - `source_id`: the node-id of the node from which the transition leaves.
  - `source_graph`: the graph containing the `source_id`.
-}
resolveTransitionFully : NodeId -> ResolutionDict -> AutoSet.Set String Uuid -> NodeId -> AutomatonGraph -> AutomatonGraph
resolveTransitionFully start_id resolutionDict recursion_stack source_id source_graph =
  let
    mkDbg_prefix s = "[resolveTransitionFully " ++ s ++ "] "
    dbg_prefix = mkDbg_prefix (Maybe.withDefault "(no desc.)" source_graph.description)
    -- we begin by renumbering ourselves.
    (source_graph_nodemap, renumbered) =
      renumberAutomatonGraphFrom start_id source_graph
      |> (\(a,b) -> debugAutomatonGraph (dbg_prefix ++ "renumbered") b |> \_ -> (a, b))
    (outbounds, (source_graph_source, source_graph_dests)) =
      IntDict.get source_id source_graph_nodemap
      |> Maybe.andThen
        (\renumbered_source ->
          Graph.get renumbered_source renumbered.graph
        )
      |> Maybe.map
        (\ctx ->
          let
            -- that source node can have multiple destination nodes.
            -- And for each destination node, we want the per-connection
            -- outbounds.
            -- Later on, we will attach the correct outbounds—depending
            -- on the connection they came from—to the terminals of the
            -- quoted graph.
            outs =
              IntDict.toList ctx.outgoing
              |> List.filterMap
                (\(k, conn) ->
                  Graph.get k renumbered.graph
                  |> Maybe.map
                    (\dest_ctx ->
                      ( k -- this is the destination node
                      , dest_ctx.outgoing -- this is the outgoing connections from that destination node.
                      , conn -- this is the connection leading to that destination node.
                      )
                    )
                )
            -- and now that I have the links, I don't actually need either the source-node OR the destination-node.
            -- After all, I will be replacing them with a graft.
            -- So I can get rid of them here.
          in
            ( outs
            , ( ctx.node.id
                |> Debug.log (dbg_prefix ++ "source-graph source-node id")
              , IntDict.keys
                  ( ctx.outgoing
                    |> debugLog_ (dbg_prefix ++ "source-graph fan-out, with renumbered ids") printFan
                  )
              )
            )
        )
      |> Maybe.withDefault ([], (-1, [])) -- no valid source-node???  Whaaaaaaaaaaaat???
    char_transition_to_graftable : NodeId -> NodeId -> Char -> (NodeId, AutomatonGraph)
    char_transition_to_graftable unusedId source_graph_dest ch =
      let
        t =
          AutoSet.singleton transitionToString <|
            Transition True (ViaCharacter ch)
        next_unusedId =
          if source_graph_dest == source_graph_source then
            unusedId + 1
          else
            unusedId + 2
        graph =
          if source_graph_dest == source_graph_source then
            Graph.fromNodesAndEdges
              [ Graph.Node unusedId (entity unusedId NoEffect) ]
              [ Edge unusedId unusedId t ]
          else
            Graph.fromNodesAndEdges
              [ Graph.Node unusedId (entity unusedId NoEffect)
              , Graph.Node (unusedId + 1) (entity (unusedId + 1) NoEffect)
              ]
              [ Edge unusedId (unusedId + 1) t
              ]
      in
        ( next_unusedId
        , { graph = graph
          , root = unusedId
          , description =
              Maybe.map (\s -> s ++ "💋-") source_graph.description
          }
          -- |> debugAutomatonGraph (mkDbg_prefix uuid ++ "character transition to AutomatonGraph")
        )
    renumbered_unusedId =
      Graph.nodeIdRange renumbered.graph
      |> Maybe.map (\(_, end) -> end + 1)
      |> Maybe.withDefault 1 --- uhh, what now?
    -- I have all the relevant data. So now we want to remove the old connections from `src`.
    -- If there is any node that, after this, does not have any incoming edges,
    -- then I can remove that node entirely.
    -- So, let me do this as a two-step thing.
    -- First, I'll visit the `dest` nodes and remove those connections.
    -- If I find a `dest` that does not have any incoming edges, then I will
    -- add it to a list for removal.
    (dest_nodes_to_check, after_dest_connections_removed) =
      List.foldl
        (\dest_nodeid (acc, g) ->
          case Graph.get dest_nodeid g.graph of
            Nothing ->
              -- huh?  Oh well, though; I'll take it!
              ( acc, g )
            Just dest_ctx ->
              let
                new_incoming =
                  IntDict.filter (\k _ -> k /= source_graph_source) dest_ctx.incoming
                  |> debugLog_ (dbg_prefix ++ "Removed #" ++ String.fromInt source_graph_source ++ " incoming from dest #" ++ String.fromInt dest_ctx.node.id ++ ". Remaining incoming connections") printFan
                new_graph =
                  { g
                    | graph =
                        Graph.insert
                          { dest_ctx
                            | incoming = new_incoming
                            , outgoing =
                                if dest_ctx.node.id == source_graph_source then
                                  -- self-recursive!
                                  -- gotta get rid of it from here too.
                                  IntDict.remove source_graph_source dest_ctx.outgoing
                                else
                                  dest_ctx.outgoing
                          }
                          g.graph
                  }
              in
                if IntDict.isEmpty new_incoming then
                  ( dest_nodeid :: acc, new_graph )
                else
                  ( acc, new_graph )
        )
        ([], renumbered)
        -- Avoid removing the source node.
        source_graph_dests
    -- get the respective graphs for each of the outgoing transitions, and graft them on
    convert_and_graft : (NodeId, IntDict Connection, Connection) -> (NodeId, AutomatonGraph) -> (NodeId, AutomatonGraph)
    convert_and_graft (source_graph_dest, outs, conn) (unusedId_, this_graph) =
      let
        identify_terminal_nodes : NodeContext Entity Connection -> List NodeId -> List NodeId
        identify_terminal_nodes ctx acc =
          let
            isTerminal = -- does this connection contain any terminal incoming transitions?
              -- because we are looking at a graph with separated terminal/non-terminal nodes,
              -- if we find any, we can conclude for all.
              ctx.incoming
              |> IntDict.values
              |> List.any (AutoSet.filter .isFinal >> (not << AutoSet.isEmpty))
          in
            if isTerminal then
              ctx.node.id :: acc
            else
              acc
        graft : NodeId -> Transition -> AutomatonGraph -> AutomatonGraph -> (NodeId, AutomatonGraph)
        graft unusedId transition graft_onto unstretched_graph_to_graft =
          let
            -- ensure that every graph has at least 2 nodes in it… or 0, if you're insane.
            graph_to_graft =
              -- I call `graft` from various places, so I do a precautionary stretch at the start.
              stretch unstretched_graph_to_graft
              |> debugAutomatonGraph (dbg_prefix ++ "graph to graft (stretched if necessary) is")
            terminals = -- the terminals of the split_graph
              -- debugAutomatonGraph (dbg_prefix ++ "graph to graft onto is") graft_onto |> \_ ->
              Graph.fold
                identify_terminal_nodes
                []
                graph_to_graft.graph
              -- |> Debug.log (dbg_prefix ++ "Terminals re:graft")
            -- now, within this context, I need to graft the split_graph
            -- into the current graph. So let's begin with Step 1.
            -- 
            -- Toss all the nodes into the containing graph;
            -- and also maintain finality or remove it, as directed.
            -- Now link outbounds to the terminals, in the containing graph.
            quoted_nodes : List (NodeContext Entity Connection)
            quoted_nodes =
              if transition.isFinal then
                -- println (dbg_prefix ++ "Transferring quoted-graph nodes as-is.")
                Graph.fold (::) [] graph_to_graft.graph
              else
                -- println (dbg_prefix ++ "Transferring quoted-graph nodes as non-terminals; host-dest is non-terminal.")
                Graph.fold
                  (\ctx acc ->
                    -- there are no terminal transitions.
                      { ctx
                        | incoming = IntDict.map (\_ -> AutoSet.map transitionToString (\t -> { t | isFinal = False })) ctx.incoming
                        , outgoing = IntDict.map (\_ -> AutoSet.map transitionToString (\t -> { t | isFinal = False })) ctx.outgoing
                      } :: acc
                  )
                  []
                  graph_to_graft.graph
            quoting_graph_with_quoted_nodes : AutomatonGraph
            quoting_graph_with_quoted_nodes =
              { graft_onto
                | graph =
                    List.foldl (Graph.insert) graft_onto.graph quoted_nodes
              }
            -- link all the outbounds from the quoted-graph.
            link_quoted_graph_outbound : NodeId -> AutomatonGraph -> AutomatonGraph
            link_quoted_graph_outbound terminal_id ag =
              { ag
                | graph =
                    Graph.update terminal_id
                      (Maybe.map (\ctx ->
                        -- debugLog_ (dbg_prefix ++ "Linking terminal #" ++ String.fromInt terminal_id ++ " to outbounds") printFan outs |> \_ ->
                        { ctx
                          | outgoing =
                              IntDict.uniteWith
                                (\_ l _ -> l) -- this should NEVER happen! Node IDs are unique…!
                                ctx.outgoing
                                outs
                        }
                      ))
                      ag.graph
              }
            with_outbounds_linked : AutomatonGraph
            with_outbounds_linked =
              List.foldl
                (link_quoted_graph_outbound)
                quoting_graph_with_quoted_nodes
                terminals
              -- |> debugAutomatonGraph (dbg_prefix ++ "with terminals linked to outbounds")
            -- and link all the inbounds of `source_id` to the root of `with_outbounds_linked`.
            -- We do this easily by just setting the root to be the same as the `source_graph`
            -- root; and then attaching the outbounds from our "native" root to it.
            with_inbounds_linked : AutomatonGraph
            with_inbounds_linked =
              Graph.get graph_to_graft.root with_outbounds_linked.graph
              |> Maybe.map
                (\root_ctx -> -- this is from the quoted graph
                  { with_outbounds_linked
                    | root =
                        if source_graph_source == with_outbounds_linked.root then
                          source_graph_source -- reassign.
                        else
                          with_outbounds_linked.root -- stays as-is.
                    , graph =
                        Graph.update source_graph_source
                          (Maybe.map
                            (\source_node_ctx ->
                                { source_node_ctx
                                  | outgoing =
                                      IntDict.uniteWith
                                        (\_ l _ -> l) -- this should NEVER happen! Node IDs are unique…!
                                        source_node_ctx.outgoing
                                        root_ctx.outgoing
                                  , incoming =
                                      IntDict.uniteWith
                                        (\_ l _ -> l) -- this should NEVER happen! Node IDs are unique…!
                                        source_node_ctx.incoming
                                        root_ctx.incoming
                                }
                            )
                          )
                          with_outbounds_linked.graph
                        -- and now the native root is irrelevant. We can remove it.
                        |> Graph.remove graph_to_graft.root
                  }
                  -- |> debugAutomatonGraph (dbg_prefix ++ "removed #" ++ String.fromInt graph_to_graft.root ++ " (graft-root) after unioning its inbounds & outbounds with original source #" ++ String.fromInt source_graph_source)
                )
                |> Maybe.withDefault with_outbounds_linked -- NOOOOOOOOO!! SHOULD NOT BE HERE!!!
          in
            ( Graph.nodeIdRange with_inbounds_linked.graph
              |> Maybe.map (\(_, end) -> end + 1)
              |> Maybe.withDefault unusedId -- whaaaaaaaaaaaaaaaaaaaaaaat! how???
            , with_inbounds_linked
            )
        graph_transition_to_graftable : NodeId -> AutomatonGraph -> Transition -> (NodeId, AutomatonGraph)
        graph_transition_to_graftable unusedId graft_onto transition =
          case transition.via of
            ViaCharacter ch ->
              let
                (next_unusedId, graftable) =
                  char_transition_to_graftable unusedId source_graph_dest ch
              in
                graft next_unusedId transition graft_onto graftable
            ViaGraphReference ref ->
              if AutoSet.member ref recursion_stack then
                -- don't consider it; that would lead us to recursion.
                println (dbg_prefix ++ "The " ++ truncate_uuid ref ++ " ref would lead to recursion; ignoring.")
                ( unusedId
                , this_graph
                )
              else
                AutoDict.get ref resolutionDict
                |> Maybe.map
                  (\referenced_graph ->
                    let
                      resulting_graph =
                        resolveTransitionFully
                          unusedId
                          resolutionDict
                          (AutoSet.insert ref recursion_stack)
                          referenced_graph.root
                          ( referenced_graph
                            -- |> debugAutomatonGraph (dbg_prefix ++ "I found a referenced graph (" ++ truncate_uuid referenced_graph.graphIdentifier ++ ") to resolve, and will resolve it now.")
                          )
                      split_graph =
                        resulting_graph
                        |> splitTerminalAndNonTerminal -- this includes a `stretch` at the end
                      new_unusedId =
                        Graph.nodeIdRange split_graph.graph
                        |> Maybe.map (Tuple.second >> (+) 1)
                        |> Maybe.withDefault -1 -- huh?!!
                    in
                      graft new_unusedId transition graft_onto split_graph
                  )
                |> Maybe.withDefault -- invalid reference. How on earth did this sneak in??
                  ( unusedId
                  , graft_onto
                  )
      in
        AutoSet.toList conn
        |> List.foldl
          (\transition (unusedId, g) ->
            graph_transition_to_graftable unusedId g transition
            -- |> debugLog_ (dbg_prefix ++ "unused-id after grafting transition " ++ transitionToString transition) (Tuple.first)
            -- |> debugLog_ (dbg_prefix ++ "automaton graph after grafting transition " ++ transitionToString transition) (Tuple.second >> printAutomatonGraph)
          )
          (unusedId_, this_graph)
        |> debugLog_ (dbg_prefix ++ "post-graft of connection " ++ connectionToOutputString conn) (Tuple.second >> printAutomatonGraph)
    after_graft =
      List.foldl
        convert_and_graft
        (renumbered_unusedId, after_dest_connections_removed)
        outbounds
      |> Tuple.second
      |> debugLog_ (dbg_prefix ++ "automaton graph after grafting transitions individually") (printAutomatonGraph)
    without_old_dests =
      -- For `src`, I link to the original; and that is sufficient.
      -- For `dest`, I can remove some of the originals because I have rebound all the outgoings
      -- to the necessary terminals.  
      { after_graft
        | graph =
            List.foldl
              (\dest_nodeid g ->
                  Graph.update dest_nodeid
                    (Maybe.andThen
                      (\dest_ctx ->
                          if IntDict.isEmpty dest_ctx.outgoing then
                            Nothing -- remove it!
                          else
                            Just dest_ctx -- keep it.
                      )
                    )
                    g
              )
              after_graft.graph
              -- the source node might be recursive (source = dest).
              -- Avoid removing the source node.
              ( dest_nodes_to_check
                |> List.filter ((/=) source_graph_source)
                |> Debug.log (dbg_prefix ++ "`dest` nodes from source-node #" ++ String.fromInt source_graph_source ++ " to check for removal are")
              )
      }
      -- |> debugAutomatonGraph (dbg_prefix ++ "After removing the necessary `dest` values")
    -- convert NFA→DFA, then minimise for the final result.
    minimised_dfa =
      -- for now, splitTerminal&NonTerminal + minimiseNodes is not sufficient to handle some cases.
      -- case in point: [ (8, "2", 10), (8, "1", 9), (10, "2", 10), (9, "1", 9) ] should get me
      -- [ (8, "12", 8) ] but it does not minimise.  So, I'm going to just do a full round-trip.
      -- After I write a paper—which is good, this will give me impetus to get it done!—then I can
      -- continue and try to sort out minimisation more properly.
      -- splitTerminalAndNonTerminal without_old_dests
      -- |> nfaToDFA
      -- |> minimiseNodesByCombiningTransitions
      let
        dfa = fromAutomatonGraph without_old_dests
        unioned = union dfa dfa
        ag = toAutomatonGraph unioned
      in
        renumberAutomatonGraphFrom start_id ag
        |> Tuple.second
        |> debugAutomatonGraphXY (dbg_prefix ++ "Minimised, renumbered 'final'")
  in
    minimised_dfa

{-| Given a transition and a fan-out, takes the specified transition, returning the
    `NodeContext` at the end of that transition and the `NodeId`s of the transitions
    that were NOT taken.
-}
takeTransition : Transition -> IntDict Connection -> AutomatonGraph -> Maybe (NodeContext Entity Connection, List NodeId)
takeTransition t fan g =
  IntDict.toList fan
  |> List.partition (\(_, conn) -> AutoSet.member t conn)
  |>
    (\(taken, not_taken) ->
      case taken of
        [(id, _)] -> -- no NFAs allowed.
          Maybe.combineFirst
            ( Graph.get id g.graph
            , List.map Tuple.first not_taken
              -- |> Debug.log ("[expand] Nodes NOT taken, with transition " ++ transitionToString t)
            )
        _ ->
          Nothing
    )

cull : List (NodeId, NodeId) -> AutomatonGraph -> AutomatonGraph
cull edges ag =
  edges
  -- |> Debug.log "[cull] Culling the following edges"
  |> List.foldl
    (\(src, dest) g ->
      { g
        | graph =
            Graph.update dest
              (Maybe.map
                (\node ->
                  { node
                    | incoming = IntDict.remove src node.incoming
                    , outgoing =
                        if src == dest then
                          IntDict.remove dest node.outgoing
                        else
                          node.outgoing
                  })
              )
              g.graph
      }
    )
    ag
  |> removeDisconnectedNodes

{-| Expand the selected `AutomatonGraph`, _and_ cull previously-taken transitions.

    Returns:
    - the updated `AutomatonGraph`;
    - the transitions taken in the updated graph; and
    - if it exists, the `NodeContext` for the node at the end of the supplied transitions.
-}
expand : AutomatonGraph -> ResolutionDict -> NodeId -> List TransitionTakenData -> ( AutomatonGraph, List TransitionTakenData, Maybe (NodeContext Entity Connection) )
expand e resolutionDict current taken_record =
  let
    unusedId =
      Graph.nodeIdRange e.graph
      |> Maybe.map (\(_, end) -> end + 1)
      |> Maybe.withDefault -1 -- huh?
    expanded =
      resolveTransitionFully
        unusedId
        resolutionDict
        (AutoSet.empty Uuid.toString)
        current
        e
      |> debugAutomatonGraph ("[expand] Expanded options from #" ++ String.fromInt current)
    followPath : Transition -> (List TransitionTakenData, List (NodeId, NodeId), Maybe (NodeContext Entity Connection, List NodeId)) -> (List TransitionTakenData, List (NodeId, NodeId), Maybe (NodeContext Entity Connection, List NodeId))
    followPath t (acc, to_disconnect, lastContext) =
      case lastContext of
        Nothing ->
          (acc, to_disconnect, Nothing)
        Just (last_taken, not_taken) ->
          -- debugLog_ ("[expand→followPath] Trying to take transition '" ++ transitionToString t ++ "' from #" ++ String.fromInt ctx.node.id ++ "; transitions are") printFan ctx.outgoing |> \_ ->
          ( TransitionTakenData last_taken.node.id t :: acc
          , List.map (\dest -> (last_taken.node.id, dest)) not_taken ++ to_disconnect
            -- |> Debug.log ("[expand→followPath] Collective edges NOT taken")
          , takeTransition t last_taken.outgoing expanded
          )
    (transitions_taken, edges_not_taken_previously, currentNode) =
      List.map (.matching) taken_record
      |> List.foldl
          followPath
          ([], [], Maybe.combineFirst (Graph.get expanded.root expanded.graph, []))
      |> (\(a, b, c) -> (List.reverse a, b, Maybe.map Tuple.first c))
    culled =
      cull edges_not_taken_previously expanded
  in
    -- debugLog_ "[expand] resolution-dict" (AutoDict.toList >> List.map (\(k, v) -> (truncate_uuid k, printAutomatonGraph v))) resolutionDict |> \_ ->
    ( culled, transitions_taken, currentNode )

-- if I can't make another transition, return `Nothing`
oneTransition : ExecutionData -> ExecutionData
oneTransition data =
  -- UPDATED PROCESS:
  -- I expand one level IN ADVANCE!  BEFORE I go through a transition!
  -- And this means that, when called, I should ALWAYS be able to take
  -- a single transition—if any transition exists.
  -- And at the end of that transition, I will arrive at some node, and
  -- I must then expand again.
  case data.finalResult of
    Just _ ->
      data -- we have a final resolution. So stick to it.
    Nothing ->
      case data.remainingData of
        [] ->
          -- we're done! Can't go any further! ExecutionState would stay the same…
          { data
            | finalResult =
                Just (InternalError "I'm done; I shouldn't be being called at all. Good day to you, and good-bye!")
            , step = data.step + 1
          }
        h::remainingData ->
          let
            -- the node-ids may well have changed.  So, find out where we are.
            currentNode =
              Graph.get data.currentNode data.computation.graph
            thisMove : Maybe (Transition, List (NodeId, NodeId), NodeContext Entity Connection)
            thisMove =
              Maybe.andThen
                (\ctx ->
                    takeTransition
                      (Transition True (ViaCharacter h))
                      ( ctx.outgoing
                        -- |> debugLog_ ("[oneTransition] possible transitions from #" ++ String.fromInt ctx.node.id) printFan
                      )
                      data.computation
                    |> Maybe.map (\(next_ctx, not_taken) ->
                      ( Transition True (ViaCharacter h)
                      , List.map (\dest -> (ctx.node.id, dest)) not_taken
                      , next_ctx
                      )
                    )
                    |> Maybe.orElseLazy
                        (\() ->
                          takeTransition (Transition False (ViaCharacter h)) ctx.outgoing data.computation
                          |> Maybe.map (\(next_ctx, not_taken) ->
                            ( Transition False (ViaCharacter h)
                            , List.map (\dest -> (ctx.node.id, dest)) not_taken
                            , next_ctx
                            )
                          )
                        )
                    -- |> debugLog_ ("[oneTransition] this-move result") (Maybe.map <| \(t, _) -> transitionToString t)
                )
                currentNode
          in
            case thisMove of
              Nothing ->
                -- I can't proceed any further.  But: why??
                case currentNode of
                  Nothing ->
                    -- ah; I failed somewhere on the path TO here.
                    -- This should be impossible; it seems to indicate that a path
                    -- which previously existed has now gone missing.
                    -- This is a bug.
                    -- And I will die on this hill…
                    { data
                      | finalResult =
                          Just <|
                            InternalError "I failed somewhere on the path.  This should be impossible; it seems to indicate that a path which previously existed has now gone missing. This extremely likely to be a bug or, even worse, some unexpected effect that I have simply never considered before.  Please investigate what's going on thoroughly!"
                      , step = data.step + 1
                    }
                    -- I return the one just BEFORE ^ the crazy happened, for debugging purposes.
                  Just x ->
                    -- println ("[oneTransition] Searched for '" ++ String.fromChar h ++ "' but no possible transition from #" ++ String.fromInt x.node.id)
                    -- so I got up to this point, and NOW there's a problem.
                    { data
                      | finalResult =
                          Just NoMatchingTransitions
                      , step = data.step + 1
                    }
              Just (t, not_taken_last, newNode) ->
                -- debugLog_ ("[oneTransition] Found transition from #" ++ String.fromInt newNode.node.id) transitionToString |> \_ ->
                let
                  current_transitions =
                    data.transitions ++ [TransitionTakenData newNode.node.id t]
                  ( advance_computation, actual_taken, renamed_node ) =
                    cull not_taken_last data.computation
                    |> \g -> expand g data.resolutionDict newNode.node.id current_transitions
                in
                  case renamed_node of
                    Nothing ->
                      { data
                        | finalResult =
                            Just <|
                              InternalError "Somehow, just expanding the computation without taking new paths has failed!!  What's going on here??"
                      }
                    Just nodeCtx ->
                      { data
                        | transitions = actual_taken
                            -- |> debugLog_ "[oneTransition] transitions-taken final" (List.map (\{dest,matching} -> transitionToString matching ++ "➡" ++ String.fromInt dest))
                          , remainingData = remainingData
                          , currentNode = nodeCtx.node.id
                          , computation = -- expand one level in advance.
                              advance_computation
                          , finalResult =
                              case remainingData of
                                [] ->
                                  if t.isFinal then Just Accepted
                                  else Just Rejected
                                _ ->
                                  -- we can plausibly continue on, so there is no
                                  -- final determination at this point in the time.
                                  Nothing
                          , step = data.step + 1
                      }

step : ExecutionData -> ExecutionData
step executionData =
  case executionData.finalResult {- |> Debug.log "Step with" -} of
    Just _ ->
      -- we have a final result; there's nothing more for us to do, thanks.
      executionData
    Nothing ->
      oneTransition executionData

run : ExecutionData -> List ExecutionData
run start =
  debugLog_ ("[run] '" ++ Maybe.withDefault "(no desc.)" start.computation.description ++ "' with") (.remainingData >> String.fromList) start |> \_ ->
  List.reverse <|
    start ::
    ( List.unfoldr
        (\current ->
          case current.finalResult of
            Just _ ->
              Nothing
            Nothing ->
              let next = step current in
              Just (next, next)
        )
        start
    )

load : String -> ResolutionDict -> AutomatonGraph -> ExecutionData
load s resolutionDict g =
  let
    initial =
      expand g resolutionDict g.root []
      |> (\(a, _, _) -> a)
  in
  -- step
    { transitions = []
    , remainingData = String.toList s
    , currentNode = initial.root
    , computation = initial
    , resolutionDict = resolutionDict
    , finalResult = Nothing
    , step = 0
    }

removeDisconnectedNodes : AutomatonGraph -> AutomatonGraph
removeDisconnectedNodes g =
  { g
    | graph =
        -- first, actually remove all disconnected nodes.
        identifyDisconnectedNodes g
        -- |> Debug.log "Disconnected nodes identified"
        |> Set.foldl Graph.remove g.graph
  }

identifyDisconnectedNodes : AutomatonGraph -> Set NodeId
identifyDisconnectedNodes g =
  Graph.mapContexts
    (\ctx ->
      { ctx
        | incoming = IntDict.filter (\_ -> not << AutoSet.isEmpty) ctx.incoming
        , outgoing = IntDict.filter (\_ -> not << AutoSet.isEmpty) ctx.outgoing
      }
    )
    g.graph
  |> Graph.guidedDfs
    Graph.alongOutgoingEdges
    (\_ acc -> (acc, identity))
    [g.root]
    ()
  |> Tuple.second
  |> Graph.nodeIds
  |> Set.fromList

automatonGraph_union : AutomatonGraph -> AutomatonGraph -> AutomatonGraph
automatonGraph_union g1 g2 =
  let
    dfa_1 = fromAutomatonGraph g1
    dfa_2 = fromAutomatonGraph g2
  in
    toAutomatonGraph (union dfa_1 dfa_2)

extend : DFARecord a -> DFARecord a -> ExtDFA
extend w_dfa_orig dfa = -- parameters: the w_dfa and the dfa
  let
    max_dfa =
      IntDict.findMax dfa.states
      |> Maybe.map (Tuple.first >> (+) 1)
      |> Maybe.withDefault 0
    max_w_dfa =
      IntDict.findMax w_dfa_orig.states
      |> Maybe.map (Tuple.first >> (+) 1)
      |> Maybe.withDefault 0
    extDFA =
      { states = dfa.states
        -- we amend this so that it has transition functions for cloned q0
      , transition_function = dfa.transition_function
      , start = dfa.start
      , finals = dfa.finals
        -- the register, correctly, does NOT include the cloned q0
      , register = Set.fromList <| IntDict.keys dfa.states
      , clone_start = w_dfa_orig.start
      , queue_or_clone = [] -- IntDict.keys w_dfa.states |> List.reverse
      , unusedId = max_dfa + max_w_dfa + 1
      , w_dfa_orig =
          { states =
              IntDict.toList w_dfa_orig.states
              |> List.map (\(a, b) -> (a + max_dfa, b))
              |> IntDict.fromList
          , transition_function =
              IntDict.toList w_dfa_orig.transition_function
              |> List.map
                (\(a, d) ->
                  ( a + max_dfa
                  , AutoDict.map (\_ b -> b + max_dfa) d
                  )
                )
              |> IntDict.fromList
          , start = w_dfa_orig.start + max_dfa
          , finals =
              Set.map (\a -> a + max_dfa) w_dfa_orig.finals
          }
          -- |> debugDFA_ "w_dfa_orig"
      }
  in
    extDFA

retract : ExtDFA -> DFARecord {}
retract extDFA =
  { states = extDFA.states
  , transition_function = extDFA.transition_function
  , start = extDFA.start
  , finals = extDFA.finals
  }

{-
This function is to get the "forward" transitions of a word that is
being added.  It is used, initially, to add the correct cloned/queued
states to a DFA in Phase 1 of union/intersection.  Then, in Phase 3,
it is used to guide the remove-unreachable algorithm to the correct
states that might need to be removed.

However, the word "forward" is misleading.  In fact, the point is that
we shouldn't have _BACKWARD_ transitions (or recursive ones), because
then our algorithms might get trapped in a loop.

Consider

t ----------> e -> s
 \            ↑
  `-> k -> p -'

t -> k -> p -> e is a fair path forward.  So is t -> e.  I actually
should be looking at both of them, in terms of forward routes.  And
this is why I cannot return a `List Char` from this function, because
I must consider multiple routes.  Instead, I need to return and use
a tree structure.
-}
-- Tree structure for all possible forward transition paths

type ForwardTree = PathEnd | ForwardNode (AutoDict.Dict String AcceptVia ForwardTree)

printForwardTree : ForwardTree -> String
printForwardTree fwd_tree =
  case fwd_tree of
    PathEnd -> "•"
    ForwardNode dict ->
      AutoDict.toList dict
      |> List.map
        (\(acceptCondition, nextTree) ->
          "(" ++ printableAcceptCondition acceptCondition ++ ") -> [" ++ printForwardTree nextTree ++ "]"
        )
      |> String.join ", "

debugForwardTree : String -> ForwardTree -> ForwardTree
debugForwardTree msg =
  debugLog_ msg printForwardTree

{-| Return the nodes of the forwardtree, in-order.
-}
forwardtree_nodes : NodeId -> ExtDFA -> ForwardTree -> List NodeId
forwardtree_nodes start extDFA_orig tree_ =
  let
    nodes_of_forwardtree : ForwardTree -> List NodeId -> List NodeId
    nodes_of_forwardtree tree acc =
      case (acc, tree) of
        ( [], _ ) ->
          -- this is an impossible state to get into, because of how
          -- the initial call is made.
          acc
        ( _, PathEnd ) ->
          acc
        ( h::_, ForwardNode dict ) ->
          -- follow all the possible forward-paths from this node.
          AutoDict.foldl
            (\acceptCondition nextTree state ->
              IntDict.get h extDFA_orig.transition_function
              |> Maybe.andThen
                (\destinations ->
                  AutoDict.get acceptCondition destinations
                  |> Maybe.map (\q -> nodes_of_forwardtree nextTree (q::state))
                )
              -- if there are no outgoing transitions, there is no path to follow; we are done.
              |> Maybe.withDefault acc
            )
            acc
            dict
  in
    nodes_of_forwardtree tree_ [start]
    |> List.reverse

delta : NodeId -> AcceptVia -> DFARecord a -> Maybe NodeId
delta q x dfa =
  IntDict.get q dfa.transition_function
  |> Maybe.andThen (AutoDict.get x)
  |> (\v ->
    case v of
      Nothing ->
        Debug.log ("[delta] δ(" ++ String.fromInt q ++ ", " ++ printableAcceptCondition x ++ ") = ∅") () |> \_ ->
        v
      Just q_ ->
        Debug.log ("[delta] δ(" ++ String.fromInt q ++ ", " ++ printableAcceptCondition x ++ ") = " ++ String.fromInt q_) () |> \_ ->
        v
  )

{-| Returns a list of (NodeId, Char) pairs representing all transitions in the given DFA that lead to the specified node.

    transitions_to targetNode dfa

- `targetNode`: The NodeId to which transitions are sought.
- `dfa`: The DFARecord to within.

Each pair consists of the source NodeId and the character label of the transition.
-}
transitions_to_dest : NodeId -> DFARecord a -> List (NodeId, AcceptVia)
transitions_to_dest q dfa =
  IntDict.foldl
    (\source dict acc ->
      AutoDict.foldl
        (\ch target acc_ ->
          if target == q then
            (source, ch) :: acc_
          else
            acc_
        )
        acc
        dict
    )
    []
    dfa.transition_function

clone_or_queue : NodeId -> NodeId -> ExtDFA -> ExtDFA
clone_or_queue q_m q_w extDFA =
  { extDFA
    | transition_function =
        case ( IntDict.get q_w extDFA.transition_function, IntDict.get q_m extDFA.transition_function ) of
          ( Just a, Nothing ) ->
            IntDict.insert q_w a extDFA.transition_function
          ( Nothing, Just b ) ->
            IntDict.insert q_w b extDFA.transition_function
          ( Nothing, Nothing) ->
            extDFA.transition_function
          ( Just a, Just b ) ->
            IntDict.insert q_w
              (AutoDict.merge
                (\x y -> Basics.compare (acceptConditionToString x) (acceptConditionToString y))
                -- left only
                (\k v acc -> AutoDict.insert k (v |> Debug.log "left only") acc)
                -- both
                (\k v _ acc -> AutoDict.insert k (v |> Debug.log "BOTH; taking left") acc)
                -- right only
                (\k v acc -> AutoDict.insert k (v |> Debug.log "right only") acc)
                (AutoDict.empty acceptConditionToString)
                a b
              )
              extDFA.transition_function
    , finals =
        if Set.member q_m extDFA.finals then
          Set.insert q_w extDFA.finals
        else
          extDFA.finals
   }

clone_or_queue_many : NodeId -> NodeId -> ForwardTree -> ExtDFA -> ExtDFA
clone_or_queue_many q_m q_w tree extDFA =
  case tree of
    PathEnd ->
      -- we're at the end of the transitions.
      clone_or_queue q_m q_w extDFA
      |> debugExtDFA_ ("[clone_or_queue_many] end of path, (q_m = #" ++ String.fromInt q_m ++ ", q_w = #" ++ String.fromInt q_w ++ ")")
    ForwardNode dict ->
      AutoDict.foldl
        (\ch subtree acc ->
          case (delta q_m ch acc, delta q_w ch acc) |> Debug.log ("(δ(q_m, " ++ printableAcceptCondition ch ++ "), δ(q_w, " ++ printableAcceptCondition ch ++ "))") of
            (_, Nothing) ->
              Debug.log "🚨 ERROR!! How can I fail to get a `w` transition via known `w`-transitions??" () |> \_ ->
              acc
            (Nothing, Just _) ->
              -- The q_m ends here, but q_w carries on. ∴ the remaining q_w must be "queued" nodes.
              clone_or_queue q_m q_w acc
              |> debugExtDFA_ ("[clone_or_queue_many] queued, (q_m = #" ++ String.fromInt q_m ++ ", q_w = #" ++ String.fromInt q_w ++ ")")
            (Just m_node, Just w_node) ->
              clone_or_queue_many m_node w_node subtree (clone_or_queue q_m q_w acc)
              |> debugExtDFA_ ("[clone_or_queue_many] cloned subtree starting at m = #" ++ String.fromInt m_node ++ ", w = #" ++ String.fromInt w_node)
        )
        extDFA
        dict

state_key : { a | q_m : Maybe NodeId, q_w : Maybe NodeId } -> String
state_key { q_m, q_w } =
  case (q_m, q_w) of
    (Nothing, Nothing) ->
      "/"
    (Just q_m_, Nothing) ->
      String.fromInt q_m_ ++ "/"
    (Nothing, Just q_w_) ->
      "/" ++ String.fromInt q_w_
    (Just q_m_, Just q_w_) ->
      String.fromInt q_m_ ++ "/" ++ String.fromInt q_w_

build_out : List { q_m : Maybe NodeId, q_w : Maybe NodeId } -> AutoSet.Set String { q_m : Maybe NodeId, q_w : Maybe NodeId  } -> AutoDict.Dict String { q_m : Maybe NodeId, q_w : Maybe NodeId } NodeId -> ExtDFA -> ExtDFA
build_out node_stack handled mapping extDFA =
  let
    head_id_for head =
      AutoDict.get head mapping
      |> Maybe.withDefaultLazy
        (\() ->
          Debug.log "🚨 ERROR!! How can I fail to get a head-mapping?? A previous build_out MUST have added one!" -1
        )
    head_mapping_for head q__w =
      head_id_for head
      |> \id ->
        -- TODO: What to do when BOTH of them have an Entity?
        -- Right now, I'm just taking `w`'s entity. This, of course, is wrong.
        -- (at least partly because it means union is not commutative)
        IntDict.get q__w extDFA.w_dfa_orig.states
        |> Maybe.map (\state -> { state | id = id })
        |> Maybe.withDefaultLazy
          (\() ->
            Entity 0 0 0 0 id NoEffect
            |> Debug.log "🚨 ERROR!! How can I fail to get a known state from `w_orig_dfa`??"
          )
    print_node_stack =
      List.map state_key >> String.join ", "
    print_state someState =
      case someState of
        Just x -> String.fromInt x
        Nothing -> "⊥"
  in
  case node_stack {- |> debugLog_ "[build_out] node_stack" print_node_stack -} of
    [] -> extDFA
    ({ q_m, q_w } as head) :: rest ->
      if AutoSet.member head handled then
        -- we have seen & dealt with this one before.
        -- Debug.log ("[build_out] I have already handled (Qm=" ++ print_state q_m ++ ", Qw=" ++ print_state q_w ++ "); pushing to top of list, then moving on.") () |> \_ ->
        build_out rest handled mapping
          { extDFA
            | queue_or_clone =
                -- if I am handling this later on, then I will push it to the start of the list.
                case AutoDict.get head mapping of
                  Just node ->
                    node :: List.filter ((/=) node) extDFA.queue_or_clone
                  Nothing ->
                    Debug.log ("[build_out] 🚨 ERROR! I should have a mapping, because I have handled this. What's going on??") () |> \_ ->
                    extDFA.queue_or_clone
          }
      else
        case (q_m, q_w) of
          (Just q__m, Just q__w) ->
            -- Both states are valid.  Now, find the transitions from these,
            -- and push them onto the stack.  When we're done, mark handled and
            -- recurse.
            let
              transitions_m =
                IntDict.get q__m extDFA.transition_function
                |> Maybe.withDefault (AutoDict.empty acceptConditionToString)
              transitions_w =
                IntDict.get q__w extDFA.w_dfa_orig.transition_function
                |> Maybe.withDefault (AutoDict.empty acceptConditionToString)
              all_transitions_out =
                (AutoDict.keys transitions_m ++ AutoDict.keys transitions_w)
                |> List.uniqueBy acceptConditionToString
                -- |> Debug.log ("[build_out (Qm=" ++ String.fromInt q__m ++ ", Qw=" ++ String.fromInt q__w ++ ")] Transitions out")
              resulting_combination_states =
                List.map
                  (\via ->
                      ( via
                      , { q_m = AutoDict.get via transitions_m
                        , q_w = AutoDict.get via transitions_w
                        }
                      )
                  )
                  all_transitions_out
                |> AutoDict.fromList acceptConditionToString
              resulting_combination_states_excluding_handled =
                AutoDict.values resulting_combination_states
                -- List.filter
                --   (\v -> not <| AutoSet.member v handled)
                --   (AutoDict.values resulting_combination_states)
              new_node_stack =
                rest ++ resulting_combination_states_excluding_handled
              new_handled =
                AutoSet.insert head handled
              (new_mapping, unusedId) =
                List.foldl
                  (\k (acc, unused) ->
                    case AutoDict.get k acc of
                      Just _ ->
                        -- okay, this already has a mapping; just carry on
                        (acc, unused)
                      Nothing ->
                        -- this is a new one; give it a new ID
                        case (k.q_w, k.q_m) of
                          (Nothing, Nothing) ->
                              -- don't bother with a mapping; this is an absorption state.
                              (acc, unused)
                          (Nothing, Just existing_m) ->
                              -- this is already in the register; just reference it.
                              -- In Carrasco & Forcada's algorithm, when we have a (Wx, Mx) pair
                              -- that transitions to (⊥, ??), then we just redirect to the existing
                              -- node in the register.
                              (AutoDict.insert k existing_m acc, unused)
                          (Just _, Nothing) ->
                            -- When we have (Wx, Mx) leading to (??, ⊥), then we have a backlink that
                            -- Carrasco & Forcada's algorithm does NOT handle properly.  And we will
                            -- make another state for that.
                            (AutoDict.insert k unused acc, unused + 1)
                          (Just _, Just _) ->
                            -- this is going to be a state that we "follow" in `m`.  We will deal
                            -- with it on the stack.
                            (AutoDict.insert k unused acc, unused + 1)
                  )
                  (mapping, extDFA.unusedId)
                  resulting_combination_states_excluding_handled
              -- okay!  By now, we have all states mapped.  So we can make the transitions
              -- out of here.
              head_mapping = head_mapping_for head q__w
              new_transitions =
                all_transitions_out
                |> List.map
                    (\via ->
                      ( via
                      , AutoDict.get via resulting_combination_states
                        |> Maybe.withDefaultLazy
                          (\() ->
                            Debug.log "🚨 ERROR!! How can I fail to get a known-good via-mapping??"
                              { q_w = Nothing, q_m = Nothing }
                          )
                        |>  (\rawKey ->
                                AutoDict.get rawKey new_mapping
                                |> Maybe.withDefaultLazy
                                  (\() ->
                                    Debug.log "🚨 ERROR!! How can I fail to get a known-good q_m+q_w-mapping??" -1
                                  )
                            )
                      )
                    )
                |> AutoDict.fromList acceptConditionToString
              -- and we need to set as final if either q_m or q_w is final.
            in
              build_out
                new_node_stack
                new_handled
                new_mapping
                ({ extDFA
                  | transition_function =
                      if AutoDict.isEmpty new_transitions then
                        extDFA.transition_function
                      else
                        IntDict.insert head_mapping.id new_transitions extDFA.transition_function
                  , states = IntDict.insert head_mapping.id head_mapping extDFA.states
                  , unusedId = unusedId
                  , queue_or_clone = head_mapping.id :: extDFA.queue_or_clone
                  , finals =
                      if Set.member q__m extDFA.finals || Set.member q__w extDFA.w_dfa_orig.finals then
                        Set.insert head_mapping.id extDFA.finals
                      else
                        extDFA.finals
                }
                -- |> debugExtDFA_ "After build_out"
                )
          (Just q__m, Nothing ) ->
            -- this state only exists in q_m. So I don't need to worry about anything from q_w.
            -- q_m should already have the necessary transitions for this; so I can ignore.
            -- println ("[build_out (Qm=" ++ String.fromInt q__m ++ ", Qw=⊥)] State only exists in Qm, so ignoring it; existing transitions remain.")
            build_out rest (AutoSet.insert head handled) mapping extDFA
          ( Nothing, Just q__w) ->
            -- this state only exists in q_w; in Carrasco & Forcada's terminology, it becomes a 
            -- "queue" state and gets added to the `queue_or_clone` list.  Once q_m is out of the
            -- picture, it's only via a backlink that I can possibly get back to any q_m-relevant
            -- node.
            -- We will bump transitions from this onto the stack so that they get the same treatment
            -- as "queued" things later on.
            let
              transitions_w =
                IntDict.get q__w extDFA.w_dfa_orig.transition_function
                |> Maybe.withDefault (AutoDict.empty acceptConditionToString)
              all_transitions_out =
                AutoDict.keys transitions_w
                |> List.uniqueBy acceptConditionToString
                -- |> Debug.log ("[build_out (Qm=⊥, Qw=" ++ String.fromInt q__w ++ ")] Transitions out")
              resulting_combination_states =
                List.map
                  (\via ->
                      ( via
                      , { q_m = Nothing
                        , q_w = AutoDict.get via transitions_w
                        }
                      )
                  )
                  all_transitions_out
                |> AutoDict.fromList acceptConditionToString
              resulting_combination_states_excluding_handled =
                AutoDict.values resulting_combination_states
                -- List.filter
                --   (\v -> not <| AutoSet.member v handled)
                --   (AutoDict.values resulting_combination_states)
              new_node_stack =
                rest ++ resulting_combination_states_excluding_handled
              new_handled =
                AutoSet.insert head handled
              (new_mapping, unusedId) =
                List.foldl
                  (\k (acc, unused) ->
                    case AutoDict.get k acc of
                      Just _ ->
                        -- okay, this already has a mapping; just carry on
                        (acc, unused)
                      Nothing ->
                        -- this is a new one; give it a new ID
                        case (k.q_w, k.q_m) of
                          (Nothing, Nothing) ->
                              -- don't bother with a mapping; this is an absorption state.
                              (acc, unused)
                          (Nothing, Just existing_m) ->
                              -- this is already in the register; just reference it.
                              -- In Carrasco & Forcada's algorithm, when we have a (Wx, Mx) pair
                              -- that transitions to (⊥, ??), then we just redirect to the existing
                              -- node in the register.
                              (AutoDict.insert k existing_m acc, unused)
                          (Just _, Nothing) ->
                            -- When we have (Wx, Mx) leading to (??, ⊥), then we have a backlink that
                            -- Carrasco & Forcada's algorithm does NOT handle properly.  And we will
                            -- make another state for that.
                            (AutoDict.insert k unused acc, unused + 1)
                          (Just _, Just _) ->
                            -- this is going to be a state that we "follow" in `m`.  We will deal
                            -- with it on the stack.
                            (AutoDict.insert k unused acc, unused + 1)
                  )
                  (mapping, extDFA.unusedId)
                  resulting_combination_states_excluding_handled
              -- okay!  By now, we have all states mapped.  So we can make the transitions
              -- out of here.
              head_mapping = head_mapping_for head q__w
              new_transitions =
                all_transitions_out
                |> List.map
                    (\via ->
                      ( via
                      , AutoDict.get via resulting_combination_states
                        |> Maybe.withDefaultLazy
                          (\() ->
                            Debug.log "🚨 ERROR!! How can I fail to get a known-good via-mapping??"
                              { q_w = Nothing, q_m = Nothing }
                          )
                        |>  (\rawKey ->
                                AutoDict.get rawKey new_mapping
                                |> Maybe.withDefaultLazy
                                  (\() ->
                                    Debug.log "🚨 ERROR!! How can I fail to get a known-good q_m+q_w-mapping??" -1
                                  )
                            )
                      )
                    )
                |> AutoDict.fromList acceptConditionToString
              -- and we need to set as final if either q_m or q_w is final.
            in
              build_out
                new_node_stack
                new_handled
                new_mapping
                ({ extDFA
                  | transition_function =
                      if AutoDict.isEmpty new_transitions then
                        extDFA.transition_function
                      else
                        IntDict.insert head_mapping.id new_transitions extDFA.transition_function
                  , states = IntDict.insert head_mapping.id head_mapping extDFA.states
                  , unusedId = unusedId
                  , queue_or_clone = head_mapping.id :: extDFA.queue_or_clone
                  , finals =
                      if Set.member q__w extDFA.w_dfa_orig.finals then
                        Set.insert head_mapping.id extDFA.finals
                      else
                        extDFA.finals
                }
                -- |> debugExtDFA_ "After build_out"
                )
          ( Nothing, Nothing ) ->
            -- Unequivocal absorption state.  Nothing can possibly lead from here.
            -- println "[build_out (⊥, ⊥)] Absorption state"
            build_out rest (AutoSet.insert head handled) mapping extDFA

phase_1 : ExtDFA -> ExtDFA
phase_1 extDFA_orig =
  -- Traverse the ForwardTree and apply append_transitions for every path
  -- going to do this the old-fashioned product way.
  -- The phase-1 algorithm in Carrasco & Forcada works for all words, but not necessarily for all
  -- DFAs.
  let
    root = { q_m = Just extDFA_orig.start, q_w = Just extDFA_orig.w_dfa_orig.start }
    initialSet = [ root ]
    handled = AutoSet.empty state_key
    mapping = AutoDict.singleton state_key root extDFA_orig.unusedId
  in
    build_out initialSet handled mapping
      { extDFA_orig
        | unusedId = extDFA_orig.unusedId + 1
        , clone_start = extDFA_orig.unusedId
      }
  -- By the end, of this, I want:
  -- 1. All nodes to be integrated; there are none that are separate.
  -- 2. The `finals` set is correct as far as both `w` and `m` are concerned.
  -- 3. The `queue_or_clone` list is present, and in the correct order
  --    (i.e. back-to-front node order, for checking right-language)

remove_unreachable : ExtDFA -> ExtDFA
remove_unreachable extDFA =
  -- can I make this even simpler?
  -- Go along the graph from the root; throw the edges into a set; stop when
  -- we encounter an edge again.
  -- Now get rid of all the edges that aren't in the set.
  let
    {-
      I need to track by (src, via, dest).
      I cannot just track by (via, dest).
      Consider: (0, a, 1) (1, b, 2) (2, a, 1) [1]
      Suddenly, that last transition doesn't get counted…
    -}
    helper : NodeId -> AutoSet.Set (Int, String, Int) (NodeId, AcceptVia, NodeId) -> AutoSet.Set (Int, String, Int) (NodeId, AcceptVia, NodeId)
    helper current seen =
      case IntDict.get current extDFA.transition_function of
        Nothing -> seen
        Just dict ->
          AutoDict.foldl
            (\via dest acc ->
              if AutoSet.member (current, via, dest) seen then
                acc -- seen it already; ignore.
              else
                AutoSet.union
                  acc
                  (helper dest (AutoSet.insert (current, via, dest) acc))
            )
            seen
            dict
    referenced_nodes =
      helper extDFA.start (AutoSet.empty (\(a,b,c) -> ( a, acceptConditionToString b, c )))
      |> AutoSet.foldl
          (\(i, _, j) acc -> Set.insert i (Set.insert j acc))
          Set.empty
    new_extDFA =
      { extDFA
        | states =
            IntDict.filter
              (\id _ -> Set.member id referenced_nodes)
              extDFA.states
        , transition_function =
            IntDict.filter
              (\id _ -> Set.member id referenced_nodes)
              extDFA.transition_function
        , finals =
            Set.filter
              (\id -> Set.member id referenced_nodes)
              extDFA.finals
        , register =
            Set.filter
              (\id -> Set.member id referenced_nodes)
              extDFA.register
        , queue_or_clone =
            List.filter
              (\id -> Set.member id referenced_nodes)
              extDFA.queue_or_clone
      }
  in
    new_extDFA

type alias RecursionState =
  -- via, (fromP, fromQ), (toP, toQ)
  { taken : AutoSet.Set (String, (NodeId, NodeId), (NodeId, NodeId)) (AcceptVia, (NodeId, NodeId), (NodeId, NodeId))
  }

replace_or_register : ExtDFA -> ExtDFA
replace_or_register extDFA_ =
  let
    pinpoint_key : ExtDFA -> NodeId -> List String
    pinpoint_key extDFA node =
      IntDict.get node extDFA.transition_function
      |> Maybe.map (\dict -> AutoDict.keys dict |> List.map acceptConditionToString)
      |> Maybe.withDefault [] -- empty string = no outbounds
    pinpoint_database_orig : Dict (List String) (List NodeId)
    pinpoint_database_orig =
      -- this will get all the nodes that have outbounds.
      let
        with_outbounds =
          extDFA_.transition_function
          |> IntDict.toList
          |> List.map
            (\(source, outboundDict) ->
              (AutoDict.keys outboundDict |> List.map acceptConditionToString, source)
            )
          |> List.gatherEqualsBy Tuple.first
          |> List.map
            (\((key, headValue), rest) ->
              ( key
              , headValue :: List.map Tuple.second rest
              )
            )
          |> Dict.fromList
        add_without_outbounds =
          extDFA_.register
          |> Set.union (Set.fromList extDFA_.queue_or_clone)
          |> Set.toList
          |> List.filterMap
            (\id ->
              if IntDict.member id extDFA_.transition_function then
                Nothing
              else
                Just id
            )
          |> (\ids -> Dict.insert [] ids with_outbounds)
      in
        add_without_outbounds
    takenTupleKey (via, from, to) =
      ( acceptConditionToString via, from, to )
    equiv : ExtDFA -> NodeId -> NodeId -> Dict (NodeId, NodeId) (Maybe Bool) -> (Bool, Dict (NodeId, NodeId) (Maybe Bool))
    equiv extDFA p_ q_ seen_ =
      -- KEY for EQUIVALENCE:
      -- 1. Same finality;
      -- 2. Same outward transitions.
      -- …and that is all.
      let
        different_finality a b = xor (Set.member a extDFA.finals) (Set.member b extDFA.finals)
        get_outgoing : NodeId -> Maybe (List (AcceptVia, NodeId))
        get_outgoing nodeid =
          IntDict.get nodeid extDFA.transition_function
          |> Maybe.map AutoDict.toList
        check_equiv : NodeId -> NodeId -> RecursionState -> Dict (NodeId, NodeId) (Maybe Bool) -> Dict (NodeId, NodeId) (Maybe Bool)
        check_equiv p q recursionData seen =
          let
            dbg_prefix = "[equiv (#" ++ String.fromInt p ++ ", #" ++ String.fromInt q ++ ")] "
          in
          case Dict.get (p, q) seen of
            Just (Just result) ->
              -- Debug.log (dbg_prefix ++ "cached result") result |> \_ ->
              seen
            Just Nothing ->
              -- the only way that this can happen is if, in fact, we're busy going through and we run
              -- into a cycle.  In that case, just stop the cycle here; and we'll assign later on, in
              -- the fold that ended up calling us.
              Debug.todo "Hmmmmmmmmmmmmm I should not be here!"
              -- println (dbg_prefix ++ "indeterminate/cycle result")
              -- seen
            Nothing -> -- This is the first time that I've ever been here.
              if different_finality p q then
                -- println (dbg_prefix ++ "not equivalent (finality differs)")
                Dict.insert (p, q) (Just False) seen
              else
                case (get_outgoing p, get_outgoing q) of
                  (Just _, Nothing) ->
                    -- println (dbg_prefix ++ "not equivalent (p-outgoing > q-outgoing)")
                    Dict.insert (p, q) (Just False) seen
                  (Nothing, Just _) ->
                    -- println (dbg_prefix ++ "not equivalent (q-outgoing > p-outgoing)")
                    Dict.insert (p, q) (Just False) seen
                  (Nothing, Nothing) ->
                    -- println (dbg_prefix ++ "equivalent (identical (zero) outgoing)")
                    Dict.insert (p, q) (Just True) seen
                  (Just a_out, Just b_out) ->
                    if a_out == b_out then
                      -- println (dbg_prefix ++ "equivalent (identical outgoing)")
                      Dict.insert (p, q) (Just True) seen
                    else
                      let
                        a_transitions = List.map Tuple.first a_out
                        b_transitions = List.map Tuple.first b_out
                      in
                        if a_transitions == b_transitions then
                          let
                            outbound_folded = -- The result is a `Dict`.
                              -- println (dbg_prefix ++ "---> determining recursively…")
                              List.stoppableFoldl
                                (\((via, out_a), (_, out_b)) state ->
                                  let
                                    takenTuple = (via, (p, q), (out_a, out_b))
                                    updated_seen =
                                      if AutoSet.member takenTuple recursionData.taken then
                                        -- I'm in a cycle. And that's fine; I've explored this path fully if I'm here.
                                        -- So let me go back, and then we can look at others.
                                        -- println (dbg_prefix ++ "cancelling recursion for transition " ++ acceptConditionToString via)
                                        state
                                      else
                                        -- println (dbg_prefix ++ "---> for both, following " ++ acceptConditionToString via)
                                        check_equiv
                                          out_a
                                          out_b
                                          { recursionData
                                            | taken =
                                                AutoSet.insert
                                                  (via, (p, q), (out_a, out_b))
                                                  recursionData.taken
                                          }
                                          state
                                  in
                                    case Dict.get (out_a, out_b) updated_seen of
                                      Nothing ->
                                        -- So if I reach here, then it is because recursion has been cancelled.
                                        -- Now, recursion is ONLY cancelled if I happen to have taken that exact path before;
                                        -- and I will only end up HERE when that has happened for ALL the explorable transitions
                                        -- in the subsequent transitions.
                                        -- And THAT, in turn, can only happen—without a False value—when:
                                        -- 1. All the finality matches; and
                                        -- 2. All the transitions match.
                                        -- … so this is—pending confirmation from other paths to be explored—potentially True!
                                        Continue updated_seen
                                      Just Nothing ->
                                        -- this ended in a cycle.  Okay, we will also return a cycle, to be resolved later on.
                                        Continue updated_seen
                                      Just (Just True) ->
                                        -- excellent! Now, we need to check the rest of them, because
                                        -- ALL the outgoings need to match.
                                        Continue updated_seen
                                      Just (Just False) ->
                                        -- nope.  We're done.
                                        -- println (dbg_prefix ++ " ---> not equivalent (determined recursively)")
                                        Stop ( Dict.insert (out_a, out_b) (Just False) updated_seen |> Dict.insert (p, q) (Just False) )
                                )
                                seen
                                (List.zip a_out b_out)
                          in
                            -- When I am done with the outbound_fold, I should be able to tell whether or not we have equivalence.
                            -- If I ever got a `False` result, then I would stop immediately, and I would also immediately
                            -- set (a, b) to `False`.  And that is the only case when I would set (a, b) to anything definite.
                            -- Now, if I am done with the fold, and I _don't_ see a `False` for (a, b), then I must have
                            -- completed everything WITHOUT running into any issues.  So I can return a `True` instead.
                            case Dict.get (p, q) outbound_folded of
                              Just (Just False) ->
                                -- ONE of the outbounds did not match.
                                -- And every outbound which I set to `Nothing` on the way resulted
                                -- in a cycle back to here.
                                -- What should I do here??
                                -- Setting all to False would be incorrect.
                                -- Obviously, setting them to True would be nonsensical.
                                -- Let me leave them as "I lead to a cycle", and then we can save time and not travel
                                -- those paths again in future; and other paths will be determinative, but
                                -- these paths will not be.
                                outbound_folded
                              _ ->
                                -- by contrast…
                                -- println (dbg_prefix ++ " ---> equivalent (determined recursively)")
                                Dict.insert (p, q) (Just True) outbound_folded
                        else
                          -- the outward transitions do not match.
                          Dict.insert (p, q) (Just False) seen
        starting_check =
          check_equiv p_ q_ (RecursionState (AutoSet.empty takenTupleKey)) seen_
      in
        if p_ == q_ then
          ( False, seen_ ) -- can't be equivalent to yourself in this context.
        else
          case Dict.get (p_, q_) starting_check of
            Just (Just result) ->
              ( result, starting_check )
            Just Nothing ->
              -- … pure cycle?!?  For now, let's call this impossible.  And we'll come back if we
              -- ever find a counterexample…
              Debug.todo "The impossibru Ⅲ has happened."
            Nothing ->
              Debug.todo "The impossibru Ⅱ has happened."
    redirectInto : NodeId -> NodeId -> ExtDFA -> IntDict (AutoDict.Dict String AcceptVia NodeId)
    redirectInto target source extDFA =
      -- redirect everything that goes to source, into target
      -- println ("[replace_or_register] Redirecting all incomings from #" ++ String.fromInt source ++ " to #" ++ String.fromInt target ++ " instead.")
      IntDict.map
        (\_ dict ->
          AutoDict.map
            (\_ candidate ->
              if candidate == source then
                target
              else
                candidate
            )
            dict
        )
        extDFA.transition_function
    process : NodeId -> Dict (List String) (List Int) -> Dict (NodeId, NodeId) (Maybe Bool) -> ExtDFA -> (ExtDFA, Dict (NodeId, NodeId) (Maybe Bool), Dict (List String) (List Int))
    process qc_node_ db seen extDFA = -- qc_node == queue/clone-node
      let
        -- the expensive part of this is the comparisons.
        -- If we have 100,000 nodes in the register, and we compare against all
        -- of them, then we're going to have a LOT of work to do, and most
        -- comparisons will immediately fail.
        -- So, can we use a database to pinpoint only the relevant nodes,
        -- and compare against those?
        -- Let's begin by finding the correct key.
        db_key : List String
        db_key = pinpoint_key extDFA qc_node_
        -- and then the subset of nodes to look at are…
        to_examine : List NodeId
        to_examine =
          Dict.get db_key db
          |> Maybe.withDefaultLazy
            (\() ->
              Debug.log "🚨 returning DEFAULT EMPTY list for db_key; this should not happen!" db_key |> \_ ->
              []
            )
        -- let's look up things in the register.
        (merge_list, updated_seen) =
          List.foldl
            (\other_node (to_merge, state) ->
              case equiv extDFA qc_node_ other_node state of
                (True, updated_state) ->
                  ( other_node :: to_merge
                  , updated_state
                  )
                (False, updated_state) ->
                  ( to_merge
                  , updated_state
                  )
            )
            ( [], seen )
            ( to_examine
              -- |> debugLog_ "Number of nodes to examine" List.length
            )
        -- merge everything we need to merge
        update_extDFA_with_merge : NodeId -> NodeId -> Dict (List String) (List NodeId) -> ExtDFA -> (ExtDFA, Dict (List String) (List NodeId))
        update_extDFA_with_merge qc_node target db_ state =
          -- the `qc_node` is removed from the graph.
          -- All transitions that used to go to `qc_node` will now go to
          -- `target` instead, which is the equivalent-node found in the graph.
          ( { state
              | states = IntDict.remove qc_node state.states
              , finals = Set.remove qc_node state.finals
              , transition_function =
                  redirectInto target qc_node state
                  |> IntDict.remove qc_node
              , start =
                  if qc_node == state.start then
                    target -- replace with equivalent.
                  else
                    state.start
            }
          -- On the database side, we have a dictionary of outgoing-transitions → ids.
          -- Note well that while `target` now has more incoming edges,
          -- that does not mean that any other node has more outgoing edges.
          -- All other nodes retain exactly the same set of outgoing edges,
          -- but some of those edges are going to be redirected to `target`
          -- instead of `qc_node`.  So, I update the database to replace
          -- all instances of `qc_node` with `target`.
          , Dict.map
              (\_ ->
                List.map (\x -> if x == qc_node then target else x)
                >> List.unique
              )
              db_
          )

      in
        case merge_list of
          [] ->
            ( { extDFA
                | register =
                    Set.insert qc_node_ extDFA.register
                    -- |> Debug.log ("[replace_or_register] No equivalent for #" ++ String.fromInt qc_node_ ++ ". Updated register.")
              }
            , updated_seen
            , db
            )
          _ ->
            List.foldl
              (\target (source, state, db_) ->
                let
                  (updated_extDFA, updated_db) = 
                    update_extDFA_with_merge source target db_ state
                in
                  ( target
                  , updated_extDFA
                  , updated_db
                  )
              )
              ( qc_node_, extDFA, db )
              merge_list
            |> (\(_, a, b) ->
                  ( a
                    -- |> debugDFA_ ("[replace_or_register] Post-merges")
                  , updated_seen
                  , b
                  )
                )              
    continue_processing : ExtDFA -> Dict (List String) (List NodeId) -> Dict (NodeId, NodeId) (Maybe Bool) -> ExtDFA
    continue_processing extDFA db seen =
      case extDFA.queue_or_clone {- |> Debug.log "[replace_or_register] Queued/Cloned nodes remaining to process" -} of
        h::t ->
          let
            (updated_dfa, updated_seen, updated_db) = process h db seen extDFA
          in
            continue_processing { updated_dfa | queue_or_clone = t } updated_db updated_seen
        [] ->
          extDFA
  in
    continue_processing extDFA_ pinpoint_database_orig Dict.empty

union : DFARecord a -> DFARecord a -> DFARecord {}
union w_dfa_orig m_dfa =
  extend w_dfa_orig m_dfa
  -- |> debugExtDFA_ "[union] extDFA creation from merged w_dfa + dfa"
  |> phase_1
  -- |> debugExtDFA_ "[union] End of Phase 1 (clone-and-queue)"
  |> (\extdfa -> remove_unreachable { extdfa | start = extdfa.clone_start })
  -- |> debugExtDFA_ "[union] End of Phase 2 (remove-unreachable + switch-start)"
  |> replace_or_register
  -- |> debugExtDFA_ "[union] End of Phase 3 (replace-or-register)"
  |> retract


empty : Entity -> DFARecord {}
empty defaultValue =
  { states = IntDict.singleton 0 defaultValue
  , transition_function = IntDict.empty
  , start = 0
  , finals = Set.empty
  }

debugFan_ : String -> IntDict Connection -> IntDict Connection
debugFan_ s fan =
  debugLog_ s printFan fan

minimisation_merge : NodeId -> NodeId -> AutomatonGraph -> AutomatonGraph
minimisation_merge head other g =
  let
    redirectFan : NodeId -> NodeId -> IntDict Connection -> IntDict Connection
    redirectFan from to fan =
      case IntDict.get from fan of
        Just conn ->
          case IntDict.get to fan of
            Just conn2 ->
              IntDict.insert to (AutoSet.union conn conn2) fan
              |> IntDict.remove from
            Nothing ->
              IntDict.insert to conn fan
              |> IntDict.remove from
        Nothing ->
          fan
  in
    Maybe.map2
      (\headNode otherNode ->
        let
          updatedIncoming =
            IntDict.uniteWith
              (\_ -> AutoSet.union)
              (redirectFan other head headNode.incoming {- |> debugFan_ "[minimisation_merge] headNode.incoming (redir)" -})
              (redirectFan other head otherNode.incoming {- |> debugFan_ "[minimisation_merge] otherNode.incoming (redir)"-})
            -- |> debugFan_ "[minimisation_merge] merged incoming"
          updatedOutgoing =
            IntDict.uniteWith
              (\_ -> AutoSet.union)
              (redirectFan other head headNode.outgoing {- |> debugFan_ "[minimisation_merge] headNode.outgoing (redir)" -})
              (redirectFan other head otherNode.outgoing {- |> debugFan_ "[minimisation_merge] otherNode.outgoing (redir)" -})
            -- |> debugFan_ "[minimisation_merge] merged outgoing"
        in
          { g
            | graph =
                Graph.insert { headNode | incoming = updatedIncoming , outgoing = updatedOutgoing } g.graph
                |> Graph.remove other
            , root = if other == g.root then head else g.root
          }
      )
      (Graph.get head g.graph)
      (Graph.get other g.graph)
    |> Maybe.withDefault g
    -- |> debugAutomatonGraph ("[minimisation_merge] Post merge of #" ++ String.fromInt head ++ " and #" ++ String.fromInt other)

minimiseNodesByCombiningTransitions : AutomatonGraph -> AutomatonGraph
minimiseNodesByCombiningTransitions g_ =
  {-
    The Graph representation encodes finality via transitions (e.g. (ch, 0) for
    a non-final state and (ch, 1) for a final state). I convert that graph
    representation into a "standard" DFA so that I can apply the minimisation
    algorithm and, ideally, any other algorithms relating to standard DFAs from
    the literature.  However, a standard DFA encodes finality via states.  This
    means that I must do a transformation of the graph representation: see the
    function `splitTerminalAndNonTerminal` for the details here.

    So, I make the appropriate change(s) at the DFA level.  When converting
    back to the graph representation for display, I may have new or, at least,
    different nodes: minimisation can make changes, splitting terminal and
    non-terminal can make changes, subset construction can make changes, and so
    on.  So I cannot "track" nodes through this, unless I want to spend a LOT
    of time and expense.  Instead, I identify nodes which can be combined by a
    Graph representation (but not in a DFA representation) and I combine them
    here.

    So.  Let us say that we have nodes `p` and `q`.  If ALL
    the outgoing transitions of `p` are equal to ALL the outgoing transitions
    of `q`, in terms of the targeted node and the transition itself, but
    EXCLUDING fully recursive links and links to each other, then I can
    combine `p` and `q`.

    At least… I think so.

    So, let's try and see.

    Algorithm:

    1. Identify all the terminal nodes.  Place them into set T.
    2. Partition the terminal nodes into those which have no outgoing edges (T1)
       and those which do (T2).  For this check, recursive edges count as outgoing.
    3. Start with T1. If there are any nodes anywhere else in the graph that have
       no outgoing edges, INCLUDING recursive outgoing edges, then the incoming
       connections can all be merged into one node. Do so.
    4. Now examine T2.  For each node T2, we might have the following cases:
       1. The node is extended by another node.
       2. The node is the extension of another node.
       3. The node can merge with another node.
       4. The node is neither extended by, nor an extension of, any other node.

       Cases 1-3, when identified, must be merged.  This means that all their
       incoming connections and outcoming connections must be combined.
       *** 🔴NOTE🔴 *** that post-merging, another of these cases may yet apply!
       
       So, how do I identify each case?

       1. If a terminal node N is extended by another node, then it will have
          these characteristics:
          (i) The outgoing connections of N will be precisely the same, in terms
              of transitions and targets, as the outgoing connections of M.
              (this also takes into account the recursive connection expected to
               be found on M, and any found on N).

       2. If a terminal node M is an extension of another node, then it will have
          these characteristics:
          (i) There will be only one incoming connection, leading to a node N. The
              outgoing connections of N will be precisely the same, in terms of
              transitions and targets, as the outgoing connections of M.
              (this also takes into account the recursive connection expected to
               be found on M).

       3. If the node N can merge with another node, then it will have these
          characteristics:
          (i) Any outgoing transition will lead to some node M.  M, in turn, will
              have exactly the same incoming transition from a different node O.
              Nodes O and N will have precisely the same outgoing connections in
              terms of transitions and targets; this also takes recursive edges
              into account.

       4. Any node which does not fall into one of the above three cases is
          considered to be in Case 4.  When all terminal nodes are part of
          Case 4, the algorithm terminates.
    -}
  let
    ending_nodes : Set NodeId
    ending_nodes =
      Graph.fold
        (\ctx nodeset ->
          if IntDict.isEmpty ctx.outgoing then
            Set.insert ctx.node.id nodeset
          else
            IntDict.foldl
              (\_ conn ((czech, state) as acc_) ->
                if czech then
                  acc_
                else if AutoSet.foldl (\t acc -> acc || .isFinal t) False conn then
                  (True, Set.insert ctx.node.id state)
                else
                  acc_
              )
              (False, nodeset)
              ctx.incoming
            |> Tuple.second
        )
        Set.empty
        g_.graph
      -- |> Debug.log "[minimiseNodes] Terminal nodes (i.e. starting points)"
    fanOutEquals : NodeContext Entity Connection -> NodeContext Entity Connection -> Bool
    fanOutEquals a b =
      let
        redirected o id =
          IntDict.toList o
          |> List.map (\(k, v) -> (k, connectionToString v))
          -- |> Debug.log ("[minimiseNodes→fanOutEquals] outgoing of #" ++ String.fromInt id)
      in
      redirected a.outgoing a.node.id == redirected b.outgoing b.node.id
      -- |> Debug.log ("[minimiseNodes→fanOutEquals] Are #" ++ String.fromInt a.node.id ++ " and #" ++ String.fromInt b.node.id ++ " equal?")
    classify : NodeContext Entity Connection -> AutomatonGraph -> Maybe (AutomatonGraph)
    classify terminal g =
      -- classify the terminal node into one of the four classes
      if IntDict.isEmpty terminal.outgoing then
        -- case T1.  We will deal with this right at the end, during
        -- finalisation after ALL user changes have been made for this
        -- round of changes.
        -- Debug.log ("[minimiseNodes] 🕳️ Terminal #" ++ String.fromInt terminal.node.id ++ " has no fan-out. I won't finalise it now.") () |> \_ ->
        let
          emptyOutgoing =
            Graph.fold
              (\nodeContext state ->
                if IntDict.isEmpty nodeContext.outgoing then
                  nodeContext.node.id :: state
                else
                  state
              )
              []
              g.graph
            -- |> Debug.log "[minimiseNodes] After merging T1 nodes"
        in
          case emptyOutgoing of
            [] ->
              -- Debug.log "[finaliseEndNodes] There are no nodes to finalise." |> \_ ->
              Nothing
            [_] ->
              -- Debug.log "[finaliseEndNodes] There is only one terminal node; therefore, nothing to finalise." |> \_ ->
              Nothing
            term::rest ->
              List.foldl
                (minimisation_merge term)
                g
                rest
              -- |> debugAutomatonGraph "[finaliseEndNodes] Post-finalisation"
              |> Just
      else
        let
          getFanExcludingMyself =
            IntDict.keys
            >> List.filterMap (\id ->
              if id == terminal.node.id then
                Nothing
              else
                Graph.get id g.graph
            )
          targets =
            getFanExcludingMyself terminal.outgoing
          sources =
            getFanExcludingMyself terminal.incoming
        in
          -- Debug.log ("[minimiseNodes] Terminal #" ++ String.fromInt terminal.node.id ++ " has a fanout.  Checking targets to see if it is extended by another.") () |> \_ ->
          case List.find (fanOutEquals terminal) targets of
            Just equivalent ->
              -- Case T2, sub-case 1
              -- println ("[minimiseNodes] 🕳️ Node #" ++ String.fromInt terminal.node.id ++ " is extended by #" ++ String.fromInt equivalent.node.id)
              Just (minimisation_merge terminal.node.id equivalent.node.id g)
            Nothing ->
              -- Debug.log ("[minimiseNodes] No suitable targets found; #" ++ String.fromInt terminal.node.id ++ " is not extended by any node.  Checking sources to see if it extends another.") () |> \_ ->
              case List.find (fanOutEquals terminal) sources of
                Just equivalent ->
                  -- Case T2, sub-case 2
                  -- println ("[minimiseNodes] 🕳️ Node #" ++ String.fromInt terminal.node.id ++ " is an extension of #" ++ String.fromInt equivalent.node.id)
                  Just (minimisation_merge terminal.node.id equivalent.node.id g)
                Nothing ->
                  -- Debug.log ("[minimiseNodes] #" ++ String.fromInt terminal.node.id ++ " neither extends nor is extended.") () |> \_ ->
                  case targets of
                    m::_ ->
                      -- Debug.log "[minimiseNodes] Checking for common sources of target-node" m.node.id |> \_ ->
                      IntDict.get terminal.node.id m.incoming
                      |> Maybe.andThen
                        (\chosenConnection ->
                          -- debugLog_ "[minimiseNodes] connection to follow back is" connectionToString chosenConnection |> \_ ->
                          IntDict.toList m.incoming
                          |> List.filterMap
                            (\(s, conn) ->
                              if s /= terminal.node.id && conn == chosenConnection then
                                Graph.get s g.graph
                                -- |> Maybe.map (debugLog_ ("[minimiseNodes] candidate node (excluding #" ++ String.fromInt terminal.node.id ++ ")") (.node >> .id))
                              else
                                Nothing
                            )
                          |> List.find (fanOutEquals terminal)
                          -- |> debugLog_ "[minimiseNodes] selected mergeable candidate"
                            -- (Maybe.map printNodeContext >> Maybe.withDefault "NONE - there is no fan-in; therefore, no merge candidate; therefore, this merge will not take place).")
                          |> Maybe.map
                            (\equivalent ->
                                -- println ("[minimiseNodes] Node #" ++ String.fromInt terminal.node.id ++ " can be merged with node #" ++ String.fromInt equivalent.node.id)
                                minimisation_merge terminal.node.id equivalent.node.id g
                            )
                        )
                    [] ->
                      -- Debug.log "[minimiseNodes] No suitable targets found." () |> \_ ->
                      Nothing
    classify_all terminals g =
      case terminals of
        [] -> g
        t::ts ->
          case Graph.get t g.graph |> Maybe.andThen (\terminal -> classify terminal g) of
            Nothing ->
              classify_all ts g
            Just newG ->
              classify_all terminals newG
  in
    classify_all
      (Set.toList ending_nodes)
      ( g_
        -- |> debugAutomatonGraph "[minimiseNodes] Initial graph"
      )
    -- |> debugAutomatonGraph "[minimiseNodes] Final graph"

toUnminimisedAutomatonGraph : DFARecord a -> AutomatonGraph
toUnminimisedAutomatonGraph dfa =
  let
    stateList = IntDict.toList dfa.states |> List.reverse -- |> Debug.log "[toUnminimisedAutomatonGraph] State-list"
    graph =
      Graph.fromNodesAndEdges
        (stateList |> List.map (\(id, label) -> Graph.Node id label))
        (IntDict.toList (dfa {- |> debugDFA_ "[toUnminimisedAutomatonGraph] DFA as received" -}).transition_function
        |> List.foldl
          (\(from, dict) state ->
            AutoDict.toList dict
            |> List.foldl
              (\(transition, to) state_ ->
                let
                  t =
                    { isFinal = Set.member to dfa.finals
                    , via = transition
                    }
                in
                case Dict.get (from, to) state_ of
                  Nothing ->
                    Dict.insert (from, to) (AutoSet.singleton transitionToString t) state_
                  Just conn ->
                    Dict.insert (from, to) (AutoSet.insert t conn) state_
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
        { graph = Graph.empty
        , description = Nothing
        , root = 0
        }
        -- |> debugAutomatonGraph "[toUnminimisedAutomatonGraph] Graph, since DFA was empty"
      _ ->
        { graph = graph
        , description = Nothing
        , root = dfa.start -- |> Debug.log "[toUnminimisedAutomatonGraph] root"
        }

toAutomatonGraph : DFARecord a -> AutomatonGraph
toAutomatonGraph dfa =
  toUnminimisedAutomatonGraph dfa
  -- |> debugAutomatonGraph "[toAutomatonGraph] Graph, as converted from DFA"
  |> minimiseNodesByCombiningTransitions
  -- |> debugAutomatonGraph "[toAutomatonGraph] Graph, minimised, final output"

renumberAutomatonGraphFrom : Int -> AutomatonGraph -> (IntDict.IntDict NodeId, AutomatonGraph)
renumberAutomatonGraphFrom start g =
  let
    fanMapper =
      IntDict.toList
      >> List.map
        (\(k, conn) ->
          ( k
          , AutoSet.toList conn
            |> List.map
              (\t ->
                ( acceptConditionToString t.via
                , if .isFinal t then 1 else 0
                )
              )
          )
        )
    nodeMap : IntDict.IntDict NodeId
    nodeMap =
      Graph.nodeIds g.graph
      |> List.filterMap (\id -> Graph.get id g.graph)
      |> List.sortBy
        (\ctx ->
          -- ignore the ids and just map by incoming & outgoing transitions.
          ( fanMapper ctx.incoming
          , fanMapper ctx.outgoing
          )
        )
      |> List.indexedMap (\i node -> (node.node.id, i + start))
      |> IntDict.fromList
    get n =
      case IntDict.get n nodeMap of
        Nothing -> Debug.todo ("87TTGUEOU for" ++ String.fromInt n ++ " I SHOULD NEVER BE HERE!")
        Just i -> i
  in
    ( nodeMap
    , { graph =
          Graph.mapContexts
            (\ctx ->
              { node = { id = get ctx.node.id, label = ctx.node.label }
              , incoming =
                  IntDict.foldl
                    (\k -> IntDict.insert (get k))
                    IntDict.empty
                    ctx.incoming
              , outgoing =
                  IntDict.foldl
                    (\k -> IntDict.insert (get k))
                    IntDict.empty
                    ctx.outgoing
              }
            )
            g.graph
      , description = g.description
      , root = get g.root
      }
      -- |> debugAutomatonGraph "[renumberAutomatonGraphFrom] result"
    )

renumberAutomatonGraph : AutomatonGraph -> AutomatonGraph
renumberAutomatonGraph = renumberAutomatonGraphFrom 1 >> Tuple.second

{- The 'stretch` operation takes an automatongraph that has a recursive terminal 
-}
stretch : AutomatonGraph -> AutomatonGraph
stretch g =
  Maybe.map2
    (\rootctx (_, maxId) ->
      let
        isTerminalConn conn =
          AutoSet.filter .isFinal conn
          |> AutoSet.isEmpty
          |> not
        (recursiveOut, nonRecursiveOut) =
          IntDict.partition (\k _ -> k == g.root) rootctx.outgoing
        recursiveTerminal =
          IntDict.partition (\_ -> isTerminalConn)
        (outgoingTerminal, outgoingNonTerminal) = recursiveTerminal recursiveOut
        to_stretch =
          outgoingTerminal
          |> IntDict.values
          |> List.head
        unstretched =
          outgoingNonTerminal
          |> IntDict.values
          |> List.head
      in
      -- if there are any RECURSIVE, TERMINAL links that go into the root, then I must
      -- 'extend' backwards (with just that recursive link).
        case (to_stretch, unstretched) of
          ( Nothing, Nothing ) -> -- there are NO recursive transitions at all on the root.
            g -- nothing to do.
          ( Nothing, Just _ ) -> -- there are no terminal retursive transitions on the root
            g -- nothing to do
          ( Just transitions, Nothing ) ->
            -- we need to extend
            { g
              | root = maxId + 1
              , graph =
                  Graph.insert
                    { node = { id = maxId + 1, label = rootctx.node.label }
                    , incoming = IntDict.empty
                    , outgoing =
                        IntDict.singleton g.root transitions
                        |> IntDict.union nonRecursiveOut
                    }
                    g.graph
            }
          ( Just terminals, Just nonTerminals ) ->
            { g
              | root = maxId + 1
              , graph =
                  Graph.insert
                    { node = { id = maxId + 1, label = rootctx.node.label }
                    , incoming = IntDict.empty
                    , outgoing =
                      IntDict.singleton g.root (AutoSet.union terminals nonTerminals)
                      |> IntDict.union nonRecursiveOut
                    }
                    g.graph
            }
    )
    (Graph.get g.root g.graph)
    (Graph.nodeIdRange g.graph)
  |> Maybe.withDefault g

splitTerminalAndNonTerminal : AutomatonGraph -> AutomatonGraph
splitTerminalAndNonTerminal g =
  {-
    In this function, I try to split terminal and non-terminal transitions.
    When a node's incoming Connections contain only terminal nodes, or only
    non-terminal nodes, then there is no problem when we convert to a DFA
    because the DFA's "finals" field will correctly record the status of all
    those transitions.  However, whenever the incoming field contains a mix
    of terminal and non-terminal transitions, then we must split the node
    into two nodes, one for each type of transition.

    (reminder: a terminal transition is identified by the second element of
     the transition tuple.  For terminal transitions, this will be 1; for
     non-terminal transitions, this will be 0).

    If the original node is `q`, let all terminal transitions terminate at
    `q`.  Then create a new node `r` which shares all of the outgoing edges
    of `q`, but accepts only non-terminal transitions.  The new node `r` can
    be given a node-id based on the .maxId field of the AutomatonGraph, which
    can be incremented to result in an unused node-id.
  -}
  let
        -- Helper to classify a transition as terminal or non-terminal
    isTerminal : Transition -> Bool
    isTerminal = .isFinal

    isNonTerminal : Transition -> Bool
    isNonTerminal = not << (.isFinal)

    -- Find the next unused node id
    nextId : Int
    nextId =
      maxId g + 1

    -- For each node, determine if it needs to be split
    nodesToSplit =
      Graph.nodeIds g.graph
      |> List.filterMap
        (\id ->
          Graph.get id g.graph
          -- |> Debug.log ("[splitTerminalAndNonTerminal] Node for #" ++ String.fromInt id)
        )
      |> List.filter (\node ->
        let
          incomingTransitions =
            node.incoming
            |> IntDict.values
            |> List.concatMap AutoSet.toList
          -- hmm.  For recursion, outgoing is authoritative, and incoming does not show the incoming recursive edge.
          outgoingRecursive =
            IntDict.get node.node.id node.outgoing
            |> Maybe.map AutoSet.toList
            |> Maybe.withDefault []
          allIncoming =
            outgoingRecursive ++ incomingTransitions
            -- |> Debug.log ("[splitTerminalAndNonTerminal] Incoming transitions to " ++ String.fromInt node.node.id)
          hasTerminal = List.any isTerminal allIncoming
          hasNonTerminal = List.any isNonTerminal allIncoming
        in
          hasTerminal && hasNonTerminal
      )
      -- |> debugLog_ "[splitTerminalAndNonTerminal] nodes to split" (List.map (.node >> .id))

    -- Build a mapping from node id to new split node id (for non-terminal transitions)
    splitMap : Dict NodeId NodeId
    splitMap =
      List.indexedMap (\i node -> (node.node.id, nextId + i)) nodesToSplit
      |> Dict.fromList
      -- |> Debug.log "[splitTerminalAndNonTerminal] Nodeid → split nodeid mapping (for non-terminal transitions)"

    -- Helper to update incoming edges for all nodes
    updateIncoming : List (NodeContext Entity Connection) -> List (NodeContext Entity Connection)
    updateIncoming nodes =
        nodes
        |> List.concatMap
          (\node ->
            case Dict.get node.node.id splitMap of
              Nothing ->
                -- Not split, keep as is
                [ node ]
              Just newId ->
                -- Split: create two nodes
                let
                  -- Partition incoming transitions
                  (terminalIn, nonTerminalIn) =
                    node.incoming
                    |> IntDict.toList
                    |> List.foldl
                        (\(src, conns) (tAcc, ntAcc) ->
                          let
                            (tSet, ntSet) = AutoSet.partition isTerminal conns
                          in
                            ( if AutoSet.isEmpty tSet then tAcc else IntDict.insert src tSet tAcc
                            , if AutoSet.isEmpty ntSet then ntAcc else IntDict.insert src ntSet ntAcc
                            )
                        )
                        (IntDict.empty, IntDict.empty)
                    -- |> debugLog_ ("[splitTerminalAndNonTerminal] Partitioned (terminal, nonterminal) incoming transitions for #" ++ String.fromInt node.node.id) (\(a,b) -> (IntDict.map (\_ v -> AutoSet.toList v) a |> IntDict.toList, IntDict.map (\_ v -> AutoSet.toList v) b |> IntDict.toList))

                  -- The outgoing edges are the same for both nodes
                  outgoing = node.outgoing
                  label = node.node.label
                  -- Create the original node with only terminal incoming
                  nodeTerm =
                    { node
                      | incoming = terminalIn
                      , outgoing = outgoing
                    }
                  -- Create the new node with only non-terminal incoming
                  nodeNonTerm =
                    { node
                      | node =
                          { id = newId
                          , label = label
                          }
                      , incoming = nonTerminalIn
                      , outgoing = outgoing
                    }
                in
                  [ nodeTerm, nodeNonTerm ]
          )

    -- Helper to update outgoing edges for all nodes
    updateOutgoing : List (NodeContext Entity Connection) -> List (NodeContext Entity Connection)
    updateOutgoing nodes =
      nodes
      |> List.map (\node ->
          let
            newOutgoing =
              IntDict.foldl
                (\dest conns acc ->
                  case Dict.get dest splitMap of
                    Nothing ->
                      -- Destination not split, keep as is
                      IntDict.insert dest conns acc
                    Just newId ->
                      -- Partition outgoing transitions
                      let
                        (tSet, ntSet) =
                          AutoSet.partition isTerminal conns
                        acc1 =
                          if not (AutoSet.isEmpty tSet) then
                            IntDict.insert dest tSet acc
                          else
                            acc
                        acc2 =
                          if not (AutoSet.isEmpty ntSet) then
                            IntDict.insert newId ntSet acc1
                          else
                            acc1
                      in
                        acc2
                )
                IntDict.empty
                node.outgoing
          in
            { node | outgoing = newOutgoing }
      )

    -- Compose the new node list
    newNodeContexts =
      g.graph
      |> Graph.nodeIds
      |> List.filterMap (\id -> Graph.get id g.graph)
      |> updateIncoming
      |> updateOutgoing

    newNodes =
      List.map (\node -> node.node) newNodeContexts

    newEdges =
      List.concatMap
        (\source ->
          source.outgoing
          |> IntDict.toList
          |> List.map (\(dest, conn) -> Edge source.node.id dest conn )
        )
        newNodeContexts

    -- Build the new graph
    newGraph =
      Graph.fromNodesAndEdges
        newNodes
        newEdges

  in
    { g | graph = newGraph }
    -- Specially, the root is always considered to be a non-terminal during conversion.
    -- Otherwise, it may end up in the final set, which means that the DFA accepts
    -- with zero transitions being taken, whereas no AutomatonGraph can possibly
    -- exhibit such behaviour: finality is encoded in transitions, so AT LEAST ONE
    -- transition MUST be taken for acceptance to occur.
    |> stretch
    -- |> debugAutomatonGraph "[splitTerminalAndNonTerminal] after split"


ellipsis : Int -> String -> String
ellipsis n s =
  if String.length s > n then
    String.slice 0 (n - 1) s ++ "…"
  else
    s

stateIdentifierToString : StateIdentifier -> String
stateIdentifierToString (f, list) =
  (if f == 1 then "*" else "")
  ++ "["
  ++ (List.map String.fromInt list |> String.join ",")
  ++ "]"

tableToString : Table -> String
tableToString table =
  Dict.toList table
  |> List.map
    (\(sourceIdentifier, columnDict) ->
      AutoDict.foldl
        (\ch (destIdentifier, v) acc ->
          acc
          ++ (printableAcceptCondition ch) ++ "→"
          ++ String.padRight 18 ' ' (stateIdentifierToString destIdentifier ++ ":" ++ ellipsis 11 (Debug.toString v))
        )
        (String.padRight 10 ' ' (stateIdentifierToString sourceIdentifier))
        columnDict
    )
  |> String.join "\n"

debugTable_ : String -> Table -> Table
debugTable_ s t =
  Debug.log (s ++ ":\n" ++ tableToString t) () |> \_ -> t

type alias Table = Dict StateIdentifier Column
type alias Column = AutoDict.Dict String AcceptVia (StateIdentifier, Entity)
type alias StateIdentifier = (Int, List NodeId)
nfaToDFA : AutomatonGraph -> AutomatonGraph
nfaToDFA g = -- use subset construction to convert an NFA to a DFA.
  {-
  Given a DFA

    .←-k--.
    0 -a→ 1 -b→ 2
          ↺
          k

  … we should come up with a table like:

          a   k    b
      .-------------
  0   |  1   -    -
  1   |  -   0,1  2   <- state 1 goes via transition `k` to states 0 AND 1
  2   |  -   -    -
  0,1 |  1   0,1  2

  …and then we can give the new "0,1" state a real name based on the actual
  .maxId, so maybe something like "3".  But for now, it is useful to keep it
  as an ordered list because it makes it easier to identify duplicate states;
  renaming can happen much later.

  So, how do we create this transition table?

  First, we get the transitions from all the nodes and put them into
  `table`.  The `table` is structured as

  source-set → transition → (dest-set, node-label)
  [NOTE: this is a bit out-of-date…]

  (
    although we use the suffix "-set", they're actually lists; and they're all
    sorted
  )

  We can then iterate through the source-set→transition intersection (which
  gives us the unique cell that we're looking for).  When the dest-set has
  one element only, then there's no problem with the transition; it's in
  DFA form already.  However, when it has more than one element, then we need
  to add a new "row" to the table (if such a row does not already exist) by
  creating an appropriate source-set that takes its name from the dest-set.
  We create the cell values by iterating through the respective table values
  of all rows referenced by the source-set.

  The process repeats until the table is complete: i.e., until there are no
  combinations which are not in the table.

  ----------

  After the process is complete, we can proceed as follows:
  1. Merge all states which have identical cell values, ensuring that we
      update all references to those states.
  2. Remove all states which cannot be reached via transitions from .root.
  3. Rename the new rows (and all references to them) using the .maxId field.

  Lastly, use the transition table to amend the graph using the Graph API,
  thus turning an NFA into a DFA.
  -}
  let
    -- Helper to normalize a list of NodeIds (sort and remove duplicates)
    normalizeSet : List NodeId -> List NodeId
    normalizeSet = List.sort >> List.unique

    populateColumnData : IntDict Connection -> Column -> Column
    populateColumnData outgoing columnDict =
      IntDict.foldl
        (\destId conn columnDict_ ->
          AutoSet.foldl
            (\t d ->
              case AutoDict.get t.via d of
                Nothing ->
                  -- Debug.log ("[nfaToDFA] Inserting first " ++ acceptConditionToString t.via ++ "-transition (" ++ (if not (.isFinal t) then "non-" else "") ++ "Final), to #" ++ String.fromInt destId) () |> \_ ->
                  Graph.get destId g.graph
                  |> Maybe.map
                    (\{node} ->
                      AutoDict.insert t.via ((if (.isFinal t) then 1 else 0, [destId]), node.label) d
                    )
                  |> Maybe.withDefaultLazy (\() -> Debug.todo ("BGFOEK " ++ String.fromInt destId))
                Just ((f2, list), v) ->
                  -- Debug.log ("[nfaToDFA] Inserting another " ++ acceptConditionToString t.via ++ "-transition (" ++ (if not (.isFinal t) then "non-" else "") ++ "Final), to #" ++ String.fromInt destId) () |> \_ ->
                  -- if any of the transitions is final, then the created state will be final
                  AutoDict.insert t.via ((max (if (.isFinal t) then 1 else 0) f2, normalizeSet (destId::list)), v) d
            )
            columnDict_
            conn
        )
        columnDict
        outgoing

    -- this is plausible here only because I have split terminal & non-terminal
    -- BEFORE this function is called.  Otherwise, it is probably nonsense…
    terminalityOf : Graph.NodeContext Entity Connection -> Int
    terminalityOf node =
      IntDict.values node.incoming
      -- has at least one incoming terminal transition.
      |> List.any (\conn -> AutoSet.filter (.isFinal) conn |> (not << AutoSet.isEmpty))
      |> \b -> if b then 1 else 0

    -- Get all transitions from the original NFA and organize them
    initialTable : Table
    initialTable =
      Graph.fold
        (\node rowDict ->
          populateColumnData node.outgoing (AutoDict.empty acceptConditionToString)
          |> \d ->
            Dict.insert (terminalityOf node, [node.node.id]) d rowDict
        )
        Dict.empty
        g.graph
      -- |> debugTable_ "[nfaToDFA] Initial table"

    -- Build the complete table by adding rows for multi-element dest-sets
    buildCompleteTable : Table -> Table
    buildCompleteTable table =
      let
        -- Find all dest-sets that have more than one element and aren't already in the table
        newSourceSets =
          table
            |> Dict.values
            |> List.concatMap AutoDict.values
            |> List.map (\(identifier, _) -> identifier)
            |> List.filter (\(_, destSet) -> List.length destSet > 1)
            -- |> List.map normalizeSet
            |> List.unique
            |> List.filter (\identifier -> not (Dict.member identifier table))

        -- Create new rows for these source-sets
        newTable =
          List.foldl
            (\(finality, sourceSet) rowDict ->
              sourceSet
              |> List.filterMap (\id -> Graph.get id g.graph)
              |> List.foldl
                  (\{outgoing} -> populateColumnData outgoing)
                  (AutoDict.empty acceptConditionToString)
              |> \d -> Dict.insert (finality, sourceSet) d rowDict
            )
            table
            newSourceSets
      in
        case newSourceSets of
          [] -> table
          _ -> buildCompleteTable newTable

    completeTable : Table
    completeTable =
      buildCompleteTable initialTable
      -- |> debugTable_ "[nfaToDFA] Complete table"

    -- Step 1: Merge states with identical cell values
    rename : StateIdentifier -> Set StateIdentifier -> Table -> Table
    rename new_name old_names table =
      let
        with_renamed_columns =
          Dict.map
            (\_ columnDict ->
              columnDict
              |> AutoDict.map (\_ (identifier, v) ->
                if Set.member identifier old_names then
                  (new_name, v)
                else
                  (identifier, v)
              )
            )
            table
      in
        case Dict.get new_name with_renamed_columns of
          Just _ ->
            -- if a new_name row already exists, then remove all other rows;
            -- they will clutter up the namespace
            Set.foldl Dict.remove with_renamed_columns old_names
          Nothing ->
            -- otherwise, rename the first of the old_names to new_name
            -- and then remove the rest.
            case Set.toList old_names |> List.findMap (\name -> Dict.get name with_renamed_columns) of
              Nothing ->
                -- … so, I didn't actually do ANY renaming.  Okay then!
                table
              Just v ->
                Set.foldl
                  Dict.remove
                  (Dict.insert new_name v with_renamed_columns)
                  old_names

    mergeIdenticalStates : List (StateIdentifier, b) -> Table -> Table
    mergeIdenticalStates to_merge table =
        case to_merge of
          [] -> table -- nothing to merge! Also, impossible :-).
          [_] -> table -- nothing to merge!
          head::tail ->
            let
              sourceSets_to_merge =
                List.map Tuple.first tail
                |> Set.fromList
                -- |> debugLog_ "[nfaToDFA] source-sets to merge" (Set.map stateIdentifierToString >> Debug.toString)
              without_merged =
                Set.foldl Dict.remove table sourceSets_to_merge
                -- |> debugTable_ "[nfaToDFA] After removals"
            in
              rename (Tuple.first head) sourceSets_to_merge without_merged

    mergedTable : Table
    mergedTable =
      let
        -- Group source-sets by their transition dictionaries
        groupedByTransitions : Table
        groupedByTransitions =
          completeTable
          |> Dict.toList
          |> List.gatherEqualsBy
              (\((finality, _), transitionsDict) ->
                -- considered equivalent, and thus mergeable, on the basis of:
                -- 1. Finality status
                -- 2. Character transition
                -- 3. Destination state-set
                ( finality
                , transitionsDict
                  |> AutoDict.toList
                  |> List.map (\(ch, (st, _)) -> (ch, st))
                )
              )
          |> List.map (\(x, xs) -> x::xs)
          -- |> debugLog_
          --     "equivalent, and therefore mergeable"
          --     (List.map
          --       (List.map
          --         (\(a, b) ->
          --           ( stateIdentifierToString a
          --           , Dict.toList b
          --             |> List.map Tuple.first
          --           )
          --         )
          --       )
          --      >> Debug.toString
          --     )
          -- Never mind the filter; I'll do this in `mergeIdenticalStates`
          -- |> List.filter
          --   (\list ->
          --     Debug.log "Checking " list |> \_ ->
          --     List.length list > 1
          --     |> Debug.log "Result"
          --   )
          |> List.foldl mergeIdenticalStates completeTable
      in
        groupedByTransitions
        -- |> debugTable_ "[nfaToDFA] Identical cell values have been merged"

    -- Step 2: Remove unreachable states (keep only those reachable from root)
    removeUnreachableStates : Table -> Table
    removeUnreachableStates table = -- not checked…
      let
        rootIdentifier =
          Graph.get g.root g.graph
          |> Maybe.map (\node -> (terminalityOf node, [node.node.id]))
          |> Maybe.withDefaultLazy (\() -> (0, [g.root]) |> Debug.log "Y>YWYAT")
        
        findReachable : List StateIdentifier -> Set StateIdentifier -> Set StateIdentifier
        findReachable worklist visited =
          case worklist of
            [] -> visited
            currentSet :: rest ->
              if Set.member currentSet visited then
                findReachable rest visited
              else
                let
                  newVisited = Set.insert currentSet visited
                  destinations : List StateIdentifier
                  destinations =
                    Dict.get currentSet table
                    |> Maybe.withDefault (AutoDict.empty acceptConditionToString)
                    |> AutoDict.values
                    |> List.map Tuple.first
                    |> List.filter (\destIdentifier -> not (Set.member destIdentifier newVisited))
                in
                findReachable (rest ++ destinations) newVisited

        reachableStates = findReachable [rootIdentifier] Set.empty
      in
        table
        |> Dict.filter (\sourceSet _ -> Set.member sourceSet reachableStates)

    reachableTable : Table
    reachableTable =
      removeUnreachableStates mergedTable
      -- |> debugTable_ "[nfaToDFA] Unreachable cell values have been removed"

    -- Step 3: Rename rows using maxId
    finalTable =
      let
        sourceSets = Dict.keys reachableTable |> List.filter (\(_, list) -> List.length list > 1)
        baseId = maxId g + 1
        
        renameMapping = 
          sourceSets
          |> List.indexedMap (\i (f, sourceList) -> ((f, sourceList), (f, [baseId + i])))

        renamedTable =
          List.foldl
            (\(old_name, new_name) table ->
              rename new_name (Set.singleton old_name) table
            )
            reachableTable
            renameMapping
      in
        renamedTable
        -- |> debugTable_ "[nfaToDFA] Rows have been renamed"

    -- Build the new DFA graph using the Graph API
    -- Create the new graph
    newGraphNodes =
      List.filterMap
        (\(_, idList) ->
          case idList of
            [] -> Nothing
            id::_ ->
              case Graph.get id g.graph of
                Just node -> -- in the original graph, just grab it from there
                  Just (id, node.node.label)
                Nothing -> -- was not in the original graph
                  Dict.values finalTable
                  |> List.map -- this is to maintain exact parity, at the cost of performance, during the swich to AutoDict + actual typing
                    (AutoDict.toList >> List.map (Tuple.mapFirst acceptConditionToString) >> Dict.fromList)
                  |> List.findMap
                    ( Dict.find (\_ ((_, v), _) -> v == idList)
                      >> Maybe.map (\(_, (_, label)) -> (id, label))
                    )
        )
        (Dict.keys finalTable)
      |> List.map (\(id, v) -> Graph.Node id v)

    newGraphEdges =
      Dict.foldl
        (\(_, sourceSet) columnDict acc ->
          case sourceSet of
            [src] ->
              AutoDict.foldl
                (\acceptCondition ((f, destSet), _) acc_ ->
                  case destSet of
                    [dest] ->
                      Dict.update (src, dest)
                        (\item ->
                          case item of
                            Nothing ->
                              Just <|
                                AutoSet.singleton transitionToString <|
                                  Transition (f == 1) acceptCondition
                            Just conn ->
                              Just <|
                                AutoSet.insert
                                  (Transition (f == 1) acceptCondition)
                                  conn
                        ) acc_
                    _ ->
                      acc_
                )
                acc
                columnDict
            _ ->
              Debug.log "HDPY>DB" |> \_ ->
              acc -- should never reach here!
        )
        Dict.empty
        finalTable
      |> Dict.toList
      |> List.map (\((src, dest), conn) -> Edge src dest conn)

    newGraph =
      Graph.fromNodesAndEdges newGraphNodes newGraphEdges
  in
    { g | graph = newGraph }
    -- |> debugAutomatonGraph "[nfaToDFA] Resulting graph"

fromAutomatonGraphHelper : AutomatonGraph -> DFARecord {}
fromAutomatonGraphHelper g =
  -- called AFTER splitting non-terminal/terminal, and AFTER NFA→DFA conversion.
  { states =
      Graph.nodes g.graph
      |> List.map (\node -> ( node.id, node.label) )
      |> IntDict.fromList
  , start = g.root
  , finals =
      Graph.fold
        (\ctx finals ->
          if isTerminalNode ctx then 
            Set.insert ctx.node.id finals
          else
            finals
        )
        Set.empty
        g.graph
  , transition_function =
      Graph.fold
        (\ctx transitions ->
          if IntDict.isEmpty ctx.outgoing then
            -- otherwise, I will be adding a spurious Dict.empty,
            -- which will affect comparisons in register_or_replace later on.
            transitions
          else
            IntDict.foldl
              (\dest conn dict ->
                AutoSet.foldl (\{via} -> AutoDict.insert via dest) dict conn
              )
              (AutoDict.empty acceptConditionToString)
              ctx.outgoing
            |> \dict -> IntDict.insert ctx.node.id dict transitions
        )
        IntDict.empty
        g.graph
  }

fromAutomatonGraph : AutomatonGraph -> DFARecord {}
fromAutomatonGraph =
    -- debugAutomatonGraph "[fromAutomatonGraph] Graph as received" >>
    splitTerminalAndNonTerminal
    -- >> debugAutomatonGraph "[fromAutomatonGraph] Graph after splitting the joined terminal+non-terminal nodes"
    >> nfaToDFA
    -- >> debugAutomatonGraph "[fromAutomatonGraph] Graph NFA→DFA conversion"
    >> fromAutomatonGraphHelper
    -- >> debugDFA_ "[fromAutomatonGraph] Graph→DFA"

encodeAutoDict : (k -> String) -> (v -> E.Value) -> AutoDict.Dict comparable k v -> E.Value
encodeAutoDict toKey toValue dictionary =
  AutoDict.toList dictionary
  |> List.map (\(k, v) -> ( toKey k, toValue v ))
  |> E.object

{-| Decode an AutoDict.

Need to pass:
- `keyFunction`: the usual `key -> comparable` function used by the AutoDict
- `stringToKey`: a decoder to convert a field name to a `key`, if possible.  Fields
    with names that cannot be converted are considered to be invaled and ignored
    entirely.
- `valueDecoder`: a decoder for the field value
-}
decodeAutoDict : (key -> comparable) -> (String -> Maybe key) -> D.Decoder value -> D.Decoder (AutoDict.Dict comparable key value)
decodeAutoDict keyFunction stringToKey valueDecoder =
  D.map (AutoDict.fromList keyFunction)
    ( D.keyValuePairs valueDecoder -- gives me a List (String, value)
      |> D.map
        (\kvList ->
          List.filterMap
            (\(fieldName, v) ->
              stringToKey fieldName
              |> Maybe.map (\k -> (k, v))
            )
            kvList
        )
    )

encodeTransition : Transition -> E.Value
encodeTransition {via, isFinal} =
  case via of
    ViaCharacter c ->
      E.object
        [ ("c", E.string <| String.fromChar c)
        , ("_", E.bool isFinal)
        ]
    ViaGraphReference uuid ->
      E.object
        [ ("ref", E.string <| Uuid.toString uuid)
        , ("_", E.bool isFinal)
        ]

encodeEdge : Edge Connection -> E.Value
encodeEdge e =
  E.object
    [ ("src", E.int e.from)
    , ("dst", E.int e.to)
    , ("via", E.list encodeTransition <| AutoSet.toList e.label)
    ]

encodeEffect : NodeEffect -> E.Value
encodeEffect effect =
  case effect of
    NoEffect ->
      E.object [ ("none", E.null) ]
    SomeEffectFigureItOutLater ->
      E.object [ ("?", E.null) ]

encodeNode : Graph.Node Entity -> E.Value
encodeNode n =
  E.object
    [ ("x", E.float n.label.x)
    , ("y", E.float n.label.y)
    , ("i", E.int n.id)
    , ("e", encodeEffect n.label.effect)
    ]

encodeAutomatonGraph : AutomatonGraph -> E.Value
encodeAutomatonGraph g =
  E.object
    [ ("edges", E.list encodeEdge <| Graph.edges g.graph)
    , ("nodes", E.list encodeNode <| Graph.nodes g.graph)
    , ("description", Maybe.map E.string g.description |> Maybe.withDefault E.null)
    --, ("uuid", Uuid.encode g.graphIdentifier)
    , ("root", E.int g.root)
    ]

decodeEffect : D.Decoder NodeEffect
decodeEffect =
  D.oneOf
    [ D.field "none" (D.null ()) |> D.map (\_ -> NoEffect)
    , D.field "?" (D.null ()) |> D.map (\_ -> SomeEffectFigureItOutLater)
    ]

decodeNode : D.Decoder (Graph.Node Entity)
decodeNode =
  D.map4
    (\x y i e ->
        { id = i
        , label =
            { id = i
            , x = x
            , y = y
            , vx = 0.0
            , vy = 0.0
            , effect = e
            }
        }
    )
    (D.field "x" <| D.float)
    (D.field "y" <| D.float)
    (D.field "i" <| D.int)
    (D.field "e" <| decodeEffect)

decodeTransition : D.Decoder Transition
decodeTransition =
  D.oneOf
    [ D.map2
        (Transition)
        (D.field "_" D.bool)
        (D.field "c" D.string
          |> D.andThen
            (\s ->
              case String.toList s of
                [] -> D.fail "Char field cannot be empty."
                [c] -> D.map ViaCharacter (D.succeed c)
                _ -> D.fail "Char field cannot be more than one character."
            )
        )
    , D.map2
        (Transition)
        (D.field "_" D.bool)
        (D.field "ref" <| D.map ViaGraphReference Uuid.decoder)
    ]

decodeEdge : D.Decoder (Edge Connection)
decodeEdge =
  D.map3
    (\f t l -> Edge f t (AutoSet.fromList transitionToString l))
    (D.field "src" <| D.int)
    (D.field "dst" <| D.int)
    (D.field "via" <| D.list decodeTransition)

decodeAutomatonGraph : D.Decoder AutomatonGraph
decodeAutomatonGraph =
  D.map4 (\n e -> AutomatonGraph (Graph.fromNodesAndEdges n e))
    (D.field "nodes" <| D.list decodeNode)
    (D.field "edges" <| D.list decodeEdge)
    (D.field "description" <| D.oneOf [ D.map Just D.string, D.null Nothing ])
    (D.field "root" <| D.int)

-----------------
-- DEBUGGING
-----------------

printTransitions : IntDict (AutoDict.Dict String AcceptVia NodeId) -> String
printTransitions transitions =
  (IntDict.foldl
    (\k dict acc ->
      AutoDict.toList dict
      |> List.map
          (\(transition, to) ->
            String.fromInt k ++ "→" ++ String.fromInt to ++ " (" ++ printableAcceptCondition transition ++ ")"
          )
      |> (++) acc
    )
    []
    transitions
  |> String.join ", ")

printDFA : DFARecord a -> String
printDFA dfa =
  "📍" ++ String.fromInt dfa.start ++ " { " ++ 
  ( List.map
      (\(id, _) ->
        if Set.member id dfa.finals then
          "*" ++ String.fromInt id
        else
          String.fromInt id
      )
      (IntDict.toList dfa.states)
    |> String.join ","
  ) ++
  " | " ++ printTransitions dfa.transition_function ++ " }"
  -- "  ▶ States: " ++
  --   ( List.map (Tuple.first >> String.fromInt) (IntDict.toList dfa.states)
  --     |> String.join ", "
  --   )
  -- ++ "\n  ▶ Transitions: " ++ printTransitions dfa.transition_function
  -- ++ "\n  ▶ Finals: " ++ Debug.toString (Set.toList dfa.finals)
  -- ++ "\n  ▶ Start: " ++ String.fromInt dfa.start

printExtDFA : ExtDFA -> String
printExtDFA extDFA =
  printDFA extDFA
  ++ "\n   ▶ orig_w_dfa = "
  ++ printDFA extDFA.w_dfa_orig
  ++ "\n   ▶ Register / Queue|Clones = "
  ++ Debug.toString (Set.toList extDFA.register) ++ " / "
  ++ Debug.toString extDFA.queue_or_clone ++ "📍"
  ++ String.fromInt extDFA.clone_start
  ++ "; unused: " ++ String.fromInt extDFA.unusedId
  -- ++ "\n  ▶ Register: " ++ Debug.toString (Set.toList extDFA.register)
  -- ++ "\n  ▶ Queue/Clones: " ++ Debug.toString extDFA.queue_or_clone
  -- ++ "\n  ▶ clone_start: " ++ String.fromInt extDFA.clone_start
  -- ++ "\n  ▶ unusedId: " ++ String.fromInt extDFA.unusedId

debugDFA_ : String -> DFARecord a -> DFARecord a
debugDFA_ s dfa =
  Debug.log (s ++ ": " ++ printDFA dfa) () |> \_ -> dfa

debugExtDFA_ : String -> ExtDFA -> ExtDFA
debugExtDFA_ s extDFA =
  Debug.log (s ++ ": " ++ printExtDFA extDFA) () |> \_ -> extDFA

debugDFA : DFARecord a -> DFARecord a
debugDFA dfa =
  Debug.log (printDFA dfa) () |> \_ -> dfa

debugExtDFA : ExtDFA -> ExtDFA
debugExtDFA extDFA =
  Debug.log (printExtDFA extDFA) () |> \_ -> extDFA
