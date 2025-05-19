module Automata.MADFA exposing (..)
import Graph exposing (Graph, NodeContext, Node, NodeId)
import IntDict exposing (IntDict)
import Set exposing (Set)
import List.Extra as List
import Maybe.Extra as Maybe
import Dict exposing (Dict)
import Automata.Data exposing (..)
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

type alias MADFARecord =
  { graph : FAGraph
    {- The maximum ID-value in this graph -}
  , maxId : NodeId
  , root : NodeId
  , queue : Set NodeId
  , cloned : Set NodeId
  , register : Dict RegisterValue NodeId -- these ones do not need to be minimised. ( isFinal, list-of-outgoing )
  }

wordToTransitions : String -> List (MTransition, Bool)
wordToTransitions txt =
  String.toList txt
  |> List.unconsLast
  |> Maybe.map (\(last, rest) -> List.map (\ch -> (ch, False)) rest ++ [(last, True)])
  |> Maybe.withDefault []

follow_transition : MTransition -> Maybe NodeId -> MADFARecord -> Maybe NodeId
follow_transition transition source madfa =
  source
  |> Maybe.andThen (\src -> Graph.get src madfa.graph)
  |> Maybe.andThen (\node ->
    case IntDict.toList node.outgoing |> List.filter (\(_, conn) -> Set.member transition conn) of
      [] -> Nothing
      -- NOTE: we ASSUME determinism here.
      (dest, _)::_ -> Just dest
  )

clone : NodeId -> Bool -> Maybe (NodeId, MTransition) -> MADFARecord -> (NodeId, MADFARecord)
clone source_id isFinal redirectionData madfa =
  let
    to_clone = Graph.get source_id madfa.graph
    created_clone =
      to_clone
      |> Maybe.map (\src_node ->
        ( NodeContext
            (Node (madfa.maxId + 1) (src_node.node.label || isFinal)) -- new state that I'm creating.
            IntDict.empty -- if needed, will redirect a node to this.
            src_node.outgoing
        )
      )
      -- |> DAWG.Debugging.debugLog ("[clone] Cloning #" ++ String.fromInt source_id ++ " to #" ++ String.fromInt (madfa.maxId + 1))
      --   (Maybe.map (\n -> { outgoing = IntDict.toList n.outgoing, incoming = IntDict.toList n.incoming }))
    new_redirected_node =
      Maybe.map
        (\(target_node, t) ->
          { target_node
          | outgoing =
            -- I know that there is at least one transition.
            (case IntDict.toList target_node.outgoing of
              [] ->
                IntDict.empty -- this is nonsense
              [(k, v)] ->
                case Set.toList v of
                  [] ->
                    IntDict.empty
                  [_] -> -- this must be the only transition in the only outgoing. Replace it.
                    IntDict.singleton (madfa.maxId + 1) v
                  _ -> -- there is more than one item in the set
                    IntDict.fromList [( madfa.maxId + 1, Set.singleton t ), (k, Set.remove t v)]
              xs -> -- there are outgoing connections to multiple nodes
                ( ( madfa.maxId + 1, Set.singleton t ) :: (List.filterMap
                  (\(k, v) ->
                    let
                      new_v = Set.remove t v
                    in
                      if Set.isEmpty new_v then Nothing else Just (k, new_v)
                  )
                  xs)
                )
                |> IntDict.fromList
            )
            -- |> DAWG.Debugging.debugLog "[clone] After redirect" (IntDict.toList >> List.map (\(a, b) -> (a, connectionToString b)))
          }
        )
        (redirectionData |> Maybe.andThen (\(id, t) -> Graph.get id madfa.graph |> Maybe.map (\n -> (n, t))))
  in
  created_clone
  |> Maybe.map (\the_clone ->
    ( madfa.maxId + 1 -- |> Debug.log ("Cloning #" ++ String.fromInt source_id ++ " with new id")
    , { madfa
      | graph =
          case new_redirected_node of
            Nothing ->
              Graph.insert the_clone madfa.graph
            Just updated_node ->
              Graph.insert the_clone madfa.graph
              |> Graph.insert updated_node
      , maxId = madfa.maxId + 1
      , cloned = Set.insert (madfa.maxId + 1) madfa.cloned
      }
      -- |> madfaLog "[clone] done" "\n"
    )
  )
  |> Maybe.withDefault (source_id, madfa) -- which is total nonsense.

queue : Bool -> (MTransition, NodeId) -> MADFARecord -> (NodeId, MADFARecord)
queue isFinal (transitionFromSource, source) madfa =
  ( madfa.maxId + 1 -- |> Debug.log ("[queue] source = #" ++ String.fromInt source ++ " via transition " ++ String.fromChar transitionFromSource ++ ", new node created is")
  , { madfa
    | graph =
        Graph.insert
          ( NodeContext
              (Node (madfa.maxId + 1) isFinal) -- new state that I'm creating.
              (IntDict.singleton source <| Set.singleton transitionFromSource)
              IntDict.empty
          ) madfa.graph
    , maxId = madfa.maxId + 1
    , queue = Set.insert (madfa.maxId + 1) madfa.queue
    }
  )

--|> \v -> Debug.log ("Creating edge: #" ++ String.fromInt source ++ " -> #" ++ String.fromInt destination ++ " on transition " ++ String.fromChar transition ++ ", outgoing is now " ++ Debug.toString (IntDict.toList v)) () |> \_ -> v

remove_unreachable : NodeId -> Maybe (List MTransition) -> MADFARecord -> MADFARecord
remove_unreachable current transitions madfa =
  case transitions of
    Nothing ->
      madfa
    Just [] ->
      Graph.get current madfa.graph
      |> Maybe.map (\node ->
        if IntDict.isEmpty node.incoming then
          { madfa
          | graph = Graph.remove current madfa.graph
          , register =
              Dict.remove (toRegisterValue current madfa) madfa.register
              -- |> (Debug.log ("Removing(Ⅱ) unreachable node #" ++ String.fromInt current ++ ", remaining in register"))
          }
        else
          madfa
      )
      |> Maybe.withDefault madfa
    Just (h::rest) ->
      let
        ( is_unreachable, q_next ) =
          Graph.get current madfa.graph
          |> Maybe.map (\node ->
            let
              next = IntDict.toList node.outgoing |> List.filter (\(_, conn) -> Set.member h conn) |> List.map Tuple.first |> List.head
            in
            ( IntDict.isEmpty node.incoming, next )
          )
          |> Maybe.withDefault ( False, Nothing )
        updated_madfa =
          if is_unreachable then
            { madfa
            | graph = Graph.remove current madfa.graph
            , register =
                Dict.remove (toRegisterValue current madfa) madfa.register
                -- |> (Debug.log ("Removing unreachable node #" ++ String.fromInt current ++ ", remaining in register"))
            }
          else
--            Debug.log ("Node #" ++ String.fromInt current ++ " is not unreachable") () |> \_ ->
            madfa
      in
        case q_next {- |> Debug.log "Next node to check for unreachability" -} of
          Nothing ->
            updated_madfa
          Just next ->
            if is_unreachable then
              remove_unreachable next (Just rest) updated_madfa
            else
              updated_madfa

toRegisterValue : NodeId -> MADFARecord -> RegisterValue
toRegisterValue q madfa =
  Graph.get q madfa.graph
  |> Maybe.map (\node ->
    ( if node.node.label then 1 else 0
    , IntDict.toList node.outgoing
      |> List.map (\(k, v) -> (k, Set.toList v))
    )
  )
  |> Maybe.withDefault (-1, []) -- nonsense default, should never appear.

merge : NodeId -> NodeId -> MADFARecord -> MADFARecord
merge new original madfa =
  -- all of the incoming transitions of the `new` node need to be
  -- redirected into the `original` node, and then the `new` node
  -- is removed.
  let
    new_incoming =
      Graph.get new madfa.graph
      |> Maybe.map .incoming
      |> Maybe.withDefault IntDict.empty -- nonsense
    new_madfa =
      { madfa
      | graph =
          Graph.update original
            (Maybe.map (\orig_node ->
              { orig_node
              | incoming = IntDict.uniteWith (\_ -> Set.union) orig_node.incoming new_incoming
              }
            ))
            madfa.graph
          |> Graph.remove new
      }
  in
    --with_updated_register
    new_madfa
--    |> madfaLog ("MERGED 🧑🏼‍🤝‍🧑🏼 new #" ++ String.fromInt new ++ " to original #" ++ String.fromInt original) "\n"

equiv_in_register : NodeId -> MADFARecord -> Maybe NodeId
equiv_in_register q madfa =
  -- They are equivalent if:
  -- (1) either both are final, or both are not final; and
  -- (2) both have exactly the same outgoing transitions (i.e. transition & destination state matches)
  let
    q_key = toRegisterValue q madfa -- |> Debug.log ("Generated key for node #" ++ String.fromInt q)
  in
    Dict.get q_key madfa.register -- |> Debug.log ("Equivalent for cloned/queued node #" ++ String.fromInt q)

replace_or_register : NodeId -> MADFARecord -> MADFARecord
replace_or_register current madfa =
  case equiv_in_register current madfa of
    Just existing ->
      merge current existing madfa
    Nothing ->
      { madfa
      | register = Dict.insert (toRegisterValue current madfa) current madfa.register
      }

phase1 : List (MTransition, Bool) -> NodeId -> NodeId -> MADFARecord -> MADFARecord
phase1 w original_q0 new_q0 original_madfa =
  -- w |> Debug.log "[1] w" |> \_ -> 
  -- original_q0 |> Debug.log "[1] original_q0" |> \_ -> 
  -- new_q0 |> Debug.log "[1] new_q0" |> \_ -> 
  -- Graph.nodes original_madfa.graph |> Debug.log "[1] original_madfa nodes" |> \_ -> 
  -- Graph.edges original_madfa.graph |> Debug.log "[1] original_madfa edges" |> \_ -> 
  -- original_madfa.maxId |> Debug.log "[1] original_madfa maxId" |> \_ ->
  -- original_madfa.cloned |> Debug.log "[1] original_madfa cloned" |> \_ ->
  -- original_madfa.queue |> Debug.log "[1] original_madfa queue" |> \_ ->
  -- original_madfa.register |> Debug.log "[1] original_madfa register" |> \_ ->
  -- original_madfa.root |> Debug.log "[1] original_madfa root" |> \_ ->
  List.foldl
    (\(transition, isFinal) (m_branch, q_last, madfa) ->
      let
        destination =
          follow_transition transition m_branch madfa -- attempt to follow a transition from the last-seen state in M
          -- |> Debug.log ("Trying to follow transition " ++ String.fromChar transition ++ " from NodeId " ++ Debug.toString m_branch ++ ", result is")
        (q, madfa_with_q) =
          destination
          |> Maybe.map (\exists_in_M -> -- there is something in M, so we will clone it.
            --Debug.log ("Calling clone") { m_branch = m_branch, q_last = q_last, transition = transition, isFinal = isFinal } |> \_ ->
            clone exists_in_M isFinal (Just (q_last, transition)) madfa
          )
          |> Maybe.withDefaultLazy (\() -> -- there is no compatible path, so we must place this onto the queue
            queue isFinal (transition, q_last) madfa
          )
      in
        ( destination, q, madfa_with_q )
    )
    (Just original_q0, new_q0, original_madfa)
    w
  |> \(_, _, madfa) -> madfa

phase2 : NodeId -> List (MTransition, Bool) -> MADFARecord -> MADFARecord
phase2 original_q0 w original_madfa =
  -- w |> Debug.log "[2] w" |> \_ -> 
  -- original_q0 |> Debug.log "[2] original_q0" |> \_ -> 
  -- Graph.nodes original_madfa.graph |> Debug.log "[2] original_madfa nodes" |> \_ -> 
  -- Graph.edges original_madfa.graph |> Debug.log "[2] original_madfa edges" |> \_ -> 
  -- original_madfa.maxId |> Debug.log "[2] original_madfa maxId" |> \_ ->
  -- original_madfa.cloned |> Debug.log "[2] original_madfa cloned" |> \_ ->
  -- original_madfa.queue |> Debug.log "[2] original_madfa queue" |> \_ ->
  -- original_madfa.register |> Debug.log "[2] original_madfa register" |> \_ ->
  -- original_madfa.root |> Debug.log "[2] original_madfa root" |> \_ ->
  remove_unreachable original_q0 (Just <| List.map Tuple.first w) original_madfa

phase3 : List NodeId -> MADFARecord -> MADFARecord
phase3 clones_and_queued original_madfa =
  -- clones_and_queued |> Debug.log "[3] clones_and_queued" |> \_ ->
  -- Graph.nodes original_madfa.graph |> Debug.log "[3] original_madfa nodes" |> \_ -> 
  -- Graph.edges original_madfa.graph |> Debug.log "[3] original_madfa edges" |> \_ -> 
  -- original_madfa.maxId |> Debug.log "[3] original_madfa maxId" |> \_ ->
  -- original_madfa.cloned |> Debug.log "[3] original_madfa cloned" |> \_ ->
  -- original_madfa.queue |> Debug.log "[3] original_madfa queue" |> \_ ->
  -- original_madfa.register |> Debug.log "[3] original_madfa register" |> \_ ->
  -- original_madfa.root |> Debug.log "[3] original_madfa root" |> \_ ->
  List.foldr
    (\w_state madfa -> replace_or_register w_state madfa)
    original_madfa
    (clones_and_queued {- |> Debug.log "Cloned/Queued states" -})

-- the same as `addstring` algorithm from the paper (Figure 1)
addWordString : List (MTransition, Bool) -> MADFARecord -> MADFARecord
addWordString w existing =
  let
    original_q0 = existing.root -- |> Debug.log "Existing root"
    (new_q0, phase_1_setup) = clone original_q0 False Nothing existing
    phase_1 =
      phase1 w original_q0 new_q0 phase_1_setup -- here, we do the initial insertion of w
      -- |> madfaLog "🤖 AFTER PHASE 1 (clone & queue)" "🎯 BEGINNING PHASE 2 (handle reachability)"
    phase_2 = -- here, we remove unreachable states
      phase2 original_q0 w phase_1
    -- Now, after this I need to replace the original_q0 with the cloned one.  The tough thing is that… I haven't kept track of it!
    -- However, I WILL find it as the smallest number in the list of cloned/queued states.
    clones_and_queued =
      Set.toList phase_2.cloned ++ Set.toList phase_2.queue
    phase_2_end =
      (case List.head clones_and_queued of -- a set will give me things in sorted order; and cloned always precedes queued.
        Nothing ->
          phase_2 -- this can only happen if NOTHING has been cloned OR queued
        Just min ->
          { phase_2 | root = min {- |> Debug.log "Setting new root node to" -} }
      )
--      |> madfaLog "🤖 AFTER PHASE 2 (handle reachability)" "🎯 BEGINNING PHASE 3 (replace-or-register)"
    phase_3 =
      phase3 clones_and_queued phase_2_end
  in
    -- finally, get rid of the clones and the queued; we don't need them.
    { phase_3
    | cloned = Set.empty
    , queue = Set.empty
    }
--    |> madfaLog "🤖 AFTER PHASE 3 (replace-or-register)" "👍🏽 COMPLETE!\n\n***\n\n"

wordToMADFA : String -> MADFARecord -> MADFARecord
wordToMADFA s existing =
  wordToTransitions s
  |> \transitions ->
    case transitions of
      [] -> existing
      _ -> addWordString transitions existing

seedMADFARecord : MADFARecord
seedMADFARecord =
  let
    initialNode =
      NodeContext (Node 0 False) IntDict.empty IntDict.empty
  in
    MADFARecord (Graph.insert initialNode Graph.empty) 0 0 Set.empty Set.empty Dict.empty

fromStrings : List String -> Maybe MADFARecord
fromStrings words =
  case words of
    [] ->
      Nothing
    _ ->
      Just <| List.foldl wordToMADFA seedMADFARecord words




redirect : List NodeId -> Graph a Connection -> Graph a Connection
redirect all_nodes graph =
  -- all of the incoming transitions of the `new` node need to be
  -- redirected into the `original` node, and then the `new` node
  -- is removed.
  case all_nodes of
    [] ->
      graph
    target::deletions -> -- pick one, whichever is first; collapse down into that one.
      let
        collective_nodes =
          List.filterMap (\id -> Graph.get id graph) all_nodes
        collective_incoming =
          List.foldl
            (\node state -> IntDict.uniteWith (\_ -> Set.union) state node.incoming)
            IntDict.empty
            collective_nodes
        collective_outgoing =
          List.foldl
            (\node state -> IntDict.uniteWith (\_ -> Set.union) state node.outgoing)
            IntDict.empty
            collective_nodes
        new_graph =
          Graph.update target
            (Maybe.map (\target_node ->
              { target_node
              | incoming = collective_incoming
              , outgoing = collective_outgoing
              }
            ))
            graph
        without_collapsed =
          List.foldl Graph.remove new_graph deletions
      in
        without_collapsed

check_identical_outgoing : List NodeId -> Maybe (List Connection) -> Graph a Connection -> Bool
check_identical_outgoing states baseline graph =
  case states of
    [] ->
      True
    h::t ->
      let
        thisOutgoing =
          Graph.get h graph |> Maybe.map (.outgoing >> IntDict.values)
          |> Maybe.withDefault [] -- nonsense
      in
        case baseline of
          Nothing ->
            check_identical_outgoing t (Just thisOutgoing) graph
          Just baselineOutgoing ->
            thisOutgoing == baselineOutgoing && check_identical_outgoing t (Just thisOutgoing) graph

collapse : NodeId -> Graph a Connection -> ( IntDict (), Graph a Connection )
collapse node_id graph =
  let
    incoming_states =
      Graph.get node_id graph
      |> Maybe.map (.incoming >> IntDict.keys)
      |> Maybe.andThen (\candidates ->
        -- if there are only 1 or 0, then there's nothing to collapse.
        case candidates of
          [] -> Nothing
          [_] -> Nothing
          _ -> Just candidates
      )
      --|> Debug.log ("[collapse #" ++ String.fromInt node_id ++ "] Incoming states")
    -- if the outgoing transitions of ALL the incoming states are IDENTICAL, then we can collapse.
    outgoing_transitions_identical =
      incoming_states
      |> Maybe.map (\states -> check_identical_outgoing states Nothing graph)
      |> Maybe.withDefault False
      --|> Debug.log ("[collapse #" ++ String.fromInt node_id ++ "] Outgoing transitions identical?")
    new_graph =
      Automata.Debugging.debugGraph ("[collapse #" ++ String.fromInt node_id ++ "] after redirection") <|   
      if outgoing_transitions_identical then
        Maybe.map (\states -> redirect states graph) incoming_states
        |> Maybe.withDefault graph -- nonsense
      else
        graph
    -- and now that we have done one redirection, we must examine the input nodes & redirect if possible.
    ancestors =
      if outgoing_transitions_identical then
        Graph.get node_id new_graph
        |> Maybe.map (.incoming >> IntDict.keys >> List.map (\k -> (k, ())))
        |> Maybe.withDefault []
        --|> Debug.log ("[collapse #" ++ String.fromInt node_id ++ "] Additional nodes to process")
        |> IntDict.fromList
      else
        IntDict.empty
  in
    ( ancestors, new_graph )

collapse_from : IntDict () -> IntDict () -> Graph () Connection -> Graph () Connection
collapse_from to_process processed graph =
  case IntDict.findMax to_process {- |> Debug.log "Collapsing, starting from" -} of
    Just (node_id, _) ->
      let
        ( ancestors, new_graph ) = collapse node_id graph
        -- never re-process a node
        new_to_process =
          IntDict.diff (IntDict.union to_process ancestors) processed
          -- |> Debug.log "Result"
        new_processed =
          IntDict.insert node_id () processed --|> Debug.log "Processed"
      in
      collapse_from new_to_process new_processed new_graph
    Nothing ->
      graph
      --|> DAWG.Debugging.debugGraph "Post-collapse"

toAutomatonGraph : MADFARecord -> AutomatonGraph ()
toAutomatonGraph madfa =
  {- The difference between a FAGraph and a DAWGGraph is that the finality is kept
     in the transition (D) rather than in the state (F).

     Other than that, should be pretty straightforward.
  -}
  let
    finalNodes =
      Graph.nodes madfa.graph
      |> List.filterMap (\node -> if node.label then Just node.id else Nothing)
      -- |> Debug.log "Final nodes"
    finalNodesSet =
      Set.fromList finalNodes
    yesMap t = (t, 1)
    noMap t = (t, 0)
    graph =
      -- basically, the idea here is to "slide" all the finality information
      -- onto the incoming transitions.
      Graph.mapContexts
        (\{node, incoming, outgoing} ->
          let
            mapper = if node.label then yesMap else noMap
          in
          NodeContext
            (Node node.id ())
            (IntDict.map (\_ conn -> Set.map mapper conn) incoming)
            (IntDict.map (\k v -> if Set.member k finalNodesSet then Set.map yesMap v else Set.map noMap v) outgoing)
        )
        madfa.graph
      -- |> Automata.Debugging.debugGraph "Pre-collapse"
    collapsed =
      collapse_from (finalNodes |> List.map (\x -> (x, ())) |> IntDict.fromList) IntDict.empty graph
      -- |> Automata.Debugging.debugGraph "Post-collapse"
    max = madfa.maxId
    root = madfa.root
  in
    AutomatonGraph collapsed max root

fromWords : List String -> Maybe (AutomatonGraph ())
fromWords words =
  fromStrings words
  |> Maybe.map toAutomatonGraph

transitionToString : MTransition -> String
transitionToString =
  String.fromChar

connectionToString : MConnection -> String
connectionToString =
  Set.map transitionToString
  >> Set.toList
  >> String.concat

graphToString : FAGraph -> String
graphToString graph =
  Graph.toString
    (\node -> Just <| if node then "⭐" else "🟢")
    (Just << connectionToString)
    graph

madfaToString : MADFARecord -> String
madfaToString madfa =
  "graph = " ++ graphToString madfa.graph ++ "\n" ++
    "root = " ++ String.fromInt madfa.root ++ "\n" ++
    "cloned = " ++ Debug.toString madfa.cloned ++ "\n" ++
    "queue = " ++ Debug.toString madfa.queue ++ "\n" ++
    "register = " ++ Debug.toString madfa.register

madfaLog : String -> String -> MADFARecord -> MADFARecord
madfaLog s s1 madfa = Debug.log (s ++ ":\n" ++ madfaToString madfa ++ "\n" ++ s1) "" |> \_ -> madfa