module Automata.DFA exposing (..)
import IntDict exposing (IntDict)
import Set exposing (Set)
import List.Extra as List
import Dict exposing (Dict)
import Automata.Data exposing (..)
import Graph exposing (Graph, NodeContext, Node, NodeId, Edge)
import Dict.Extra
import Dict exposing (update)
import List.Extra exposing (remove)
import Maybe.Extra
import Html exposing (tr)
import Tuple.Extra
import Automata.Debugging
import Css exposing (row)
import Automata.Debugging exposing (debugGraph)
import Automata.Debugging exposing (debugAutomatonGraph)
import Svg.Attributes exposing (k)

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias MTransition = Char
type alias MConnection = Set MTransition -- a MConnection is a link between two nodes.
-- type alias Connections = IntDict MConnection
type alias Node = NodeContext Bool MConnection -- a Node indicates terminality


oneTransition : AutomatonGraph a -> ExecutionState -> ExecutionState
oneTransition g executionState =
  let
    transitionWithData : ExecutionData -> ExecutionState
    transitionWithData data =
      Graph.get data.currentNode g.graph
      |> Maybe.map
        (\ctx ->
          case data.remainingData of
            [] ->
              executionState -- we're done! Can't go any further!
            h::t ->
              ctx.outgoing
              |> IntDict.toList
              |> List.findMap
                (\(k, conn) ->
                  if Set.member (h, 1) conn then
                    Just <| Accepted
                      { data
                        | transitionsTaken = (h, 1)::data.transitionsTaken
                        , remainingData = t
                        , currentNode = k
                      }
                  else if Set.member (h, 0) conn then
                    Just <| Rejected
                      { data
                        | transitionsTaken = (h, 0)::data.transitionsTaken
                        , remainingData = t
                        , currentNode = k
                      }
                  else
                    Nothing
                )
              |> Maybe.withDefault (NoPossibleTransition data)
        )
      |> Maybe.withDefault (RequestedNodeDoesNotExist data)
  in
    case executionState of
      Accepted d ->
        transitionWithData d
      Rejected d ->
        transitionWithData d
      RequestedNodeDoesNotExist _ ->
        executionState
      NoPossibleTransition _ ->
        executionState

step : AutomatonGraph a -> ExecutionResult -> ExecutionResult
step g executionResult =
  case executionResult of
    CanContinue executionState ->
      case oneTransition g executionState of
        Accepted ({ remainingData } as d) ->
          case remainingData of
            [] -> EndOfInput (Accepted d)
            _ -> CanContinue (Accepted d)
        Rejected ({ remainingData } as d) ->
          case remainingData of
            [] -> EndOfInput (Rejected d)
            _ -> CanContinue (Rejected d)
        RequestedNodeDoesNotExist d ->
          Debug.log "INTERNAL ERROR! NODE DOES NOT EXIST!" d |> \_ ->
          InternalError
        NoPossibleTransition d ->
          -- I only arrive here when there IS remaining data,
          -- but there IS NOT a matching transition for it.
          case d.transitionsTaken of
            [] ->
              -- Finality is stored on the TRANSITION, not the STATE.
              -- Therefore, I must take at least one TRANSITION to
              -- accept.  Otherwise—by default—I will reject.
              EndOfComputation (Rejected d)
            (_, 1)::_ ->
              EndOfComputation (Accepted d)
            (_, _)::_ -> -- i.e. (_, 0)::_
              EndOfComputation (Rejected d)
    EndOfInput _ ->
      executionResult
    EndOfComputation _ ->
      executionResult
    InternalError ->
      executionResult

stepThroughInitial : String -> AutomatonGraph a -> ExecutionResult
stepThroughInitial s g =
  step g
    (CanContinue <| Rejected <|
      ExecutionData [] (String.toList s) g.root
    )

run : String -> AutomatonGraph a -> ExecutionResult
run s g =
  let
    execute result =
      case step g result of
        (CanContinue _) as okay ->
          execute okay
        x ->
          x
  in
    execute (stepThroughInitial s g)

mkConn : String -> Connection
mkConn s =
  let
    helper xs acc =
      case xs of
        '!'::ch::rest ->
          helper rest (Set.insert (ch, 1) acc |> Set.remove (ch, 0))
        ch::rest ->
          helper rest (Set.insert (ch, 0) acc |> Set.remove (ch, 1))
        [] -> acc
  in
    helper (String.toList s) Set.empty

-- for use from CLI
mkDFA : List (NodeId, Char, NodeId) -> List NodeId -> DFARecord {} ()
mkDFA transitions finals =

  { states = List.concatMap (\(a, _, b) -> [(a, ()), (b, ())]) transitions |> IntDict.fromList
  , transition_function =
      List.foldl
        (\(a, ch, b) state ->
          IntDict.update a
            (\possible ->
              case possible of
                Nothing -> Just <| Dict.singleton ch b
                Just existing ->
                  -- overwrite if we have a collision.
                  Debug.log "Overwriting (new, existing)" ((a, ch, b), (a, existing)) |> \_ ->
                  Just <| Dict.insert ch b existing
            )
            state
        )
        IntDict.empty
        transitions
  , start =
      -- the start is taken as the very first state encountered
      case transitions of
        (s, _, _)::_ -> s
        _ -> 0
  , finals = Set.fromList finals
  }

mkAutomatonGraphWithValues : (NodeId -> a) -> List (NodeId, String, NodeId) -> AutomatonGraph a
mkAutomatonGraphWithValues valueFunction ts =
  let
    edges =
      List.foldl
        (\(src, s, dest) acc ->
          Dict.update (src, dest)
            (\item ->
              case item of
                Nothing ->
                  Just (mkConn s)
                Just existing ->
                  Just <| Set.union existing (mkConn s)
            )
            acc
        )
        Dict.empty
        ts
      |> Dict.toList
      |> List.map (\((src, dest), conn) -> Edge src dest conn)
    nodes =
      List.foldl
        (\(src, _, dest) acc -> Set.insert src acc |> Set.insert dest)
        Set.empty
        ts
      |> Set.toList
      |> List.map (\x -> Node x (valueFunction x))
  in
    case nodes of
      [] ->
        { graph = Graph.fromNodesAndEdges [Node 0 (valueFunction 0)] []
        , root = 0
        , maxId = 0
        }
      _ ->
        { graph =
            Graph.fromNodesAndEdges nodes edges
        , root =
            case ts of
              (src, _, _)::_ -> src
              _ -> 0
        , maxId = List.maximumBy .id nodes |> Maybe.map .id |> Maybe.withDefault 0
        }

mkAutomatonGraph : List (NodeId, String, NodeId) -> AutomatonGraph ()
mkAutomatonGraph =
  mkAutomatonGraphWithValues (\_ -> ())

extend : DFARecord a b -> DFARecord a b -> ExtDFA b
extend w_dfa_orig dfa = -- parameters: the w_dfa and the dfa
  let
    max_dfa =
      IntDict.findMax dfa.states
      |> Maybe.map (Tuple.first >> (+) 1)
      |> Maybe.withDefault 0
    max_w_dfa = IntDict.findMax w_dfa_orig.states
      |> Maybe.map (Tuple.first >> (+) 1)
      |> Maybe.withDefault 0
    w_dfa =
      { states =
          IntDict.toList w_dfa_orig.states |> List.map (\(a, b) -> (a + max_dfa, b)) |> IntDict.fromList
      , transition_function =
          IntDict.toList w_dfa_orig.transition_function
          |> List.map
              (\(a, d) ->
                (a + max_dfa, Dict.map (\_ b -> b + max_dfa) d)
              )
          |> IntDict.fromList
      , start = w_dfa_orig.start + max_dfa
      , finals = Set.map (\a -> a + max_dfa) w_dfa_orig.finals
      }
    unusedId = max_dfa + max_w_dfa + 1
    extDFA =
      { states = IntDict.union w_dfa.states dfa.states
        -- we amend this so that it has transition functions for cloned q0
      , transition_function = IntDict.union w_dfa.transition_function dfa.transition_function
          -- IntDict.get dfa.start dfa.transition_function
          -- |> Maybe.map (\to_add -> IntDict.insert unusedId to_add dfa.transition_function)
          -- |> Maybe.withDefault dfa.transition_function
          -- |> debugLog_ "Amended transitions" printTransitions
      , start = dfa.start
      , finals = Set.union w_dfa.finals dfa.finals
        -- the register, correctly, does NOT include the cloned q0
      , register = Set.fromList <| IntDict.keys dfa.states
      , clone_start = w_dfa.start
      , queue_or_clone = IntDict.keys w_dfa.states |> List.reverse
      , unusedId = unusedId
      }
      |> clone_or_queue dfa.start w_dfa.start
  in
    extDFA

retract : ExtDFA a -> DFARecord {} a
retract extDFA =
  { states = extDFA.states
  , transition_function = extDFA.transition_function
  , start = extDFA.start
  , finals = extDFA.finals
  }

{-| Create a DFA that accepts exactly one string. -}
add_string_to_dfa : String -> Maybe (DFARecord {} ())
add_string_to_dfa string =
  if String.isEmpty string then
    Nothing
  else
    Just -- <| debugDFA_ ("Creating single-string DFA for '" ++ string ++ "'") <|
      -- If this is a n-character string, then we will want n+1 states
      { states = IntDict.fromList (List.range 0 (String.length string) |> List.map (\i -> (i, ())))
      , start = 0
      , finals = Set.singleton (String.length string)
      , transition_function =
          IntDict.fromList
            ( case String.toList string of
                [] ->
                  []
                [ch] ->
                  [(0, Dict.singleton ch 1)]
                xs ->
                  List.foldl
                    (\ch (acc, nodeId) -> ( (nodeId, Dict.singleton ch (nodeId + 1)) :: acc, nodeId + 1 ))
                    ( [], 0 )
                    xs
                  |> Tuple.first
            )
      }

{-| Create a DFA that accepts exactly one string. -}
remove_string_from_dfa : String -> Maybe (DFARecord {} ())
remove_string_from_dfa string =
  if String.isEmpty string then
    Nothing
  else
    Just -- <| debugDFA_ ("Creating single-string DFA for '" ++ string ++ "'") <|
      -- If this is a n-character string, then we will want n+1 states
      { states = IntDict.fromList (List.range 0 (String.length string) |> List.map (\i -> (i, ())))
      , start = 0
      , finals = Set.empty -- this is quite literally the only difference between add/remove!
      , transition_function =
          IntDict.fromList
            ( case String.toList string of
                [] ->
                  []
                [ch] ->
                  [(0, Dict.singleton ch 1)]
                xs ->
                  List.foldl
                    (\ch (acc, nodeId) -> ( (nodeId, Dict.singleton ch (nodeId + 1)) :: acc, nodeId + 1 ))
                    ( [], 0 )
                    xs
                  |> Tuple.first
            )
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

type ForwardTree = PathEnd | ForwardNode (Dict Char ForwardTree)

{-| Return the nodes of the forwardtree, in-order.
-}
forwardtree_nodes : NodeId -> ExtDFA a -> ForwardTree -> List NodeId
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
          Dict.foldl
            (\ch nextTree state ->
              IntDict.get h extDFA_orig.transition_function
              |> Maybe.andThen
                (\destinations ->
                  Dict.get ch destinations
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

all_forward_transitions : NodeId -> ExtDFA a -> ForwardTree
all_forward_transitions start extDFA =
  let
    helper : NodeId -> Set NodeId -> ForwardTree
    helper current seen =
      case IntDict.get current extDFA.transition_function of
        Nothing -> PathEnd
        Just dict ->
          let
            filtered =
              Dict.filter
                (\_ state ->
                  not (Set.member state seen)
                  && current /= state
                )
                dict
            children =
              Dict.map (\_ state -> helper state (Set.insert state seen)) filtered
          in
            if Dict.isEmpty children then PathEnd else ForwardNode children
  in
    helper start Set.empty
    -- |> Debug.log "[all_forward_transitions] result"

w_forward_transitions : ExtDFA a -> ForwardTree
w_forward_transitions extDFA =
  let
    helper : NodeId -> Set NodeId -> ForwardTree
    helper current seen =
      case IntDict.get current extDFA.transition_function of
        Nothing -> PathEnd
        Just dict ->
          let
            filtered =
              Dict.filter
                (\_ state ->
                  not (Set.member state seen)
                  && current /= state
                  && List.member state extDFA.queue_or_clone
                )
                dict
            children =
              Dict.map (\_ state -> helper state (Set.insert state seen)) filtered
          in
            if Dict.isEmpty children then PathEnd else ForwardNode children
  in
    helper extDFA.clone_start Set.empty
    -- |> Debug.log "[w_forward_transitions] result"

delta : NodeId -> Char -> DFARecord a b -> Maybe NodeId
delta q x dfa =
  IntDict.get q dfa.transition_function
  |> Maybe.andThen (Dict.get x)

-- delta_star : NodeId -> List Char -> DFARecord a -> Maybe NodeId
-- delta_star q xs dfa =
--   List.foldl (\x -> Maybe.andThen (\q_ -> delta q_ x dfa)) (Just q) xs

transitions_from_source : NodeId -> DFARecord a b -> List (NodeId, Char)
transitions_from_source q dfa =
  IntDict.get q dfa.transition_function
  |> Maybe.map (Dict.toList >> List.map Tuple.Extra.flip)
  |> Maybe.withDefault []

{-| Returns a list of (NodeId, Char) pairs representing all transitions in the given DFA that lead to the specified node.

    transitions_to targetNode dfa

- `targetNode`: The NodeId to which transitions are sought.
- `dfa`: The DFARecord to search within.

Each pair consists of the source NodeId and the character label of the transition.
-}
transitions_to_dest : NodeId -> DFARecord a b -> List (NodeId, Char)
transitions_to_dest q dfa =
  IntDict.foldl
    (\source dict acc ->
      Dict.foldl
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

clone_or_queue : NodeId -> NodeId -> ExtDFA a -> ExtDFA a
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
            IntDict.insert q_w (Dict.union a b) extDFA.transition_function
    , finals =
        if Set.member q_m extDFA.finals then
          Set.insert q_w extDFA.finals
        else
          extDFA.finals
  }

clone_or_queue_many : NodeId -> NodeId -> ForwardTree -> ExtDFA a -> ExtDFA a
clone_or_queue_many q_m q_w tree extDFA =
  case tree of
    PathEnd ->
      -- we're at the end of the transitions.
      clone_or_queue q_m q_w extDFA
    ForwardNode dict ->
      Dict.foldl
        (\ch subtree acc ->
          case (delta q_m ch acc, delta q_w ch acc) of
            (_, Nothing) ->
              Debug.log "🚨 ERROR!! How can I fail to get a `w` transition via known `w`-transitions??" () |> \_ ->
              acc
            (Nothing, Just _) ->
              -- The q_m ends here, but q_w carries on. ∴ the remaining q_w must be "queued" nodes.
              clone_or_queue q_m q_w acc
            (Just m_node, Just w_node) ->
              clone_or_queue_many m_node w_node subtree (clone_or_queue q_m q_w acc)
        )
        extDFA
        dict

phase_1 : ExtDFA a -> ExtDFA a
phase_1 extDFA_orig =
  -- Traverse the ForwardTree and apply append_transitions for every path
  clone_or_queue_many
    extDFA_orig.start
    extDFA_orig.clone_start
    (w_forward_transitions extDFA_orig)
    extDFA_orig

remove_unreachable : ForwardTree -> ExtDFA a -> ExtDFA a
remove_unreachable old_w_path extDFA_orig =
  -- check: are there any incoming transitions from EXTERNAL nodes?
  -- "external" nodes are those nodes which are not along the w-path,
  -- as expressed by via the `w_forward_transitions` function.
  -- This should work because
  -- 1. We clone all nodes to a new start.  We therefore have an
  --    old-start and a new-start that are distinct.  We should always
  --    be able to remove the old-start (it has 0 incoming edges).
  -- 2. We redirected all outgoing edges of the existing nodes along the
  --    w-path, with the EXCEPTION of the w-path edges themselves.  So,
  --    if there are any incoming edges that come to the old w-path nodes,
  --    then they must be coming from:
  --    (a) the old w-path nodes.
  --    (b) non-w-path nodes.
  -- 3. After a node is removed, we can remove the next node (if it has 0
  --    incoming edges), and so on.  We stop whenever we have at least 1
  --    incoming edge.  This is according to Carrasco & Forcada's algorithm.
  --    However, if w-path nodes are recursive, then this fails, because each
  --    will have incoming edges from the other, and we can't clean them up.
  -- 4. So, instead, we will go for a slightly different algorithm.
  --    Step 1: collect all the nodes along the old w-path.  Place them in a
  --            set. 
  --    Step 2: partition the nodes into those which have an external incoming
  --            edge ("R"), and those which do not ("S").
  --    Step 3: moving in w-path order, examine the nodes of S.  If a node has
  --            an incoming node in R, add it to R and remove it from S.
  --            In other words, this is a partition refinement.
  --    Step 4: remove all the nodes in the S set.
  --    This should work because we are no longer relying on each node to be
  --    0-incoming BECAUSE of node-removal.  Instead, we are relying on each
  --    node to be 0-incoming after checking for external references directly.
  let
    -- Step 1
    old_w_path_nodes =
      forwardtree_nodes extDFA_orig.start extDFA_orig old_w_path
    old_w_path_set = Set.fromList old_w_path_nodes
    -- Step 2
    initial_partition : (List NodeId, List NodeId)
    initial_partition = -- ( with_external_edges, without_external_edges )
      old_w_path_set
      |> Set.foldl
        (\node (external, internal_only) ->
          let
            has_external_edges = -- are there any external incoming edges?
              transitions_to_dest node extDFA_orig -- incoming edges
              |> List.map Tuple.first
              |> List.any (\edge -> not (Set.member edge old_w_path_set)) -- externality
          in
            if has_external_edges then
              (node :: external, internal_only)
            else
              (external, node :: internal_only)
        )
        ([], [])
    -- Step 3
    -- make the final partition
    (with_external_edges, without_external_edges) =
      -- TODO: …this can probably be made less expensive by going the other way
      -- and looking at the destinations of with_external_edges. Oh well!
      let
        partition : List NodeId -> (Set NodeId, Set NodeId) -> (Set NodeId, Set NodeId)
        partition remaining (nodes_with_external_edges, nodes_without_external_edges) =
          case remaining of
            [] -> (nodes_with_external_edges, nodes_without_external_edges)
            node::t ->
              let
                incoming_nodes =
                  transitions_to_dest node extDFA_orig
                  |> List.map Tuple.first
                  |> Set.fromList
              in
                if Set.isEmpty (Set.intersect incoming_nodes nodes_with_external_edges) then
                  partition t (nodes_with_external_edges, nodes_without_external_edges)
                else
                  -- there is overlap, so this is in the first partition.
                  partition t (Set.insert node nodes_with_external_edges, Set.remove node nodes_without_external_edges)
      in
        partition
          (old_w_path_nodes {- |> Debug.log "checking in order" -}) -- must check in-order
          (Tuple.mapBoth Set.fromList Set.fromList initial_partition)
        --|> Debug.log "(to_keep, to_remove)"
    -- Step 4
    purge : NodeId -> ExtDFA a -> ExtDFA a
    purge q extDFA =
      { extDFA
        | states = IntDict.remove q extDFA.states
        , transition_function = IntDict.remove q extDFA.transition_function
        , register = Set.remove q extDFA.register
        , finals = Set.remove q extDFA.finals
        , queue_or_clone = List.filter ((/=) q) extDFA.queue_or_clone
      }
  in
    Set.foldl purge extDFA_orig without_external_edges

replace_or_register : ExtDFA a -> ExtDFA a
replace_or_register extDFA =
  let
    equiv : NodeId -> NodeId -> Bool
    equiv p q =
      let
        final_p = Set.member p extDFA.finals
        final_q = Set.member q extDFA.finals
      in
      if xor final_p final_q then
        False
      else
        let
          p_outgoing =
            IntDict.get p extDFA.transition_function
            -- |> Debug.log ("Checking 'p'-outgoing for " ++ String.fromInt p)
          q_outgoing =
            IntDict.get q extDFA.transition_function
            -- |> Debug.log ("Checking 'q'-outgoing for " ++ String.fromInt q)
        in
          case ( p_outgoing, q_outgoing ) of
            ( Just _, Nothing ) -> False
            ( Nothing, Just _ ) -> False
            _ -> p_outgoing == q_outgoing
    redirectInto : NodeId -> NodeId -> IntDict (Dict Char NodeId)
    redirectInto target source =
      -- redirect everything that goes to source, into target
      IntDict.map
        (\_ dict ->
          Dict.map
            (\_ candidate ->
              if candidate == source then
                target
              else
                candidate
            )
            dict
        )
        extDFA.transition_function
  in
  case extDFA.queue_or_clone of
    h::t ->
      case Set.toList extDFA.register |> List.find (equiv h) of
        Just found_equivalent ->
          -- Debug.log ("Registering " ++ String.fromInt h ++ " as equivalent to " ++ String.fromInt found_equivalent) () |> \_ ->
          replace_or_register
            { extDFA
              | states = IntDict.remove h extDFA.states
              , finals = Set.remove h extDFA.finals
              , transition_function =
                  redirectInto found_equivalent h
                  |> IntDict.remove h
              , queue_or_clone = t
              , start =
                  if h == extDFA.start then
                    found_equivalent -- replace with equivalent.
                  else
                    extDFA.start
            }
        Nothing ->
          -- Debug.log ("No equivalent found for " ++ String.fromInt h) () |> \_ ->
          replace_or_register
            { extDFA
              | register = Set.insert h extDFA.register
              , queue_or_clone = t
            }
    [] ->
      extDFA

union : DFARecord a b -> DFARecord a b -> DFARecord {} b
union w_dfa_orig m_dfa =
    extend w_dfa_orig m_dfa
    -- |> debugExtDFA_ "[union] extDFA creation from merged w_dfa + dfa"
    |> phase_1
    -- |> debugExtDFA_ "[union] End of Phase 1 (clone-and-queue)"
    |> (\extdfa -> remove_unreachable (w_forward_transitions extdfa) extdfa)
    |> (\dfa -> { dfa | start = dfa.clone_start })
    -- |> debugExtDFA_ "[union] End of Phase 2 (remove-unreachable + switch-start)"
    |> replace_or_register
    -- |> debugExtDFA_ "[union] End of Phase 3 (replace-or-register)"
    |> retract

{-
This is like union, but without the final step (register-or-replace).  That's
because, during user changes, if we join nodes too eagerly, we might end up
joining nodes that the user hasn't intended to join; fundamentally, the problem
is that we don't know (yet) if the right-language is complete. Until we know,
doing register-or-replace will be inaccurate.
-}
partial_union : DFARecord a b -> DFARecord a b -> DFARecord {} b
partial_union w_dfa_orig m_dfa =
    extend w_dfa_orig m_dfa
    -- |> debugExtDFA_ "[partial_union] extDFA creation from merged w_dfa + dfa"
    |> phase_1
    -- |> debugExtDFA_ "[partial_union] End of Phase 1 (clone-and-queue)"
    |> (\extdfa -> remove_unreachable (w_forward_transitions extdfa) extdfa)
    |> (\dfa -> { dfa | start = dfa.clone_start })
    -- |> debugExtDFA_ "[partial_union] End of Phase 2 (remove-unreachable + switch-start)"
    |> retract

complement : DFARecord a b -> DFARecord a b
complement dfa =
  -- the non-final states become the final states, and vice-versa.
  { dfa
    | finals = Set.diff (IntDict.keys dfa.states |> Set.fromList) dfa.finals
  }

modifyConnection : NodeId -> NodeId -> Connection -> AutomatonGraph a -> AutomatonGraph a
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
    craaazy_extend : DFARecord {} b -> ExtDFA b
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
    rewriteLink : AutomatonGraph a -> AutomatonGraph a
    rewriteLink g =
      { g
        | graph =
            Graph.update source
              (Maybe.map (\sourceContext ->
                { sourceContext
                  | outgoing =
                      if Set.isEmpty newConn then
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

removeConnection : NodeId -> NodeId -> AutomatonGraph a -> AutomatonGraph a
removeConnection source target =
  let
    craaazy_extend : DFARecord {} b -> ExtDFA b
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
    removeLink : AutomatonGraph a -> AutomatonGraph a
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

addString : String -> Maybe (DFARecord {} ()) -> Maybe (DFARecord {} ())
addString string maybe_dfa =
  let
    string_dfa = add_string_to_dfa string
  in
    case ( maybe_dfa, string_dfa ) of
      ( Nothing, _ ) ->
        string_dfa
      ( _, Nothing ) ->
        maybe_dfa
      ( Just dfa, Just s ) ->
        Just (union s dfa {- |> debugDFA_ ("after adding '" ++ string ++ "'") -})

removeString : String -> Maybe (DFARecord {} ()) -> Maybe (DFARecord {} ())
removeString string maybe_dfa =
  let
    string_dfa = remove_string_from_dfa string
  in
    case ( maybe_dfa, string_dfa ) of
      ( Nothing, _ ) ->
        string_dfa
      ( _, Nothing ) ->
        maybe_dfa
      ( Just dfa, Just s ) ->
        Just (union s dfa {- |> debugDFA_ ("after removing '" ++ string ++ "'") -})

wordsToDFA : List String -> DFARecord {} ()
wordsToDFA strings =
  List.foldl addString Nothing strings
  |> Maybe.withDefault
    { states = IntDict.empty
    , transition_function = IntDict.empty
    , start = -1
    , finals = Set.empty
    }

empty : a -> DFARecord {} a
empty defaultValue =
  { states = IntDict.singleton 0 defaultValue
  , transition_function = IntDict.empty
  , start = 0
  , finals = Set.empty
  }

fromWords : List String -> DFARecord {} ()
fromWords =
  List.foldl addString Nothing
  >> Maybe.withDefault (empty ())
  -- >> Maybe.map toGraph
  -- >> Maybe.withDefault Automata.Data.empty

debugFan_ : String -> IntDict Connection -> IntDict Connection
debugFan_ s fan =
  IntDict.toList fan
  |> List.map (\(id, conn) -> (id, Set.toList conn))
  |> Debug.log s
  |> \_ -> fan

minimisation_merge : NodeId -> NodeId -> AutomatonGraph a -> AutomatonGraph a
minimisation_merge head other g =
  let
    redirectFan : NodeId -> NodeId -> IntDict Connection -> IntDict Connection
    redirectFan from to fan =
      case IntDict.get from fan of
        Just conn ->
          case IntDict.get to fan of
            Just conn2 ->
              IntDict.insert to (Set.union conn conn2) fan
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
              (\_ -> Set.union)
              (redirectFan other head headNode.incoming {- |> debugFan_ "[minimisation_merge] headNode.incoming (redir)" -})
              (redirectFan other head otherNode.incoming {- |> debugFan_ "[minimisation_merge] otherNode.incoming (redir)"-})
            -- |> debugFan_ "[minimisation_merge] merged incoming"
          updatedOutgoing =
            IntDict.uniteWith
              (\_ -> Set.union)
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
    -- |> Automata.Debugging.debugAutomatonGraph ("[minimisation_merge] Post merge of #" ++ String.fromInt head ++ " and #" ++ String.fromInt other)

type alias TupleEdge = (NodeId, NodeId, Transition)
type alias Partition = Set NodeId
type alias HopcroftRecord =
  { w : List Partition -- still to be processed.
  , p : List Partition -- partitions
  }

hopcroft : AutomatonGraph a -> List (List Int)
hopcroft dawg =
  -- This is Hopcroft's Algorithm
  let
    edges = -- Edge (Transition)
      Graph.edges dawg.graph
      |> List.concatMap
        (\{from, to, label} ->
          Set.toList label
          |> List.map (\t -> (from, to, t))
        )
    (finals, nonFinals) = -- the initial partition.
      -- those which lead to finality, and those which don't.
      List.partition (\(_, _, (_, isFinal)) -> isFinal == 1) edges
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

minimiseNodesByCombiningTransitions : AutomatonGraph a -> AutomatonGraph a
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
                else if Set.foldl (\(_, f) acc -> acc || f == 1) False conn then
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
    fanOutEquals : NodeContext a Connection -> NodeContext a Connection -> Bool
    fanOutEquals a b =
      let
        redirected o id =
          IntDict.toList o
          -- |> Debug.log ("[minimiseNodes→fanOutEquals] outgoing of #" ++ String.fromInt id)
      in
      redirected a.outgoing a.node.id == redirected b.outgoing b.node.id
      -- |> Debug.log "[minimiseNodes→fanOutEquals] Are these equal?"
    classify : NodeContext a Connection -> AutomatonGraph a -> Maybe (AutomatonGraph a)
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
            -- |> Automata.Debugging.debugAutomatonGraph "[minimiseNodes] After merging T1 nodes"
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
              -- Automata.Debugging.println ("[minimiseNodes] 🕳️ Node #" ++ String.fromInt terminal.node.id ++ " is extended by #" ++ String.fromInt equivalent.node.id)
              Just (minimisation_merge terminal.node.id equivalent.node.id g)
            Nothing ->
              -- Debug.log ("[minimiseNodes] No suitable targets found; #" ++ String.fromInt terminal.node.id ++ " is not extended by any node.  Checking sources to see if it extends another.") () |> \_ ->
              case List.find (fanOutEquals terminal) sources of
                Just equivalent ->
                  -- Case T2, sub-case 2
                  -- Automata.Debugging.println ("[minimiseNodes] 🕳️ Node #" ++ String.fromInt terminal.node.id ++ " is an extension of #" ++ String.fromInt equivalent.node.id)
                  Just (minimisation_merge terminal.node.id equivalent.node.id g)
                Nothing ->
                  -- Debug.log ("[minimiseNodes] #" ++ String.fromInt terminal.node.id ++ " neither extends nor is extended.") () |> \_ ->
                  case targets of
                    m::_ ->
                      -- Debug.log "[minimiseNodes] Checking for common sources of target-node" m.node.id |> \_ ->
                      IntDict.get terminal.node.id m.incoming
                      |> Maybe.andThen
                        (\chosenConnection ->
                          -- Debug.log "[minimiseNodes] transition to follow back is" chosenConnection |> \_ ->
                          IntDict.toList m.incoming
                          |> List.filterMap
                            (\(s, conn) ->
                              if s /= terminal.node.id && conn == chosenConnection then
                                Graph.get s g.graph
                              else
                                Nothing
                            )
                          -- |> Debug.log ("[minimiseNodes] candidate nodes (excluding #" ++ String.fromInt terminal.node.id ++ ")")
                          |> List.find (fanOutEquals terminal)
                          -- |> Debug.log "[minimiseNodes] selected mergeable candidate"
                          |> Maybe.map
                            (\equivalent ->
                                -- Automata.Debugging.println ("[minimiseNodes] Node #" ++ String.fromInt terminal.node.id ++ " can be merged with node #" ++ String.fromInt equivalent.node.id)
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
      (g_ {- |> Automata.Debugging.debugAutomatonGraph "[minimiseNodes] Initial graph" -})
    -- |> Automata.Debugging.debugAutomatonGraph "[minimiseNodes] Final graph"


toAutomatonGraph : DFARecord a b -> AutomatonGraph b
toAutomatonGraph dfa =
  let
    stateList = IntDict.toList dfa.states |> List.reverse -- |> Debug.log "[toAutomatonGraph] State-list"
    graph =
      Graph.fromNodesAndEdges
        (stateList |> List.map (\(id, label) -> Node id label))
        (IntDict.toList (dfa {- |> debugDFA_ "[toAutomatonGraph] DFA as received" -}).transition_function
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
        -- |> debugAutomatonGraph "[toAutomatonGraph] Graph, since DFA was empty"
      h::_ ->
        { graph = graph
        , maxId = Tuple.first h -- |> Debug.log "[toGraph] maxId"
        , root = dfa.start -- |> Debug.log "[toGraph] root"
        }
        -- |> debugAutomatonGraph "[toAutomatonGraph] Graph, as converted from DFA"
        |> minimiseNodesByCombiningTransitions

fromGraph : NodeId -> Graph n Connection -> DFARecord {} n
fromGraph start graph =
  { graph = graph
  , root = start
  , maxId = List.maximum (Graph.nodes graph |> List.map .id) |> Maybe.withDefault 0
  }
  |> fromAutomatonGraph

renumberAutomatonGraph : AutomatonGraph a -> AutomatonGraph a
renumberAutomatonGraph g =
  let
    fanMapper =
      IntDict.toList >> List.map (\(k, conn) -> (k, Set.toList conn)) 
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
      |> List.indexedMap (\i node -> (node.node.id, i))
      |> IntDict.fromList
    get n =
      case IntDict.get n nodeMap of
        Nothing -> Debug.todo ("87TTGUEOU for" ++ String.fromInt n ++ " I SHOULD NEVER BE HERE!")
        Just i -> i
  in
    { graph =
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
    , maxId = IntDict.findMax nodeMap |> Maybe.map Tuple.second |> Maybe.withDefault 0
    , root = get g.root
    }
    -- |> debugAutomatonGraph "[renumberAutomatonGraph] result"

splitTerminalAndNonTerminal : AutomatonGraph a -> AutomatonGraph a
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
    isTerminal : (MTransition, Int) -> Bool
    isTerminal (_, isFinal) =
      isFinal == 1

    isNonTerminal : (MTransition, Int) -> Bool
    isNonTerminal (_, isFinal) =
      isFinal == 0

    -- Find the next unused node id
    nextId : Int
    nextId =
      g.maxId + 1

    -- For each node, determine if it needs to be split
    nodesToSplit =
      Graph.nodeIds g.graph
      |> List.filterMap (\id -> Graph.get id g.graph {- |> Debug.log ("Node for id" ++ String.fromInt id) -})
      |> List.filter (\node ->
        let
          incomingTransitions =
            node.incoming
            |> IntDict.values
            |> List.concatMap Set.toList
          -- hmm.  For recursion, outgoing is authoritative, and incoming does not show the incoming recursive edge.
          outgoingRecursive =
            (node.outgoing |> IntDict.get node.node.id |> Maybe.map Set.toList |> Maybe.withDefault [])
          allIncoming =
            outgoingRecursive ++ incomingTransitions
            -- |> Debug.log ("[splitTerminalAndNonTerminal] Incoming transitions to " ++ String.fromInt node.node.id)
          hasTerminal = List.any isTerminal allIncoming
          hasNonTerminal = List.any isNonTerminal allIncoming
        in
          hasTerminal && hasNonTerminal
      )
      -- |> debugLog_ "[splitTerminalAndNonTerminal] nodes to split" (Debug.toString << List.map (.node >> .id))

    -- Build a mapping from node id to new split node id (for non-terminal transitions)
    splitMap : Dict NodeId NodeId
    splitMap =
      List.indexedMap (\i node -> (node.node.id, nextId + i)) nodesToSplit
      |> Dict.fromList

    newMaxId = Dict.keys splitMap |> List.maximum |> Maybe.withDefault g.maxId

    -- Helper to update incoming edges for all nodes
    updateIncoming : List (NodeContext a Connection) -> List (NodeContext a Connection)
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
                            (tSet, ntSet) = Set.partition isTerminal conns
                          in
                            ( if Set.isEmpty tSet then tAcc else IntDict.insert src tSet tAcc
                            , if Set.isEmpty ntSet then ntAcc else IntDict.insert src ntSet ntAcc
                            )
                        )
                        (IntDict.empty, IntDict.empty)

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
    updateOutgoing : List (NodeContext a Connection) -> List (NodeContext a Connection)
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
                          Set.partition isTerminal conns
                        acc1 =
                          if not (Set.isEmpty tSet) then
                            IntDict.insert dest tSet acc
                          else
                            acc
                        acc2 =
                          if not (Set.isEmpty ntSet) then
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
    { graph = newGraph
    , maxId = newMaxId
    , root = g.root
    }
    -- |> Automata.Debugging.debugAutomatonGraph " after split"


ellipsis : Int -> String -> String
ellipsis n s =
  if String.length s > n then
    String.slice 0 (n - 1) s ++ "…"
  else
    s

stateIdentifierToString : StateIdentifier -> String
stateIdentifierToString (f, list) =
  (if f == 0 then "" else "*")
  ++ "["
  ++ (List.map String.fromInt list |> String.join ",")
  ++ "]"

tableToString : Table a -> String
tableToString table =
  Dict.toList table
  |> List.map
    (\(sourceIdentifier, columnDict) ->
      Dict.foldl
        (\ch (destIdentifier, v) acc ->
          acc
          ++ (String.fromChar ch) ++ "→"
          ++ String.padRight 18 ' ' (stateIdentifierToString destIdentifier ++ ":" ++ ellipsis 11 (Debug.toString v))
        )
        (String.padRight 10 ' ' (stateIdentifierToString sourceIdentifier))
        columnDict
    )
  |> String.join "\n"

debugTable_ : String -> Table a -> Table a
debugTable_ s t =
  Debug.log (s ++ ":\n" ++ tableToString t) () |> \_ -> t

type alias Table a = Dict StateIdentifier (Column a)
type alias Column a = Dict Char (StateIdentifier, a)
type alias StateIdentifier = (Int, List NodeId)
nfaToDFA : AutomatonGraph a -> AutomatonGraph a
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
    normalizeSet = List.sort >> List.Extra.unique

    populateColumnData : IntDict Connection -> Column a -> Column a
    populateColumnData outgoing columnDict =
      IntDict.foldl
        (\destId conn columnDict_ ->
          Set.foldl
            (\(ch, f) d ->
              case Dict.get ch d of
                Nothing ->
                  -- Debug.log ("Inserting first " ++ String.fromChar ch ++ "-transition (" ++ (if f == 0 then "non-" else "") ++ "Final), to #" ++ String.fromInt destId) () |> \_ ->
                  Graph.get destId g.graph
                  |> Maybe.map (\{node} -> Dict.insert ch ((f, [destId]), node.label) d)
                  |> Maybe.Extra.withDefaultLazy (\() -> Debug.todo ("BGFOEK " ++ String.fromInt destId))
                Just ((f2, list), v) ->
                  -- Debug.log ("Inserting another " ++ String.fromChar ch ++ "-transition (" ++ (if f == 0 then "non-" else "") ++ "Final), to #" ++ String.fromInt destId) () |> \_ ->
                  -- if any of the transitions is final, then the created state will be final
                  Dict.insert ch ((max f f2, normalizeSet (destId::list)), v) d
            )
            columnDict_
            conn
        )
        columnDict
        outgoing

    -- this is plausible here only because I have split terminal & non-terminal
    -- BEFORE this function is called.  Otherwise, it is probably nonsense…
    terminalityOf : Graph.NodeContext n Connection -> Int
    terminalityOf node =
      IntDict.values node.incoming
      -- has at least one incoming terminal transition.
      |> List.any (\conn -> Set.filter (\(_, f) -> f == 1) conn |> (not << Set.isEmpty))
      |> \b -> if b then 1 else 0

    -- Get all transitions from the original NFA and organize them
    initialTable : Table a
    initialTable =
      Graph.fold
        (\node rowDict ->
          populateColumnData node.outgoing Dict.empty
          |> \d ->
            Dict.insert (terminalityOf node, [node.node.id]) d rowDict
        )
        Dict.empty
        g.graph
      -- |> debugTable_ "[nfaToDFA] Initial table"

    -- Build the complete table by adding rows for multi-element dest-sets
    buildCompleteTable : Table a -> Table a
    buildCompleteTable table =
      let
        -- Find all dest-sets that have more than one element and aren't already in the table
        newSourceSets =
          table
            |> Dict.values
            |> List.concatMap Dict.values
            |> List.map (\(identifier, _) -> identifier)
            |> List.filter (\(_, destSet) -> List.length destSet > 1)
            -- |> List.map normalizeSet
            |> List.Extra.unique
            |> List.filter (\identifier -> not (Dict.member identifier table))

        -- Create new rows for these source-sets
        newTable =
          List.foldl
            (\(finality, sourceSet) rowDict ->
              sourceSet
              |> List.filterMap (\id -> Graph.get id g.graph)
              |> List.foldl (\{outgoing} -> populateColumnData outgoing) Dict.empty
              |> \d -> Dict.insert (finality, sourceSet) d rowDict
            )
            table
            newSourceSets
      in
        case newSourceSets of
          [] -> table
          _ -> buildCompleteTable newTable

    completeTable : Table a
    completeTable =
      buildCompleteTable initialTable
      -- |> debugTable_ "[nfaToDFA] Complete table"

    -- Step 1: Merge states with identical cell values
    rename : StateIdentifier -> Set StateIdentifier -> Table a -> Table a
    rename new_name old_names table =
      let
        with_renamed_columns =
          Dict.map
            (\_ columnDict ->
              columnDict
              |> Dict.map (\_ (identifier, v) ->
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

    mergeIdenticalStates : List (StateIdentifier, b) -> Table a -> Table a
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

    mergedTable : Table a
    mergedTable =
      let
        -- Group source-sets by their transition dictionaries
        groupedByTransitions : Table a
        groupedByTransitions =
          completeTable
          |> Dict.toList
          |> List.Extra.gatherEqualsBy
              (\((finality, _), transitionsDict) ->
                -- considered equivalent, and thus mergeable, on the basis of:
                -- 1. Finality status
                -- 2. Character transition
                -- 3. Destination state-set
                ( finality
                , transitionsDict
                  |> Dict.toList
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
    removeUnreachableStates : Table a -> Table a
    removeUnreachableStates table = -- not checked…
      let
        rootIdentifier =
          Graph.get g.root g.graph
          |> Maybe.map (\node -> (terminalityOf node, [node.node.id]))
          |> Maybe.Extra.withDefaultLazy (\() -> (0, [g.root]) |> Debug.log "Y>YWYAT")
        
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
                    |> Maybe.withDefault Dict.empty
                    |> Dict.values
                    |> List.map Tuple.first
                    |> List.filter (\destIdentifier -> not (Set.member destIdentifier newVisited))                  
                in
                findReachable (rest ++ destinations) newVisited

        reachableStates = findReachable [rootIdentifier] Set.empty
      in
        table
        |> Dict.filter (\sourceSet _ -> Set.member sourceSet reachableStates)

    reachableTable : Table a
    reachableTable =
      removeUnreachableStates mergedTable
      -- |> debugTable_ "[nfaToDFA] Unreachable cell values have been removed"

    -- Step 3: Rename rows using maxId
    (finalTable, new_maxId) =
      let
        sourceSets = Dict.keys reachableTable |> List.filter (\(_, list) -> List.length list > 1)
        baseId = g.maxId + 1
        
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
        ( renamedTable -- |> debugTable_ "[nfaToDFA] Rows have been renamed"
        , g.maxId + List.length renameMapping
        )

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
                  |> List.findMap
                    ( Dict.Extra.find (\_ ((_, v), _) -> v == idList)
                      >> Maybe.map (\(_, (_, label)) -> (id, label))
                    )
        )
        (Dict.keys finalTable)
      |> List.map (\(id, v) -> Node id v)

    newGraphEdges =
      Dict.foldl
        (\(_, sourceSet) columnDict acc ->
          case sourceSet of
            [src] ->
              Dict.foldl
                (\ch ((f, destSet), _) acc_ ->
                  case destSet of
                    [dest] ->
                      Dict.update (src, dest)
                        (\item ->
                          case item of
                            Nothing ->
                              Just <| Set.singleton (ch, f)
                            Just conn ->
                              Just <| Set.insert (ch, f) conn
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
    { graph = newGraph
    , root = g.root
    , maxId = new_maxId
    }
    -- |> Automata.Debugging.debugAutomatonGraph "[nfaToDFA] Resulting graph"

fromAutomatonGraphHelper : AutomatonGraph a -> DFARecord {} a
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
                Set.foldl (\(char,_) -> Dict.insert char dest) dict conn
              )
              Dict.empty
              ctx.outgoing
            |> \dict -> IntDict.insert ctx.node.id dict transitions
        )
        IntDict.empty
        g.graph
  }

fromAutomatonGraph : AutomatonGraph a -> DFARecord {} a
fromAutomatonGraph =
    -- Automata.Debugging.debugAutomatonGraph "[fromAutomatonGraph] Graph as received" >>
    splitTerminalAndNonTerminal
    -- >> Automata.Debugging.debugAutomatonGraph "[fromAutomatonGraph] Graph after splitting the joined terminal+non-terminal nodes"
    >> nfaToDFA
    -- >> Automata.Debugging.debugAutomatonGraph "[fromAutomatonGraph] Graph NFA→DFA conversion"
    >> fromAutomatonGraphHelper
    -- >> debugDFA_ "[fromAutomatonGraph] Graph→DFA"

connectionToString : MConnection -> String
connectionToString =
  Set.map String.fromChar
  >> Set.toList
  >> String.concat

-----------------
-- DEBUGGING
-----------------

printTransitions : IntDict (Dict Char NodeId) -> String
printTransitions transitions =
  (IntDict.foldl
    (\k dict acc ->
      Dict.toList dict
      |> List.map (\(transition, to) -> String.fromInt k ++ "→" ++ String.fromInt to ++ " (" ++ String.fromChar transition ++ ")")
      |> (++) acc
    )
    []
    transitions
  |> String.join ", ")

printDFA : DFARecord a b -> String
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

printExtDFA : ExtDFA a -> String
printExtDFA extDFA =
  printDFA extDFA
  ++ "\n▶ Register / Queue|Clones = "
  ++ Debug.toString (Set.toList extDFA.register) ++ " / "
  ++ Debug.toString extDFA.queue_or_clone ++ "📍"
  ++ String.fromInt extDFA.clone_start
  ++ "; unused: " ++ String.fromInt extDFA.unusedId
  -- ++ "\n  ▶ Register: " ++ Debug.toString (Set.toList extDFA.register)
  -- ++ "\n  ▶ Queue/Clones: " ++ Debug.toString extDFA.queue_or_clone
  -- ++ "\n  ▶ clone_start: " ++ String.fromInt extDFA.clone_start
  -- ++ "\n  ▶ unusedId: " ++ String.fromInt extDFA.unusedId

debugDFA_ : String -> DFARecord a b -> DFARecord a b
debugDFA_ s dfa =
  Debug.log (s ++ ": " ++ printDFA dfa) () |> \_ -> dfa

debugExtDFA_ : String -> ExtDFA a -> ExtDFA a
debugExtDFA_ s extDFA =
  Debug.log (s ++ ": " ++ printExtDFA extDFA) () |> \_ -> extDFA

debugDFA : DFARecord a b -> DFARecord a b
debugDFA dfa =
  Debug.log (printDFA dfa) () |> \_ -> dfa

debugExtDFA : ExtDFA a -> ExtDFA a
debugExtDFA extDFA =
  Debug.log (printExtDFA extDFA) () |> \_ -> extDFA

debugLog : (a -> String) -> a -> a
debugLog f a =
  Debug.log (f a) () |> \_ -> a

debugLog_ : String -> (a -> String) -> a -> a
debugLog_ s f a =
  Debug.log (s ++ ": " ++ f a) () |> \_ -> a