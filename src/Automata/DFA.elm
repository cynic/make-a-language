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

{- So with an DFA, I can basically do anything that's deterministic.

Looping back to previous nodes? Go ahead.
Looping back to ourselves? Definitely!

Now with that said, nodes without outgoing edges SHOULD be terminal nodes.
And everything is connected.  And outgoing nodes have deterministic transitions.
-}

type alias NodeId = Int
type alias ATransition = ( (NodeId, Char), NodeId )

type alias DFARecord extending label =
  { extending |
    states : IntDict label -- the () is the label.
  , transition_function: IntDict (Dict Char NodeId) -- NodeId × Char → NodeId
  , start : NodeId
  , finals : Set NodeId
  }

type alias ExtDFA label =
  { states : IntDict label
  , transition_function: IntDict (Dict Char NodeId)
  , start : NodeId
  , finals : Set NodeId
  , register : Set NodeId
  , clone_start : NodeId
  , queue_or_clone : List NodeId
  , unusedId : NodeId
  }

type alias CloneResult a =
  { extDFA : ExtDFA a
  , q_w : Maybe NodeId -- Nothing, if we've reached the end.
  , q_m : Maybe NodeId -- Nothing, if we reached a break in the transitions.
  , seen_qw : Set NodeId -- the states of q_w we've seen thus far.
  }

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

mkAutomatonGraph : List (NodeId, String, NodeId) -> AutomatonGraph ()
mkAutomatonGraph ts =
  let
    edges =
      List.foldl
        (\(src, s, dest) acc ->
          Dict.update (src, dest)
            (\item ->
              let
                transition =
                  case String.toList s of
                    ['!', ch] -> (ch, 1)
                    [ch] -> (ch, 0)
                    _ -> ('🛈', 0)
              in
                case item of
                  Nothing ->
                    Just <| Set.singleton transition
                  Just conn ->
                    Just <| Set.insert transition conn
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
      |> List.map (\x -> Node x ())
  in
    { graph =
        Graph.fromNodesAndEdges nodes edges
    , root =
        case ts of
          (src, _, _)::_ -> src
          _ -> 0
    , maxId = List.maximumBy .id nodes |> Maybe.map .id |> Maybe.withDefault 0
    }

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
    |> Debug.log "all_forward_transitions"

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
    |> Debug.log "w_forward_transitions"

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
    |> debugExtDFA_ "extDFA creation from merged w_dfa + dfa"
    |> phase_1
    |> debugExtDFA_ "End of Phase 1 (clone-and-queue)"
    |> (\extdfa -> remove_unreachable (w_forward_transitions extdfa) extdfa)
    |> (\dfa -> { dfa | start = dfa.clone_start })
    |> debugExtDFA_ "End of Phase 2 (remove-unreachable + switch-start)"
    |> replace_or_register
    |> debugExtDFA_ "End of Phase 3 (replace-or-register)"
    |> retract

complement : DFARecord a b -> DFARecord a b
complement dfa =
  -- the non-final states become the final states, and vice-versa.
  { dfa
    | finals = Set.diff (IntDict.keys dfa.states |> Set.fromList) dfa.finals
  }

modifyConnection : NodeId -> NodeId -> Connection -> AutomatonGraph a -> Graph a Connection
modifyConnection source target newConn g =
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
    rewriteLink : Graph a Connection -> Graph a Connection
    rewriteLink =
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
  in
    rewriteLink g.graph
    |> Automata.Debugging.debugGraph "[modifyConnection] After rewriteLink"
    |> fromGraph g.root
    |> debugDFA_ "[modifyConnection] After conversion to DFA"
    |> craaazy_extend
    |> debugExtDFA_ "[modifyConnection] After craaazy extension…"
    |>( \dfa ->
          if Set.isEmpty newConn then
            remove_unreachable (all_forward_transitions target dfa) dfa
          else
            dfa -- skip step; nothing is disconnected by this.
      )
    |> (\dfa -> { dfa | start = dfa.clone_start })
    |> debugExtDFA_ "[modifyConnection] If newConn was empty, this is also after remove_unreachable"
    |> replace_or_register
    |> debugExtDFA_ "[modifyConnection] After replace_or_register"
    |> retract
    |> toAutomatonGraph
    |> .graph

removeConnection : NodeId -> NodeId -> AutomatonGraph a -> Graph a Connection
removeConnection a b g =
  modifyConnection a b Set.empty g

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

minimiseNodesByCombiningTransitions : Set NodeId -> Graph a Connection -> Graph a Connection
minimiseNodesByCombiningTransitions finals_from_dfa g_ =
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
    fanEquals a b =
      IntDict.toList a == IntDict.toList b
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
    merge : NodeId -> NodeId -> Graph a Connection -> Graph a Connection
    merge head other g =
      Maybe.map2
        (\headNode otherNode ->
          let
            updatedIncoming =
              IntDict.uniteWith
                (\_ -> Set.union)
                (redirectFan other head headNode.incoming {- |> debugFan_ "[merge] headNode.incoming (redir)" -})
                (redirectFan other head otherNode.incoming {- |> debugFan_ "[merge] otherNode.incoming (redir)"-})
              -- |> debugFan_ "[merge] merged incoming"
            updatedOutgoing =
              IntDict.uniteWith
                (\_ -> Set.union)
                (redirectFan other head headNode.outgoing {- |> debugFan_ "[merge] headNode.outgoing (redir)" -})
                (redirectFan other head otherNode.outgoing {- |> debugFan_ "[merge] otherNode.outgoing (redir)" -})
              -- |> debugFan_ "[merge] merged outgoing"
          in
            Graph.insert { headNode | incoming = updatedIncoming , outgoing = updatedOutgoing } g
            |> Graph.remove other
        )
        (Graph.get head g)
        (Graph.get other g)
      |> Maybe.withDefault g
      |> debugGraph ("[minimiseNodes] Post merge of #" ++ String.fromInt head ++ " and #" ++ String.fromInt other)
    classify : NodeId -> Graph a Connection -> Maybe (Graph a Connection)
    classify terminal g =
      -- classify the terminal node into one of the four classes
      Graph.get terminal g
      |> Maybe.andThen
        (\node ->
          if IntDict.isEmpty node.outgoing then
            -- case T1
            let
              otherEmptyOutgoing =
                Graph.fold
                  (\nodeContext state ->
                    if IntDict.isEmpty nodeContext.outgoing && nodeContext.node.id /= node.node.id then
                      nodeContext.node.id :: state
                    else
                      state
                  )
                  []
                  g
                |> Debug.log ("[minimiseNodes] Nodes (besides #" ++ String.fromInt node.node.id ++ ") with no outgoing")
              newGraph =
                List.foldl
                  (\nodeId state -> merge node.node.id nodeId state)
                  g
                  otherEmptyOutgoing
                |> debugGraph "[minimiseNodes] After merging T1 nodes"
            in
              case otherEmptyOutgoing of
                [] -> Nothing
                _ -> Just newGraph
          else
            let
              targets =
                IntDict.keys node.outgoing
                |> List.filterMap (\target -> Graph.get target g)
              sources =
                IntDict.keys node.incoming
                |> List.filterMap (\source -> Graph.get source g)
            in
              case List.find (\target -> target.node.id /= node.node.id && fanEquals target.outgoing node.outgoing) targets of
                Just equivalent ->
                  -- Case T2, sub-case 1
                  Automata.Debugging.println ("[minimiseNodes] Node #" ++ String.fromInt node.node.id ++ " is extended by #" ++ String.fromInt equivalent.node.id)
                  Just (merge node.node.id equivalent.node.id g)
                Nothing ->
                  case List.find (\source -> source.node.id /= node.node.id && fanEquals source.outgoing node.outgoing) sources of
                    Just equivalent ->
                      -- Case T2, sub-case 2
                      Automata.Debugging.println ("[minimiseNodes] Node #" ++ String.fromInt node.node.id ++ " is an extension of #" ++ String.fromInt equivalent.node.id)
                      Just (merge node.node.id equivalent.node.id g)
                    Nothing ->
                      case List.filter (\t -> t.node.id /= node.node.id) targets of
                        m::_ ->
                          IntDict.get node.node.id m.incoming
                          |> Maybe.andThen
                            (\chosenConnection ->
                              IntDict.toList m.incoming
                              |> List.filterMap
                                (\(s, conn) ->
                                  if s /= node.node.id && conn == chosenConnection then
                                    Graph.get s g
                                  else
                                    Nothing
                                )
                              |> List.find
                                (\n ->
                                  fanEquals n.outgoing node.outgoing
                                )
                              |> Maybe.map
                                (\equivalent ->
                                    Automata.Debugging.println ("[minimiseNodes] Node #" ++ String.fromInt node.node.id ++ " can be merged with node #" ++ String.fromInt equivalent.node.id)
                                    merge node.node.id equivalent.node.id g
                                )
                            )
                        [] ->
                          Nothing
        )
    classify_all terminals g =
      case terminals of
        [] -> g
        t::ts ->
          case classify t g of
            Nothing ->
              classify_all ts g
            Just newG ->
              classify_all terminals newG
  in
    classify_all (Set.toList finals_from_dfa) (g_ |> debugGraph "[minimiseNodes] Initial graph")


toAutomatonGraph : DFARecord a b -> AutomatonGraph b
toAutomatonGraph dfa =
  let
    stateList = IntDict.toList dfa.states |> List.reverse --|> Debug.log "[toGraph] State-list"
    graph =
      Graph.fromNodesAndEdges
        (stateList |> List.map (\(id, label) -> Node id label))
        (IntDict.toList dfa.transition_function
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
      |> minimiseNodesByCombiningTransitions dfa.finals
  in
    case stateList of
      [] ->
        Automata.Data.empty
      h::_ ->
        { graph = graph
        , maxId = Tuple.first h -- |> Debug.log "[toGraph] maxId"
        , root = dfa.start -- |> Debug.log "[toGraph] root"
        }

fromGraph : NodeId -> Graph n Connection -> DFARecord {} n
fromGraph start graph =
  { graph = graph
  , root = start
  , maxId = List.maximum (Graph.nodes graph |> List.map .id) |> Maybe.withDefault 0
  }
  |> fromAutomatonGraph


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
            -- |> Debug.log ("Incoming transitions to " ++ String.fromInt node.node.id)
          hasTerminal = List.any isTerminal allIncoming
          hasNonTerminal = List.any isNonTerminal allIncoming
        in
          hasTerminal && hasNonTerminal
      )
      |> debugLog_ "nodes to split" (Debug.toString << List.map (.node >> .id))

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
    |> Automata.Debugging.debugAutomatonGraph " after split"


ellipsis : Int -> String -> String
ellipsis n s =
  if String.length s > n then
    String.slice 0 (n - 1) s ++ "…"
  else
    s

tableToString : Dict (List NodeId) (Dict (Char, Int) (List NodeId, a)) -> String
tableToString table =
  Dict.toList table
  |> List.map
    (\(sourceSet, columnDict) ->
      Dict.foldl
        (\transition (destSet, v) acc ->
          acc
          ++ (transitionToString transition) ++ "→"
          ++ String.padRight 18 ' ' (Debug.toString destSet ++ ":" ++ ellipsis 11 (Debug.toString v))
        )
        (String.padRight 10 ' ' (Debug.toString sourceSet))
        columnDict
    )
  |> String.join "\n"

debugTable_ : String -> Dict (List NodeId) (Dict (Char, Int) (List NodeId, a)) -> Dict (List NodeId) (Dict (Char, Int) (List NodeId, a))
debugTable_ s t =
  Debug.log (s ++ ":\n" ++ tableToString t) () |> \_ -> t

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

  (
    although we use the suffix "-set", they're actually lists; and they're all
    sorted
  )

  The exact type is: Dict (List NodeId) (Dict (Char, Int) (List NodeId, a))

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

    populateColumnData : IntDict Connection -> Dict Transition (List NodeId, a) -> Dict Transition (List NodeId, a)
    populateColumnData outgoing columnDict =
      IntDict.foldl
        (\destId conn columnDict_ ->
          Set.foldl
            (\transition d ->
              case Dict.get transition d of
                Nothing ->
                  Graph.get destId g.graph
                  |> Maybe.map (\{node} -> Dict.insert transition ([destId], node.label) d)
                  |> Maybe.Extra.withDefaultLazy (\() -> Debug.todo ("BGFOEK " ++ String.fromInt destId))
                Just (list, v) ->
                  Dict.insert transition (normalizeSet (destId::list), v) d
            )
            columnDict_
            conn
        )
        columnDict
        outgoing

    -- Get all transitions from the original NFA and organize them
    -- Type: Dict (List NodeId) (Dict (Char, Int) (List NodeId, a))
    initialTable : Dict (List NodeId) (Dict Transition (List NodeId, a))
    initialTable =
      Graph.fold
        (\node rowDict ->
          populateColumnData node.outgoing Dict.empty
          |> \d -> Dict.insert [node.node.id] d rowDict
        )
        Dict.empty
        g.graph
      |> debugTable_ "[nfaToDFA] Initial table"

    -- Build the complete table by adding rows for multi-element dest-sets
    buildCompleteTable : Dict (List NodeId) (Dict Transition (List NodeId, a)) -> Dict (List NodeId) (Dict (Char, Int) (List NodeId, a))
    buildCompleteTable table =
      let
        -- Find all dest-sets that have more than one element and aren't already in the table
        newSourceSets =
          table
            |> Dict.values
            |> List.concatMap Dict.values
            |> List.map Tuple.first
            |> List.filter (\destSet -> List.length destSet > 1)
            -- |> List.map normalizeSet
            |> List.Extra.unique
            |> List.filter (\sourceSet -> not (Dict.member sourceSet table))

        -- Create new rows for these source-sets
        newTable =
          List.foldl
            (\sourceSet rowDict ->
              sourceSet
              |> List.filterMap (\id -> Graph.get id g.graph)
              |> List.foldl (\{outgoing} -> populateColumnData outgoing) Dict.empty
              |> \d -> Dict.insert sourceSet d rowDict
            )
            table
            newSourceSets
      in
        case newSourceSets of
          [] -> table
          _ -> buildCompleteTable newTable

    completeTable =
      buildCompleteTable initialTable
      |> debugTable_ "[nfaToDFA] Complete table"

    -- Step 1: Merge states with identical cell values
    rename : List NodeId -> Set (List NodeId) -> Dict (List NodeId) (Dict Transition (List NodeId, a)) -> Dict (List NodeId) (Dict Transition (List NodeId, a))
    rename new_name old_names table =
      let
        with_renamed_columns =
          Dict.map
            (\_ columnDict ->
              columnDict
              |> Dict.map (\_ (destSet, v) ->
                if Set.member destSet old_names then
                  (new_name, v)
                else
                  (destSet, v)
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

    mergeIdenticalState : ((List NodeId, b), List (List NodeId, b)) -> Dict (List NodeId) (Dict Transition (List NodeId, a)) -> Dict (List NodeId) (Dict Transition (List NodeId, a))
    mergeIdenticalState ((mergeHead_sourceSet, _), to_merge) table =
      let
        sourceSets_to_merge = List.map Tuple.first to_merge |> Set.fromList
        without_merged =
          Set.foldl (\sourceSet table_ -> Dict.remove sourceSet table_) table sourceSets_to_merge
      in
        rename mergeHead_sourceSet sourceSets_to_merge without_merged

    mergedTable =
      let
        -- Group source-sets by their transition dictionaries
        groupedByTransitions =
          completeTable
          |> Dict.toList
          |> List.Extra.gatherEqualsBy (\(_, transitions) -> transitions)
          |> List.filter (\(_, group) -> List.length group > 1)
          |> List.foldl mergeIdenticalState completeTable
      in
        groupedByTransitions
        |> debugTable_ "[nfaToDFA] Identical cell values have been merged"

    -- Step 2: Remove unreachable states (keep only those reachable from root)
    removeUnreachableStates : Dict (List NodeId) (Dict (Char, Int) (List NodeId, a)) -> Dict (List NodeId) (Dict (Char, Int) (List NodeId, a))
    removeUnreachableStates table = -- not checked…
      let
        rootSet = [g.root]
        
        findReachable : List (List NodeId) -> Set (List NodeId) -> Set (List NodeId)
        findReachable worklist visited =
          case worklist of
            [] -> visited
            currentSet :: rest ->
              if Set.member currentSet visited then
                findReachable rest visited
              else
                let
                  newVisited = Set.insert currentSet visited
                  destinations = 
                    Dict.get currentSet table
                    |> Maybe.withDefault Dict.empty
                    |> Dict.values
                    |> List.map Tuple.first
                    |> List.filter (\destSet -> not (Set.member destSet newVisited))
                in
                findReachable (rest ++ destinations) newVisited

        reachableStates = findReachable [rootSet] Set.empty
      in
        table
        |> Dict.filter (\sourceSet _ -> Set.member sourceSet reachableStates)

    reachableTable =
      removeUnreachableStates mergedTable
      |> debugTable_ "[nfaToDFA] Unreachable cell values have been removed"

    -- Step 3: Rename rows using maxId
    (finalTable, new_maxId) =
      let
        sourceSets = Dict.keys reachableTable |> List.filter (\sourceSet -> List.length sourceSet > 1)
        baseId = g.maxId + 1
        
        renameMapping = 
          sourceSets
          |> List.indexedMap (\i sourceSet -> (sourceSet, [baseId + i]))

        renamedTable =
          List.foldl
            (\(old_name, new_name) table ->
              rename new_name (Set.singleton old_name) table
            )
            reachableTable
            renameMapping
      in
        ( renamedTable |> debugTable_ "[nfaToDFA] Rows have been renamed", g.maxId + List.length renameMapping)

    -- Build the new DFA graph using the Graph API
    -- Create the new graph
    newGraphNodes =
      List.filterMap
        (\idList ->
          case idList of
            [] -> Nothing
            id::_ ->
              case Graph.get id g.graph of
                Just node -> -- in the original graph, just grab it from there
                  Just (id, node.node.label)
                Nothing -> -- was not in the original graph
                  Dict.values finalTable
                  |> List.findMap
                    ( Dict.Extra.find (\_ (v, _) -> v == idList)
                      >> Maybe.map (\(_, (_, label)) -> (id, label))
                    )
        )
        (Dict.keys finalTable)
      |> List.map (\(id, v) -> Node id v)

    newGraphEdges =
      Dict.foldl
        (\sourceSet columnDict acc ->
          case sourceSet of
            [src] ->
              Dict.foldl
                (\transition (destSet, _) acc_ ->
                  case destSet of
                    [dest] ->
                      Dict.update (src, dest)
                        (\item ->
                          case item of
                            Nothing ->
                              Just <| Set.singleton transition
                            Just conn ->
                              Just <| Set.insert transition conn
                        ) acc_
                    _ ->
                      acc_
                )
                acc
                columnDict
            _ ->
              acc
        )
        Dict.empty
        finalTable
      |> Dict.toList
      |> List.map (\((src, dest), conn) -> Edge src dest conn)

    newGraph =
      Graph.fromNodesAndEdges newGraphNodes newGraphEdges
      |> debugGraph "[nfaToDFA] Resulting graph"
  in
    { graph = newGraph
    , root = g.root
    , maxId = new_maxId
    }

fromAutomatonGraph : AutomatonGraph a -> DFARecord {} a
fromAutomatonGraph =
  let
    -- create the DFARecord.
    craft g =
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
  in
    Automata.Debugging.debugAutomatonGraph "before split"
    >> splitTerminalAndNonTerminal
    >> nfaToDFA
    >> craft

connectionToString : MConnection -> String
connectionToString =
  Set.map String.fromChar
  >> Set.toList
  >> String.concat

{-| Create a DFA consisting of all paths ending at the specified transition.
-}
wordsEndingAt : (NodeId, b) -> Graph.Graph b Connection -> Set NodeId -> DFARecord a b -> DFARecord a b
wordsEndingAt (nodeId, label) graph visited_orig dfa =
  let
    visited = Set.insert nodeId visited_orig
  in
  if nodeId == dfa.start && Set.member nodeId visited_orig then
    -- we are done!
    { dfa | states = IntDict.insert nodeId label dfa.states }
  else if Set.member nodeId visited_orig then
    -- I have visited this node before. The only way that can happen is
    -- if this node is "later" in the sequence.  If I follow it, we will
    -- enter a loop!  So, we don't take it.
    -- However, we must record this transition, and that is done during the
    -- call to the wordsEndingAt function.
    { dfa | states = IntDict.insert nodeId label dfa.states }
  else
    -- now, *I* must contribute to the `acc` myself.
    Graph.get nodeId graph
    |> Maybe.map
      (\node -> -- the `incoming` is an IntDict Connection
          -- my `acc` will ALSO have the things from MY `Connection`'s respective `outgoing`s.
          -- When I get called, that will already be in `acc`.
          node.incoming
          |> IntDict.toList
          |> List.foldl
            (\(nodeid, transitions) state ->
              -- each of the transitions effectively forms a new path back to `nodeid`
              Graph.get nodeid graph
              |> Maybe.map
                (\outNode ->
                  (Set.toList transitions
                  |> List.foldl
                      (\(t, isFinal) dfa_ ->
                        wordsEndingAt
                          (outNode.node.id, outNode.node.label)
                          graph
                          visited
                          { dfa_
                            | states = IntDict.insert nodeid outNode.node.label dfa_.states
                            , transition_function =
                                IntDict.update nodeid
                                  (\maybe_dict ->
                                      case maybe_dict of
                                        Nothing -> Just (Dict.singleton t nodeId)
                                        Just dict ->
                                          case Dict.get t dict of
                                            Just existing ->
                                              Just dict |> Debug.log ("conflict with existing (" ++ String.fromInt existing ++ "); taking existing.")
                                            Nothing ->
                                              Just (Dict.insert t nodeId dict)
                                  )
                                  dfa_.transition_function
                            , finals =
                                if isFinal == 1 then
                                  Set.insert nodeId dfa_.finals
                                else
                                  dfa_.finals
                          }
                      ) state -- link the nodeid to each transition
                  )
                )
              |> Maybe.Extra.withDefaultLazy (\() -> (dfa |> debugDFA_ ("[wordsEndingAt] OOPS!  Couldn't find ID " ++ String.fromInt nodeid))) -- should NEVER be here!!
            )
            { dfa | states = IntDict.insert nodeId node.node.label dfa.states }
            -- at the end of this, I should have a List (NodeId, Char) to process.
          -- Okay; by now, I have a list of places to GO, and the characters that will get me there.
      )
    |> Maybe.Extra.withDefaultLazy (\() -> Debug.todo "GIUK%S") -- SHOULD NEVER BE HERE!!!
    |> debugDFA_ ("[wordsEndingAt(" ++ String.fromInt nodeId ++ ")] Returning result")
    -- well, at this point, I have the DFA.  Now I need to union it.


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
  "  ▶ States: " ++
    ( List.map (Tuple.first >> String.fromInt) (IntDict.toList dfa.states)
      |> String.join ", "
    )
  ++ "\n  ▶ Transitions: " ++ printTransitions dfa.transition_function
  ++ "\n  ▶ Finals: " ++ Debug.toString (Set.toList dfa.finals)
  ++ "\n  ▶ Start: " ++ String.fromInt dfa.start

printExtDFA : ExtDFA a -> String
printExtDFA extDFA =
  printDFA extDFA
  ++ "\n  ▶ Register: " ++ Debug.toString (Set.toList extDFA.register)
  ++ "\n  ▶ Queue/Clones: " ++ Debug.toString extDFA.queue_or_clone
  ++ "\n  ▶ clone_start: " ++ String.fromInt extDFA.clone_start
  ++ "\n  ▶ unusedId: " ++ String.fromInt extDFA.unusedId

debugDFA_ : String -> DFARecord a b -> DFARecord a b
debugDFA_ s dfa =
  Debug.log (s ++ ":\n" ++ printDFA dfa) () |> \_ -> dfa

debugExtDFA_ : String -> ExtDFA a -> ExtDFA a
debugExtDFA_ s extDFA =
  Debug.log (s ++ ":\n" ++ printExtDFA extDFA) () |> \_ -> extDFA

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