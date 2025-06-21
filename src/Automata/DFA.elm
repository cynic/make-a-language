module Automata.DFA exposing (..)
import IntDict exposing (IntDict)
import Set exposing (Set)
import List.Extra as List
import Dict exposing (Dict)
import Automata.Data exposing (..)
import Graph exposing (Graph, NodeContext, Node, NodeId, Edge)
import Dict.Extra
import Dict exposing (update)
import Automata.MADFA exposing (toAutomatonGraph, collapse)
import Automata.MADFA exposing (MADFARecord)
import List.Extra exposing (remove)
import Maybe.Extra
import Html exposing (tr)
import Tuple.Extra

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
                IntDict.update target
                  (\_ ->
                    if Set.isEmpty newConn then
                      Nothing
                    else
                      Just newConn
                  )
                  sourceContext.outgoing
          }
        ))
  in
    rewriteLink g.graph
    |> fromGraph g.root
    |> craaazy_extend
    -- |> debugExtDFA_ "[modifyConnection] After craaazy extension…"
    |>( \dfa ->
          if Set.isEmpty newConn then
            remove_unreachable (all_forward_transitions target dfa) dfa
          else
            dfa -- skip step; nothing is disconnected by this.
      )
    |> (\dfa -> { dfa | start = dfa.clone_start })
    -- |> debugExtDFA_ "[modifyConnection] If newConn was empty, this is also after remove_unreachable"
    |> replace_or_register
    -- |> debugExtDFA_ "[modifyConnection] After replace_or_register"
    |> retract
    |> toGraph
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

toGraph : DFARecord a b -> AutomatonGraph b
toGraph dfa =
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

fromAutomatonGraph : AutomatonGraph a -> DFARecord {} a
fromAutomatonGraph g =
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

transitionToString : MTransition -> String
transitionToString =
  String.fromChar

connectionToString : MConnection -> String
connectionToString =
  Set.map transitionToString
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
                                        Just dict -> Just (Dict.insert t nodeId dict)
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
    |> Maybe.withDefault dfa -- SHOULD NEVER BE HERE!!!
    |> debugDFA_ ("[wordsEndingAt] Returning result")
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