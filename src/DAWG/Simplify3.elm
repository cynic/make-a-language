module DAWG.Simplify3 exposing (..)
import Graph exposing (Graph, NodeContext, Node, NodeId, Edge)
import IntDict exposing (IntDict)
import Set exposing (Set)
import List.Extra as List
import Maybe.Extra as Maybe
import Html.Attributes exposing (src)
import Dict exposing (Dict)
import DAWG.Data exposing (..)

type alias EdgeRecord = Edge Transition

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias Transition = Char
-- type alias Connection = Set Transition -- a Connection is a link between two nodes.
-- type alias Connections = IntDict Connection
type alias Node = NodeContext Bool Transition -- a Node indicates terminality
type alias FAGraph = Graph Bool Transition -- finally, the Finite Automaton.
type Automaton
  = MADFA MADFARecord -- minimal acyclic deterministic finite automaton
--  | DFA -- deterministic finite automaton

type alias RegisterValue = ( Int, List ( NodeId, Transition ) ) -- (isFinal, list-of-outgoing)

type alias MADFARecord =
  { graph : FAGraph
    {- The maximum ID-value in this graph -}
  , maxId : NodeId
  , root : NodeId
  , queue : Set NodeId
  , cloned : Set NodeId
  , register : Dict RegisterValue NodeId -- these ones do not need to be minimised. ( isFinal, list-of-outgoing )
  }

foldlSpecial : (a -> b -> b) -> (a -> b -> c) -> b -> List a -> Maybe c
foldlSpecial func lastItem acc list =
  case list of
    [] ->
      Nothing

    [x] ->
      Just <| lastItem x acc

    x :: xs ->
      foldlSpecial func lastItem (func x acc) xs

wordToTransitions : String -> List (Transition, Bool)
wordToTransitions txt =
  String.toList txt
  |> List.unconsLast
  |> Maybe.map (\(last, rest) -> List.map (\ch -> (ch, False)) rest ++ [(last, True)])
  |> Maybe.withDefault []

type DestinationSet
  = Queue
  | Cloned
  | Register
type InsertOptions
  = AddTo DestinationSet
  | MarkAsFinal
  | MarkAsInitial

follow_transition : Transition -> Maybe NodeId -> MADFARecord -> Maybe NodeId
follow_transition transition source madfa =
  source
  |> Maybe.andThen (\src -> Graph.get src madfa.graph)
  |> Maybe.andThen (\node ->
    case IntDict.toList node.outgoing |> List.filter (\(_, t) -> t == transition) of
      [] -> Nothing
      -- NOTE: we ASSUME determinism here.
      (dest, _)::_ -> Just dest
  )

clone : NodeId -> Bool -> MADFARecord -> (NodeId, MADFARecord)
clone source isFinal madfa =
  Graph.get source madfa.graph
  |> Maybe.map (\src_node ->
    ( madfa.maxId + 1 |> Debug.log ("Cloning #" ++ String.fromInt source ++ " with new id")
    , { madfa
      | graph =
          Graph.insert
            ( NodeContext
                (Node (madfa.maxId + 1) (src_node.node.label || isFinal)) -- new state that I'm creating.
                IntDict.empty -- incoming, which is empty.
                src_node.outgoing
            ) madfa.graph
      , maxId = madfa.maxId + 1
      , cloned = Set.insert (madfa.maxId + 1) madfa.cloned
      } --|> madfaLog
    )
  )
  |> Maybe.withDefault (source, madfa) -- which is total nonsense.

queue : Bool -> MADFARecord -> (NodeId, MADFARecord)
queue isFinal madfa =
  ( madfa.maxId + 1 |> Debug.log "Queuing new NodeId"
  , { madfa
    | graph =
        Graph.insert
          ( NodeContext
              (Node (madfa.maxId + 1) isFinal) -- new state that I'm creating.
              IntDict.empty
              IntDict.empty
          ) madfa.graph
    , maxId = madfa.maxId + 1
    , queue = Set.insert (madfa.maxId + 1) madfa.queue
    }
  )

addTransition : NodeId -> Transition -> NodeId -> MADFARecord -> MADFARecord
addTransition source transition destination madfa =
  { madfa
  | graph =
      Graph.update source
        (Maybe.map (\src_node ->
          { src_node
          | outgoing =
              IntDict.filter (\_ t -> t /= transition) src_node.outgoing -- get rid of any existing identical transitions
              |> IntDict.insert destination transition -- add the transition
              |> \v -> Debug.log ("Creating edge: #" ++ String.fromInt source ++ " -> #" ++ String.fromInt destination ++ " on transition " ++ String.fromChar transition ++ ", outgoing is now " ++ Debug.toString (IntDict.toList v)) () |> \_ -> v
          }
        ))
        madfa.graph
  }

remove_unreachable : NodeId -> List Transition -> MADFARecord -> MADFARecord
remove_unreachable current transitions madfa =
  case transitions of
    [] ->
      madfa
    h::rest ->
      let
        ( is_unreachable, q_next ) =
          Graph.get current madfa.graph
          |> Maybe.map (\node ->
            let
              next = IntDict.toList node.outgoing |> List.filter (\(_, t) -> t == h) |> List.map Tuple.first |> List.head
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
                |> (Debug.log ("Removing unreachable node #" ++ String.fromInt current ++ ", remaining in register"))
            }
          else
            Debug.log ("Node #" ++ String.fromInt current ++ " is not unreachable") () |> \_ ->
            madfa
      in
        case q_next |> Debug.log "Next node to check for unreachability" of
          Nothing ->
            updated_madfa
          Just next ->
            if is_unreachable then
              remove_unreachable next rest updated_madfa
            else
              updated_madfa

toRegisterValue : NodeId -> MADFARecord -> RegisterValue
toRegisterValue q madfa =
  Graph.get q madfa.graph
  |> Maybe.map (\node ->
    ( if node.node.label then 1 else 0
    , IntDict.toList node.outgoing
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
    new_incoming_states =
      IntDict.keys new_incoming
    new_incoming_keys =
      List.map (\k -> toRegisterValue k madfa) new_incoming_states
    new_madfa =
      { madfa
      | graph =
          Graph.update original
            (Maybe.map (\orig_node ->
              { orig_node
              | incoming = IntDict.union orig_node.incoming new_incoming
              }
            ))
            madfa.graph
          |> Graph.remove new
      }
    -- with_updated_register =
    --   { new_madfa
    --   | register =
    --       List.foldl Dict.remove madfa.register new_incoming_keys -- these are the states affected by the merge. Remove their original keys…
    --       |> \d -> List.foldl (\node state -> Dict.insert (toRegisterValue node new_madfa) node state) d new_incoming_states -- … and recompute them.
    --   }
  in
    --with_updated_register
    new_madfa
    |> madfaLog ("MERGED 🧑🏼‍🤝‍🧑🏼 new #" ++ String.fromInt new ++ " to original #" ++ String.fromInt original) "\n"

equiv_in_register : NodeId -> MADFARecord -> Maybe NodeId
equiv_in_register q madfa =
  -- They are equivalent if:
  -- (1) either both are final, or both are not final; and
  -- (2) both have exactly the same outgoing transitions (i.e. transition & destination state matches)
  let
    q_key = toRegisterValue q madfa |> Debug.log ("Generated key for node #" ++ String.fromInt q)
  in
    Dict.get q_key madfa.register |> Debug.log ("Equivalent for cloned/queued node #" ++ String.fromInt q)

replace_or_register : NodeId -> MADFARecord -> MADFARecord
replace_or_register current madfa =
  case equiv_in_register current madfa of
    Just existing ->
      merge current existing madfa
    Nothing ->
      { madfa
      | register = Dict.insert (toRegisterValue current madfa) current madfa.register
      }

-- the same as `addstring` algorithm from the paper (Figure 1)
addWordString : List (Transition, Bool) -> MADFARecord -> MADFARecord
addWordString w existing =
  let
    original_q0 = existing.root |> Debug.log "Existing root"
    (new_q0, phase_1_setup) = clone original_q0 False existing
    phase_1 = -- here, we do the initial insertion of w
      List.foldl
        (\(transition, isFinal) (m_branch, q_last, madfa) ->
          let
            destination =
              follow_transition transition m_branch madfa -- attempt to follow a transition from the last-seen state in M
              |> Debug.log ("Trying to follow transition " ++ String.fromChar transition ++ " from NodeId " ++ Debug.toString m_branch ++ ", result is")
            (q, madfa_with_q) =
              destination
              |> Maybe.map (\exists_in_M -> -- there is something in M, so we will clone it.
                clone exists_in_M isFinal madfa
              )
              |> Maybe.withDefaultLazy (\() -> -- there is no compatible path, so we must place this onto the queue
                queue isFinal madfa
              )
            with_w_transition =
              addTransition q_last transition q madfa_with_q
          in
            ( destination, q, with_w_transition )
        )
        (Just existing.root, new_q0, phase_1_setup)
        w
      |> \(_, _, madfa) ->
          madfa
          |> madfaLog "🤖 AFTER PHASE 1 (clone & queue)" "🎯 BEGINNING PHASE 2 (handle reachability)"
    phase_2 = -- here, we remove unreachable states
      remove_unreachable original_q0 (List.map Tuple.first w) phase_1
    -- CHECK ALGORITHM: why is there _another_ if-statement after, to handle a node at |w|+1?  Can unreachable go that far?
    -- Now, after this I need to replace the original_q0 with the cloned one.  The tough thing is that… I haven't kept track of it!
    -- However, I WILL find it as the smallest number in the list of cloned/queued states.
    clones_and_queued =
      Set.toList phase_2.cloned ++ Set.toList phase_2.queue
    phase_2_end =
      (case List.minimum clones_and_queued of
        Nothing ->
          phase_2 -- this can only happen if NOTHING has been cloned OR queued
        Just min ->
          { phase_2 | root = min |> Debug.log "Setting new root node to" }
      )|> madfaLog "🤖 AFTER PHASE 2 (handle reachability)" "🎯 BEGINNING PHASE 3 (replace-or-register)"
    phase_3 =
      List.foldr
        (\w_state madfa -> replace_or_register w_state madfa)
        phase_2_end
        (clones_and_queued |> Debug.log "Cloned/Queued states")
  in
    -- finally, get rid of the clones and the queued; we don't need them.
    { phase_3
    | cloned = Set.empty
    , queue = Set.empty
    }
    |> madfaLog "🤖 AFTER PHASE 3 (replace-or-register)" "👍🏽 COMPLETE!\n\n***\n\n"

wordToMADFA : String -> MADFARecord -> MADFARecord
wordToMADFA s existing =
  wordToTransitions s
  |> \wordGraph -> addWordString wordGraph existing

seedMADFARecord : MADFARecord
seedMADFARecord =
  let
    initialNode =
      NodeContext (Node 0 False) IntDict.empty IntDict.empty
  in
    MADFARecord (Graph.insert initialNode Graph.empty) 0 0 Set.empty Set.empty Dict.empty


toMADFA : List String -> Maybe MADFARecord
toMADFA words =
  foldlSpecial wordToMADFA wordToMADFA seedMADFARecord words





transitionToString : Transition -> String
transitionToString =
  String.fromChar

graphToString : FAGraph -> String
graphToString graph =
  Graph.toString
    (\node -> Just <| if node then "⭐" else "🟢")
    (Just << transitionToString)
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