module Automata.DFA exposing (..)
import IntDict exposing (IntDict)
import Set exposing (Set)
import AutoSet
import List.Extra as List
import Dict exposing (Dict)
import AutoDict
import Automata.Data exposing (..)
import Graph exposing (NodeContext, NodeId, Edge)
import Dict.Extra
import Maybe.Extra
import Json.Encode as E
import Json.Decode as D
import Uuid
import Automata.Debugging exposing (printAutomatonGraph)
import Automata.Debugging exposing (debugAutomatonGraph)
import Uuid exposing (Uuid)
import Automata.Debugging as Debugging
import Automata.Debugging exposing (printNodeContext)
import Automata.Debugging exposing (println)

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.

connectionToGraph : Uuid -> Connection -> AutomatonGraph
connectionToGraph uuid conn =
  Automata.Data.empty uuid -- STUB!!

graphReferenceSet : Connection -> AutoSet.Set String Uuid
graphReferenceSet conn =
  AutoSet.empty Uuid.toString -- STUB!!

traverseConnectionGraph : List Char -> AutoSet.Set String Uuid -> AutomatonGraph -> Maybe (AutoSet.Set String Uuid, List Char)
traverseConnectionGraph data allowed g =
  Nothing -- STUB!!

oneTransition : AutomatonGraph -> ExecutionState -> ExecutionState
oneTransition g executionState =
  -- let
  --   transitionWithData : ExecutionData -> ExecutionState
  --   transitionWithData data =
  --     Graph.get data.currentNode g.graph
  --     |> Maybe.map
  --       (\ctx ->
  --         case data.remainingData of
  --           [] ->
  --             executionState -- we're done! Can't go any further!
  --           _ ->
  --             let
  --               outgoingAsGraphs =
  --                 IntDict.toList ctx.outgoing
  --                 |> List.map
  --                   (\(k, conn) ->
  --                     -- Create a graph from the things in this connection
  --                     { outgoing = k
  --                     , transitionGraph = connectionToGraph g.graphIdentifier conn
  --                     , allowed = graphReferenceSet conn
  --                     , connection = conn
  --                     }
  --                   )
  --               chosenOutgoingData =
  --                 outgoingAsGraphs
  --                 |> List.findMap
  --                   (\{outgoing, transitionGraph, allowed, connection} ->
  --                     -- traverseConnectionGraph only generates a Just value IF it accepts.
  --                     -- This is the same as how a character is used to "accept".
  --                     -- But that has no effect on whether the transition in the link
  --                     -- is actually terminal or not.
  --                     traverseConnectionGraph data.remainingData allowed transitionGraph
  --                     |> Maybe.map
  --                       (\(finalSet, consumed) ->
  --                         { dest = outgoing -- I have a Just value, so YES, I select THIS outgoing!
  --                         , matching_transitions =
  --                           let
  --                             final =
  --                               AutoSet.filter (isFinalInContext overlayContext) connection
  --                             present =
  --                               AutoSet.filter (\{tags} -> AutoSet.size (AutoSet.intersect finalSet tags) > 0) final
  --                           in
  --                             present
  --                         , consumed = consumed
  --                         , remaining = List.drop (List.length consumed) data.remainingData
  --                         }
  --                       )
  --                   )
  --                 |> Maybe.map
  --                   (\{dest, matching_transitions, consumed, remaining} ->
  --                     Accepted
  --                       { data
  --                       | transitionsTaken = (data.currentNode, matching_transitions, consumed)::data.transitionsTaken
  --                       , remainingData = remaining
  --                       , currentNode = dest
  --                       }
  --                   )
  --             in
  --               chosenOutgoingData
  --               |> Maybe.withDefault (NoPossibleTransition data)
  --       )
  --     |> Maybe.withDefault (RequestedNodeDoesNotExist data)
  -- in
  --   case executionState of
  --     Accepted d ->
  --       transitionWithData d
  --     Rejected d ->
  --       transitionWithData d
  --     RequestedNodeDoesNotExist _ ->
  --       executionState
  --     NoPossibleTransition _ ->
        executionState

step : AutomatonGraph -> ExecutionResult -> ExecutionResult
step g executionResult =
  case executionResult {- |> Debug.log "Step with" -} of
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
          case d.transitions of
            [] ->
              -- Finality is stored on the TRANSITION, not the STATE.
              -- Therefore, I must take at least one TRANSITION to
              -- accept.  Otherwise—by default—I will reject.
              EndOfComputation (Rejected d)
            {matching}::_ ->
              let
                final = AutoSet.filter (isFinalInContext d.overlayContext) matching
              in
                if AutoSet.size final > 0 then
                  EndOfComputation (Accepted d)
                else
                  EndOfComputation (Rejected d)
    EndOfInput _ ->
      executionResult
    EndOfComputation _ ->
      executionResult
    InternalError ->
      executionResult

run : AutomatonGraph -> ExecutionResult -> ExecutionResult
run g r =
  let
    execute result =
      case step g result of
        (CanContinue _) as okay ->
          execute okay
        x ->
          x
  in
    execute r

stepThroughInitial : String -> AutomatonGraph -> ExecutionResult
stepThroughInitial s g =
  let
    upcomingAcceptConditions =
      String.toList
  in
  step g
    (CanContinue <| Rejected <|
      ExecutionData [] (upcomingAcceptConditions s) g.root (makeInitialContext g)
    )

extend : Phase1Purpose -> DFARecord a -> DFARecord a -> ExtDFA
extend purpose w_dfa_orig dfa = -- parameters: the w_dfa and the dfa
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
          |> debugDFA_ "w_dfa_orig"
      }
      |> debugExtDFA_ "extDFA (initial)"
      |> debugExtDFA_ "extDFA (post-clone-and-queue)"
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

w_forward_transitions : ExtDFA -> ForwardTree
w_forward_transitions extDFA =
  let
    {-
      I need to track by (src, via, dest).
      I cannot just track by (via, dest).
      Consider: (0, a, 1) (1, b, 2) (2, a, 1) [1]
      Suddenly, that last transition doesn't get counted…
    -}
    helper : Maybe NodeId -> NodeId -> AutoSet.Set (Int, String, Int) (NodeId, AcceptVia, NodeId) -> ForwardTree
    helper src current seen =
      case IntDict.get current extDFA.transition_function of
        Nothing -> PathEnd
        Just dict ->
          let
            filtered =
              AutoDict.filter
                (\via dest ->
                  not (AutoSet.member ({- Debug.log "testing" -} (current, via, dest)) ({- Debug.log "seen" -} seen)) &&
                  current /= dest --&&
                  --List.member dest extDFA.queue_or_clone
                )
                dict
              -- |> Debug.log "filtered"
            children =
              AutoDict.map
                (\via state ->
                  -- Debug.log "Examining" (current, via, state) |> \_ ->
                  helper (Just current) state (AutoSet.insert (current, via, state) seen)
                )
                filtered
          in
            if AutoDict.isEmpty children then
              PathEnd
            else
              ForwardNode children
  in
    helper Nothing extDFA.clone_start (AutoSet.empty (\(a,b,c) -> ( a, acceptConditionToString b, c )))
    |> debugForwardTree "[w_forward_transitions] result"

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

type Phase1Purpose
  = Add_Word
  | Remove_Word

clone_or_queue : Phase1Purpose -> NodeId -> NodeId -> ExtDFA -> ExtDFA
clone_or_queue purpose q_m q_w extDFA =
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
                (\k v w acc -> AutoDict.insert k (v |> Debug.log "BOTH; taking left") acc)
                -- right only
                (\k v acc -> AutoDict.insert k (v |> Debug.log "right only") acc)
                (AutoDict.empty acceptConditionToString)
                a b
              )
              extDFA.transition_function
    , finals =
        -- now, here is the trick: if I am ADDING q_w, then I add q_w to
        -- the finals as appropriate.  However, if I am REMOVING
        -- q_w, then none of these states becomes final; we don't stop anywhere
        -- that q_w stops.
        case purpose of
          Add_Word ->
            if Set.member q_m extDFA.finals then
              Set.insert q_w extDFA.finals
            else
              extDFA.finals
          Remove_Word ->
            extDFA.finals
   }

clone_or_queue_many : Phase1Purpose -> NodeId -> NodeId -> ForwardTree -> ExtDFA -> ExtDFA
clone_or_queue_many purpose q_m q_w tree extDFA =
  case tree of
    PathEnd ->
      -- we're at the end of the transitions.
      clone_or_queue purpose q_m q_w extDFA
      |> debugExtDFA_ ("[clone_or_queue_many] end of path, (q_m = #" ++ String.fromInt q_m ++ ", q_w = #" ++ String.fromInt q_w ++ ")")
    ForwardNode dict ->
      AutoDict.foldl
        (\ch subtree acc ->
          case (delta q_m ch acc, delta q_w ch acc) |> Debug.log ("(δ(q_m, " ++ printableAcceptCondition ch ++ "), δ(q_w, " ++ printableAcceptCondition ch ++ "))") of
            (_, Nothing) ->
              Debug.log "🚨 ERROR!! How can I fail to get a `w` transition via known `w`-transitions??" () |> \_ ->
              acc
            (Nothing, Just _) ->
              case purpose of
                Add_Word ->
                  -- The q_m ends here, but q_w carries on. ∴ the remaining q_w must be "queued" nodes.
                  clone_or_queue purpose q_m q_w acc
                  |> debugExtDFA_ ("[clone_or_queue_many] queued, (q_m = #" ++ String.fromInt q_m ++ ", q_w = #" ++ String.fromInt q_w ++ ")")
                Remove_Word ->
                  -- we don't want to or need to add q_w-specific nodes.
                  acc
            (Just m_node, Just w_node) ->
              clone_or_queue_many purpose m_node w_node subtree (clone_or_queue purpose q_m q_w acc)
              |> debugExtDFA_ ("[clone_or_queue_many] cloned subtree starting at m = #" ++ String.fromInt m_node ++ ", w = #" ++ String.fromInt w_node)
        )
        extDFA
        dict

state_key : { q_m : Maybe NodeId, q_w : Maybe NodeId } -> String
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
      |> Maybe.Extra.withDefaultLazy
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
        |> Maybe.Extra.withDefaultLazy
          (\() ->
            Entity 0 0 0 0 id NoEffect
            |> Debug.log "🚨 ERROR!! How can I fail to get a known state from `w_orig_dfa`??"
          )
  in
  case node_stack of
    [] -> extDFA
    ({ q_m, q_w } as head) :: rest ->
      if AutoSet.member head handled then
        -- we have seen & dealt with this one before.
        -- HOWEVER, it now occurs at a later point in the BFS.
        -- So, we may need to check it twice for right-language equivalence.
        let
          head_id = head_id_for head
          updated_queue_or_clone =
            if Set.member head_id extDFA.register then
              extDFA.queue_or_clone -- nothing to do
            else
              head_id_for head :: extDFA.queue_or_clone
        in
          build_out rest handled mapping
            { extDFA | queue_or_clone = updated_queue_or_clone }
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
                |> Debug.log ("[build_out (Qm=" ++ String.fromInt q__m ++ ", Qw=" ++ String.fromInt q__w ++ ")] Transitions out")
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
                List.filter
                  (\v -> not <| AutoSet.member v handled)
                  (AutoDict.values resulting_combination_states)
              new_node_stack =
                -- depth-first
                resulting_combination_states_excluding_handled ++ rest
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
                        |> Maybe.Extra.withDefaultLazy
                          (\() ->
                            Debug.log "🚨 ERROR!! How can I fail to get a known-good via-mapping??"
                              { q_w = Nothing, q_m = Nothing }
                          )
                        |>  (\rawKey ->
                                AutoDict.get rawKey new_mapping
                                |> Maybe.Extra.withDefaultLazy
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
                |> debugExtDFA_ "After build_out"
                )
          (Just q__m, Nothing ) ->
            -- this state only exists in q_m. So I don't need to worry about anything from q_w.
            -- q_m should already have the necessary transitions for this; so I can ignore.
            println ("[build_out (Qm=" ++ String.fromInt q__m ++ ", Qw=⊥)] State only exists in Qm, so ignoring it; existing transitions remain.")
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
                |> Debug.log ("[build_out (Qm=⊥, Qw=" ++ String.fromInt q__w ++ ")] Transitions out")
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
                List.filter
                  (\v -> not <| AutoSet.member v handled)
                  (AutoDict.values resulting_combination_states)
              new_node_stack =
                resulting_combination_states_excluding_handled ++ rest
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
                        |> Maybe.Extra.withDefaultLazy
                          (\() ->
                            Debug.log "🚨 ERROR!! How can I fail to get a known-good via-mapping??"
                              { q_w = Nothing, q_m = Nothing }
                          )
                        |>  (\rawKey ->
                                AutoDict.get rawKey new_mapping
                                |> Maybe.Extra.withDefaultLazy
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
                |> debugExtDFA_ "After build_out"
                )
          ( Nothing, Nothing ) ->
            -- Unequivocal absorption state.  Nothing can possibly lead from here.
            println "[build_out (⊥, ⊥)] Absorption state"
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

remove_unreachable : ForwardTree -> ExtDFA -> ExtDFA
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
    (_, without_external_edges) =
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
          (old_w_path_nodes |> Debug.log "checking in order") -- must check in-order
          (Tuple.mapBoth Set.fromList Set.fromList initial_partition)
        |> Debug.log "(to_keep, to_remove)"
    -- Step 4
    purge : NodeId -> ExtDFA -> ExtDFA
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

replace_or_register : ExtDFA -> ExtDFA
replace_or_register extDFA =
  let
    equiv : NodeId -> NodeId -> Bool
    equiv p =
      let
        p_outgoing =
          IntDict.get p extDFA.transition_function
          |> Maybe.map AutoDict.toList
          |> Debug.log ("'p'-outgoing for " ++ String.fromInt p ++ " is")
      in
        \q ->
          if xor (Set.member p extDFA.finals) (Set.member q extDFA.finals) then
            -- MUST have the same finality status.
            Debug.log ("Checking against 'q'-outgoing for " ++ String.fromInt q ++ "; not equivalent; finality differs.") () |> \_ ->
            False
          else
            let
              q_outgoing =
                IntDict.get q extDFA.transition_function
                |> Maybe.map AutoDict.toList
                |> Debug.log ("Checking against 'q'-outgoing for " ++ String.fromInt q)
            in
              case ( p_outgoing, q_outgoing ) of
                ( Just _, Nothing ) -> False
                ( Nothing, Just _ ) -> False
                ( Nothing, Nothing ) -> True
                ( Just a, Just b ) ->
                  Debug.log ("    Result of deep comparison") <|
                  a == b
    redirectInto : NodeId -> NodeId -> IntDict (AutoDict.Dict String AcceptVia NodeId)
    redirectInto target source =
      -- redirect everything that goes to source, into target
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
  in
  case extDFA.queue_or_clone |> Debug.log "queue_or_clone" of
    h::t ->
      let
        equiv_to_cloned_or_queued = equiv h
      in
        case Set.toList extDFA.register |> List.find equiv_to_cloned_or_queued of
          Just found_equivalent ->
            Debug.log ("Registering " ++ String.fromInt h ++ " as equivalent to " ++ String.fromInt found_equivalent) () |> \_ ->
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
            Debug.log ("No equivalent found for " ++ String.fromInt h) () |> \_ ->
            replace_or_register
              { extDFA
                | register = Set.insert h extDFA.register
                , queue_or_clone = t
              }
    [] ->
      extDFA

union : DFARecord a -> DFARecord a -> DFARecord {}
union w_dfa_orig m_dfa =
  extend Add_Word w_dfa_orig m_dfa
  |> debugExtDFA_ "[union] extDFA creation from merged w_dfa + dfa"
  |> phase_1 -- Add_Word
  |> debugExtDFA_ "[union] End of Phase 1 (clone-and-queue)"
  |> (\extdfa -> remove_unreachable (w_forward_transitions extdfa) extdfa)
  |> (\dfa -> { dfa | start = dfa.clone_start })
  |> debugExtDFA_ "[union] End of Phase 2 (remove-unreachable + switch-start)"
  |> replace_or_register
  |> debugExtDFA_ "[union] End of Phase 3 (replace-or-register)"
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
  Automata.Debugging.debugLog s Automata.Debugging.printFan fan

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
            |> debugFan_ "[minimisation_merge] merged incoming"
          updatedOutgoing =
            IntDict.uniteWith
              (\_ -> AutoSet.union)
              (redirectFan other head headNode.outgoing {- |> debugFan_ "[minimisation_merge] headNode.outgoing (redir)" -})
              (redirectFan other head otherNode.outgoing {- |> debugFan_ "[minimisation_merge] otherNode.outgoing (redir)" -})
            |> debugFan_ "[minimisation_merge] merged outgoing"
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
    |> Automata.Debugging.debugAutomatonGraph ("[minimisation_merge] Post merge of #" ++ String.fromInt head ++ " and #" ++ String.fromInt other)

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
    overlayContext = makeInitialContext g_ 
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
                else if AutoSet.foldl (\t acc -> acc || isFinalInContext overlayContext t) False conn then
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
      |> Debug.log "[minimiseNodes] Terminal nodes (i.e. starting points)"
    fanOutEquals : NodeContext Entity Connection -> NodeContext Entity Connection -> Bool
    fanOutEquals a b =
      let
        redirected o id =
          IntDict.toList o
          --|> Debug.log ("[minimiseNodes→fanOutEquals] outgoing of #" ++ String.fromInt id)
      in
      redirected a.outgoing a.node.id == redirected b.outgoing b.node.id
      |> Debug.log ("[minimiseNodes→fanOutEquals] Are #" ++ String.fromInt a.node.id ++ " and #" ++ String.fromInt b.node.id ++ " equal?")
    classify : NodeContext Entity Connection -> AutomatonGraph -> Maybe (AutomatonGraph)
    classify terminal g =
      -- classify the terminal node into one of the four classes
      if IntDict.isEmpty terminal.outgoing then
        -- case T1.  We will deal with this right at the end, during
        -- finalisation after ALL user changes have been made for this
        -- round of changes.
        Debug.log ("[minimiseNodes] 🕳️ Terminal #" ++ String.fromInt terminal.node.id ++ " has no fan-out. I won't finalise it now.") () |> \_ ->
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
            |> Debug.log "[minimiseNodes] After merging T1 nodes"
        in
          case emptyOutgoing of
            [] ->
              Debug.log "[finaliseEndNodes] There are no nodes to finalise." |> \_ ->
              Nothing
            [_] ->
              Debug.log "[finaliseEndNodes] There is only one terminal node; therefore, nothing to finalise." |> \_ ->
              Nothing
            term::rest ->
              List.foldl
                (minimisation_merge term)
                g
                rest
              |> debugAutomatonGraph "[finaliseEndNodes] Post-finalisation"
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
          Debug.log ("[minimiseNodes] Terminal #" ++ String.fromInt terminal.node.id ++ " has a fanout.  Checking targets to see if it is extended by another.") () |> \_ ->
          case List.find (fanOutEquals terminal) targets of
            Just equivalent ->
              -- Case T2, sub-case 1
              Automata.Debugging.println ("[minimiseNodes] 🕳️ Node #" ++ String.fromInt terminal.node.id ++ " is extended by #" ++ String.fromInt equivalent.node.id)
              Just (minimisation_merge terminal.node.id equivalent.node.id g)
            Nothing ->
              Debug.log ("[minimiseNodes] No suitable targets found; #" ++ String.fromInt terminal.node.id ++ " is not extended by any node.  Checking sources to see if it extends another.") () |> \_ ->
              case List.find (fanOutEquals terminal) sources of
                Just equivalent ->
                  -- Case T2, sub-case 2
                  Automata.Debugging.println ("[minimiseNodes] 🕳️ Node #" ++ String.fromInt terminal.node.id ++ " is an extension of #" ++ String.fromInt equivalent.node.id)
                  Just (minimisation_merge terminal.node.id equivalent.node.id g)
                Nothing ->
                  Debug.log ("[minimiseNodes] #" ++ String.fromInt terminal.node.id ++ " neither extends nor is extended.") () |> \_ ->
                  case targets of
                    m::_ ->
                      Debug.log "[minimiseNodes] Checking for common sources of target-node" m.node.id |> \_ ->
                      IntDict.get terminal.node.id m.incoming
                      |> Maybe.andThen
                        (\chosenConnection ->
                          Automata.Debugging.debugLog "[minimiseNodes] connection to follow back is" connectionToString chosenConnection |> \_ ->
                          IntDict.toList m.incoming
                          |> List.filterMap
                            (\(s, conn) ->
                              if s /= terminal.node.id && conn == chosenConnection then
                                Graph.get s g.graph
                                |> Maybe.map (Debugging.debugLog ("[minimiseNodes] candidate node (excluding #" ++ String.fromInt terminal.node.id ++ ")") (.node >> .id))
                              else
                                Nothing
                            )
                          |> List.find (fanOutEquals terminal)
                          |> Automata.Debugging.debugLog "[minimiseNodes] selected mergeable candidate"
                            (Maybe.map printNodeContext >> Maybe.withDefault "NONE - there is no fan-in; therefore, no merge candidate; therefore, this merge will not take place).")
                          |> Maybe.map
                            (\equivalent ->
                                Automata.Debugging.println ("[minimiseNodes] Node #" ++ String.fromInt terminal.node.id ++ " can be merged with node #" ++ String.fromInt equivalent.node.id)
                                minimisation_merge terminal.node.id equivalent.node.id g
                            )
                        )
                    [] ->
                      Debug.log "[minimiseNodes] No suitable targets found." () |> \_ ->
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
      (g_ |> Automata.Debugging.debugAutomatonGraph "[minimiseNodes] Initial graph" )
    |> Automata.Debugging.debugAutomatonGraph "[minimiseNodes] Final graph"


toAutomatonGraph : Uuid.Uuid -> DFARecord a -> AutomatonGraph
toAutomatonGraph uuid dfa =
  let
    stateList = IntDict.toList dfa.states |> List.reverse -- |> Debug.log "[toAutomatonGraph] State-list"
    graph =
      Graph.fromNodesAndEdges
        (stateList |> List.map (\(id, label) -> Graph.Node id label))
        (IntDict.toList (dfa {- |> debugDFA_ "[toAutomatonGraph] DFA as received" -}).transition_function
        |> List.foldl
          (\(from, dict) state ->
            AutoDict.toList dict
            |> List.foldl
              (\(transition, to) state_ ->
                let
                  t =
                    { finality = AutoDict.singleton Uuid.toString uuid (Set.member to dfa.finals)
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
        Automata.Data.empty uuid
        -- |> debugAutomatonGraph "[toAutomatonGraph] Graph, since DFA was empty"
      h::_ ->
        { graph = graph
        , graphIdentifier = uuid
        , root = dfa.start -- |> Debug.log "[toGraph] root"
        }
        -- |> debugAutomatonGraph "[toAutomatonGraph] Graph, as converted from DFA"
        |> minimiseNodesByCombiningTransitions
        |> debugAutomatonGraph "[toAutomatonGraph] Graph, final output"

renumberAutomatonGraph : AutomatonGraph -> AutomatonGraph
renumberAutomatonGraph g =
  let
    overlayContext = makeInitialContext g
    fanMapper =
      IntDict.toList
      >> List.map
        (\(k, conn) ->
          ( k
          , AutoSet.toList conn
            |> List.map
              (\t ->
                ( acceptConditionToString t.via
                , if isFinalInContext overlayContext t then 1 else 0
                )
              )
          )
        )
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
    , graphIdentifier = g.graphIdentifier
    , root = get g.root
    }
    -- |> debugAutomatonGraph "[renumberAutomatonGraph] result"

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
    overlayContext = makeInitialContext g
        -- Helper to classify a transition as terminal or non-terminal
    isTerminal : Transition -> Bool
    isTerminal = isFinalInContext overlayContext

    isNonTerminal : Transition -> Bool
    isNonTerminal = not << (isFinalInContext overlayContext)

    -- Find the next unused node id
    nextId : Int
    nextId =
      maxId g + 1

    -- For each node, determine if it needs to be split
    nodesToSplit =
      Graph.nodeIds g.graph
      |> List.filterMap (\id -> Graph.get id g.graph |> Debug.log ("[splitTerminalAndNonTerminal] Node for #" ++ String.fromInt id))
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
            |> Debug.log ("[splitTerminalAndNonTerminal] Incoming transitions to " ++ String.fromInt node.node.id)
          hasTerminal = List.any isTerminal allIncoming
          hasNonTerminal = List.any isNonTerminal allIncoming
        in
          hasTerminal && hasNonTerminal
      )
      |> debugLog_ "[splitTerminalAndNonTerminal] nodes to split" (Debug.toString << List.map (.node >> .id))

    -- Build a mapping from node id to new split node id (for non-terminal transitions)
    splitMap : Dict NodeId NodeId
    splitMap =
      List.indexedMap (\i node -> (node.node.id, nextId + i)) nodesToSplit
      |> Dict.fromList

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
    { graph = newGraph
    , graphIdentifier = g.graphIdentifier
    , root = g.root
    }
    |> Automata.Debugging.debugAutomatonGraph "[splitTerminalAndNonTerminal] after split"


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

    overlayContext = makeInitialContext g
    populateColumnData : IntDict Connection -> Column -> Column
    populateColumnData outgoing columnDict =
      IntDict.foldl
        (\destId conn columnDict_ ->
          AutoSet.foldl
            (\t d ->
              case AutoDict.get t.via d of
                Nothing ->
                  Debug.log ("Inserting first " ++ acceptConditionToString t.via ++ "-transition (" ++ (if not (isFinalInContext overlayContext t) then "non-" else "") ++ "Final), to #" ++ String.fromInt destId) () |> \_ ->
                  Graph.get destId g.graph
                  |> Maybe.map
                    (\{node} ->
                      AutoDict.insert t.via ((if (isFinalInContext overlayContext t) then 1 else 0, [destId]), node.label) d
                    )
                  |> Maybe.Extra.withDefaultLazy (\() -> Debug.todo ("BGFOEK " ++ String.fromInt destId))
                Just ((f2, list), v) ->
                  Debug.log ("Inserting another " ++ acceptConditionToString t.via ++ "-transition (" ++ (if not (isFinalInContext overlayContext t) then "non-" else "") ++ "Final), to #" ++ String.fromInt destId) () |> \_ ->
                  -- if any of the transitions is final, then the created state will be final
                  AutoDict.insert t.via ((max (if (isFinalInContext overlayContext t) then 1 else 0) f2, normalizeSet (destId::list)), v) d
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
      |> List.any (\conn -> AutoSet.filter (isFinalInContext overlayContext) conn |> (not << AutoSet.isEmpty))
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
      |> debugTable_ "[nfaToDFA] Initial table"

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
      |> debugTable_ "[nfaToDFA] Complete table"

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
                |> debugLog_ "[nfaToDFA] source-sets to merge" (Set.map stateIdentifierToString >> Debug.toString)
              without_merged =
                Set.foldl Dict.remove table sourceSets_to_merge
                |> debugTable_ "[nfaToDFA] After removals"
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
        |> debugTable_ "[nfaToDFA] Identical cell values have been merged"

    -- Step 2: Remove unreachable states (keep only those reachable from root)
    removeUnreachableStates : Table -> Table
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
      |> debugTable_ "[nfaToDFA] Unreachable cell values have been removed"

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
        |> debugTable_ "[nfaToDFA] Rows have been renamed"

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
                    ( Dict.Extra.find (\_ ((_, v), _) -> v == idList)
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
                                  Transition (AutoDict.singleton Uuid.toString g.graphIdentifier (f == 1)) acceptCondition
                            Just conn ->
                              Just <|
                                AutoSet.insert
                                  (Transition (AutoDict.singleton Uuid.toString g.graphIdentifier (f == 1)) acceptCondition)
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
    { graph = newGraph
    , graphIdentifier = g.graphIdentifier
    , root = g.root
    }
    |> Automata.Debugging.debugAutomatonGraph "[nfaToDFA] Resulting graph"

fromAutomatonGraphHelper : AutomatonGraph -> DFARecord {}
fromAutomatonGraphHelper g =
  -- called AFTER splitting non-terminal/terminal, and AFTER NFA→DFA conversion.
  let
    overlayContext = makeInitialContext g
  in
  { states =
      Graph.nodes g.graph
      |> List.map (\node -> ( node.id, node.label) )
      |> IntDict.fromList
  , start = g.root
  , finals =
      Graph.fold
        (\ctx finals ->
          if isTerminalNode overlayContext ctx then 
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
    Automata.Debugging.debugAutomatonGraph "[fromAutomatonGraph] Graph as received" >>
    splitTerminalAndNonTerminal
    >> Automata.Debugging.debugAutomatonGraph "[fromAutomatonGraph] Graph after splitting the joined terminal+non-terminal nodes"
    >> nfaToDFA
    >> Automata.Debugging.debugAutomatonGraph "[fromAutomatonGraph] Graph NFA→DFA conversion"
    >> fromAutomatonGraphHelper
    >> debugDFA_ "[fromAutomatonGraph] Graph→DFA"

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
encodeTransition {via, finality} =
  case via of
    ViaCharacter c ->
      E.object
        [ ("c", E.string <| String.fromChar c)
        , ("_", encodeAutoDict Uuid.toString E.bool finality)
        ]
    ViaGraphReference uuid ->
      E.object
        [ ("ref", E.string <| Uuid.toString uuid)
        , ("_", encodeAutoDict Uuid.toString E.bool finality)
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
    , ("uuid", Uuid.encode g.graphIdentifier)
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
        (D.field "_" (decodeAutoDict Uuid.toString Uuid.fromString D.bool))
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
        (D.field "_" (decodeAutoDict Uuid.toString Uuid.fromString D.bool))
        (D.field "ref" <| D.map ViaGraphReference Uuid.decoder)
    ]

decodeEdge : Uuid -> D.Decoder (Edge Connection)
decodeEdge uuid =
  D.map3
    (\f t l -> Edge f t (AutoSet.fromList transitionToString l))
    (D.field "src" <| D.int)
    (D.field "dst" <| D.int)
    (D.field "via" <| D.list decodeTransition)

decodeAutomatonGraph : D.Decoder AutomatonGraph
decodeAutomatonGraph =
  D.field "uuid" Uuid.decoder
  |> D.andThen
    (\uuid ->
      D.map3 (\n e -> AutomatonGraph (Graph.fromNodesAndEdges n e) uuid)
        (D.field "nodes" <| D.list decodeNode)
        (D.field "edges" <| D.list <| decodeEdge uuid)
        (D.field "root" <| D.int)
    )

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

debugLog : (a -> String) -> a -> a
debugLog f a =
  Debug.log (f a) () |> \_ -> a

debugLog_ : String -> (a -> String) -> a -> a
debugLog_ s f a =
  Debug.log (s ++ ": " ++ f a) () |> \_ -> a