module Automata.DFA exposing (..)
import IntDict exposing (IntDict)
import Set exposing (Set)
import List.Extra as List
import Dict exposing (Dict)
import Automata.Data exposing (..)
import Graph exposing (Graph, NodeContext, Node, NodeId, Edge)
import Automata.Debugging
import Maybe.Extra
import Automata.MADFA exposing (redirect)
import Dict.Extra

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

type alias DFARecord a =
  { a |
    states : IntDict () -- the () is the label.
  , transition_function: IntDict (Dict Char NodeId) -- NodeId × Char → NodeId
  , start : NodeId
  , finals : Set NodeId
  }

type alias ExtDFA =
  { states : IntDict ()
  , transition_function: IntDict (Dict Char NodeId)
  , start : NodeId
  , finals : Set NodeId
  , register : Set NodeId
  , clone_start : NodeId
  , queue_or_clone : List NodeId
  , unusedId : NodeId
  }

type alias CloneResult =
  { extDFA : ExtDFA
  , q_w : Maybe NodeId -- Nothing, if we've reached the end.
  , q_m : Maybe NodeId -- Nothing, if we reached a break in the transitions.
  , seen_qw : Set NodeId -- the states of q_w we've seen thus far.
  }


extend : DFARecord a -> DFARecord a -> ExtDFA
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
  in
    extDFA

retract : ExtDFA -> DFARecord {}
retract extDFA =
  { states = extDFA.states
  , transition_function = extDFA.transition_function
  , start = extDFA.start
  , finals = extDFA.finals
  }

create : List (NodeId, ()) -> List (NodeId, Char, NodeId) -> NodeId -> Set NodeId -> DFARecord {}
create nodes edges start finals =
  let
    states = IntDict.fromList nodes
  in
  { states = states
  , transition_function =
      List.foldl
        (\(from, transition, to) state ->
            if IntDict.member from states && IntDict.member to states then
              IntDict.update from
                (\possibly ->
                  case possibly of
                    Nothing ->
                      -- first entry, that's fun.
                      Just (Dict.singleton transition to)
                    Just dict ->
                      case Dict.get transition dict of
                        Nothing ->
                          -- first transition leading from here, okay.
                          Just (Dict.insert transition to dict)
                        Just _ ->
                          -- this would be deterministic.  Instead, overwrite.
                          Debug.log ("[create] Overwriting transition " ++ String.fromChar transition ++ " from " ++ String.fromInt from ++ " to " ++ String.fromInt to ++ ".") () |> \_ ->
                          Just (Dict.insert transition to dict)
                )
                state
            else
              Debug.log ("[create] Skipping edge " ++ String.fromInt from ++ " → " ++ String.fromInt to ++ ", because of non-existence.")
                (IntDict.member from states, IntDict.member to states) |> \_ ->
              state
        )
        IntDict.empty
        edges
  , start = start
  , finals = finals
  }

{-| Create a DFA that accepts exactly one string. -}
string_to_dfa : String -> Maybe (DFARecord {})
string_to_dfa string =
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

w_forward_transitions : ExtDFA -> List Char
w_forward_transitions extDFA =
  let
    helper : NodeId -> Set NodeId -> List Char -> List Char
    helper current seen acc =
      IntDict.get current extDFA.transition_function
      |> Maybe.map
        (\dict ->
          Dict.filter
            (\_ state -> not (Set.member state seen) && current /= state && state >= extDFA.clone_start)
            dict
          |> Dict.toList
          |> (\l ->
                case l of
                  [] -> acc
                  [(transition, state)] -> helper state (Set.insert state seen) (transition :: acc)
                  _ -> [] -- I SHOULD NEVER GET HERE!!!
            )
        )
      |> Maybe.withDefault acc
  in
    helper extDFA.clone_start Set.empty []
    |> List.reverse
    |> Debug.log "w_forward_transitions"

delta : NodeId -> Char -> DFARecord a -> Maybe NodeId
delta q x dfa =
  IntDict.get q dfa.transition_function
  |> Maybe.andThen (Dict.get x)

-- delta_star : NodeId -> List Char -> DFARecord a -> Maybe NodeId
-- delta_star q xs dfa =
--   List.foldl (\x -> Maybe.andThen (\q_ -> delta q_ x dfa)) (Just q) xs

transitions_of : NodeId -> DFARecord a -> Dict Char NodeId
transitions_of q dfa =
  IntDict.get q dfa.transition_function
  |> Maybe.withDefault Dict.empty

phase_1 : ExtDFA -> ExtDFA
phase_1 extDFA_orig =
  -- here, we just need to clone all the corresponding "clone" transitions.
  let
    -- well, now that we have them… let's follow along and attach the "excess" transitions
    -- to the "cloned" states.
    append_transitions : NodeId -> NodeId -> List Char -> ExtDFA -> ExtDFA
    append_transitions q_m q_w transitions extDFA =
      let
        updated () =
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

      in
      case transitions of
        [] ->
          -- we're at the end of the transitions.
          -- transition_function
          updated ()
        h::t ->
          -- get all of them from q_m, and append them to q_w, EXCLUDING `h`.
          case ( delta q_m h extDFA, delta q_w h extDFA ) of
            (_, Nothing) ->
              -- I should NEVER BE HERE!  How can I be, when this is part of already-in `w` transitions??
              Debug.log "🚨🚨🚨🚨🚨 ERROR!! How can I fail to get a `w` transition via known `w`-transitions??" () |> \_ ->
              extDFA
            (Nothing, Just _) ->
              -- The q_m ends here, but q_w carries on. ∴ the remaining q_w must be "queued" nodes.
              Debug.log ("Transition (" ++ String.fromChar h ++ ") doesn't lead to anything in M. Appending last, then stopping: the rest are queued.") () |> \_ ->
              updated ()
            (Just m_node, Just w_node) ->
              -- right, get the respective transitions of each of these, then.
              Debug.log ("Appending transitions from " ++ String.fromInt q_m ++ ", excluding (" ++ String.fromChar h ++ ").") () |> \_ ->
              append_transitions
                m_node
                w_node
                t
                (updated ())
  in
    append_transitions
      extDFA_orig.start
      extDFA_orig.clone_start
      (w_forward_transitions extDFA_orig)
      extDFA_orig

remove_unreachable : ExtDFA -> ExtDFA
remove_unreachable extDFA_orig =
  -- check: are there any incoming transitions?
  let
    purge : NodeId -> ExtDFA -> ExtDFA
    purge q extDFA =
      { extDFA
        | states = IntDict.remove q extDFA.states
        , transition_function = IntDict.remove q extDFA.transition_function
        , register = Set.remove q extDFA.register
        , finals = Set.remove q extDFA.finals
      }
    mogrify : NodeId -> List Char -> ExtDFA -> ExtDFA
    mogrify q transitions extDFA =
      case transitions of
        [] ->
          if unreachable q extDFA then
            purge q extDFA
          else
            extDFA
        h::t ->
          case delta q h extDFA of
            Nothing ->
              if unreachable q extDFA then
                purge q extDFA
              else
                extDFA
            Just next ->
              if unreachable q extDFA then
                mogrify next t (purge q extDFA)
              else
                -- the killer's last hurrah…
                if unreachable next extDFA then
                  purge next extDFA
                else
                  extDFA
    unreachable : NodeId -> ExtDFA -> Bool
    unreachable q extDFA =
      extDFA.transition_function
      |> IntDict.toList
      |> List.any (\(_, dict) -> Dict.Extra.any (\_ -> (==) q) ({- Debug.log "Checking in" -} dict))
      |> not
      -- |> Debug.log ("Is " ++ String.fromInt q ++ " unreachable?")
  in
    mogrify extDFA_orig.start (w_forward_transitions extDFA_orig) extDFA_orig
    |> (\dfa -> { dfa | start = dfa.clone_start })

replace_or_register : ExtDFA -> ExtDFA
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
            |> Debug.log ("Checking outgoing for " ++ String.fromInt p)
          q_outgoing =
            IntDict.get q extDFA.transition_function
            |> Debug.log ("Checking outgoing for " ++ String.fromInt q)
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
          Debug.log ("Registering " ++ String.fromInt h ++ " as equivalent to " ++ String.fromInt found_equivalent) () |> \_ ->
          replace_or_register
            { extDFA
              | states = IntDict.remove h extDFA.states
              , finals = Set.remove h extDFA.finals
              , transition_function =
                  redirectInto found_equivalent h
                  |> IntDict.remove h
              , queue_or_clone = t
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
    extend w_dfa_orig m_dfa
    |> debugExtDFA_ "extDFA creation from merged w_dfa + dfa"
    |> phase_1
    |> debugExtDFA_ "End of Phase 1"
    |> remove_unreachable
    |> debugExtDFA_ "End of Phase 2"
    |> replace_or_register
    |> debugDFA_ "End of Phase 3"
    |> retract

addString : String -> Maybe (DFARecord {}) -> Maybe (DFARecord {})
addString string maybe_dfa =
  let
    string_dfa = string_to_dfa string
  in
    case ( maybe_dfa, string_dfa ) of
      ( Nothing, _ ) ->
        string_dfa
      ( _, Nothing ) ->
        maybe_dfa
      ( Just dfa, Just s ) ->
        Just <| debugDFA_ ("after adding '" ++ string ++ "'") <| union s dfa

wordsToDFA : List String -> DFARecord {}
wordsToDFA strings =
  List.foldl addString Nothing strings
  |> Maybe.withDefault
    { states = IntDict.empty
    , transition_function = IntDict.empty
    , start = -1
    , finals = Set.empty
    }

fromWords : List String -> AutomatonGraph
fromWords =
  List.foldl addString Nothing
  >> Maybe.map toGraph
  >> Maybe.withDefault Automata.Data.empty

toGraph : DFARecord a -> AutomatonGraph
toGraph dfa =
  let
    stateList = IntDict.toList dfa.states |> List.reverse
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
        , maxId = Tuple.first h
        , root = dfa.start
        }

transitionToString : MTransition -> String
transitionToString =
  String.fromChar

connectionToString : MConnection -> String
connectionToString =
  Set.map transitionToString
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

printDFA : DFARecord a -> String
printDFA dfa =
  "  ▶ States: " ++
    ( List.map (Tuple.first >> String.fromInt) (IntDict.toList dfa.states)
      |> String.join ", "
    )
  ++ "\n  ▶ Transitions: " ++ printTransitions dfa.transition_function
  ++ "\n  ▶ Finals: " ++ Debug.toString (Set.toList dfa.finals)
  ++ "\n  ▶ Start: " ++ String.fromInt dfa.start

printExtDFA : ExtDFA -> String
printExtDFA extDFA =
  printDFA extDFA
  ++ "\n  ▶ Register: " ++ Debug.toString (Set.toList extDFA.register)
  ++ "\n  ▶ Queue/Clones: " ++ Debug.toString extDFA.queue_or_clone
  ++ "\n  ▶ clone_start: " ++ String.fromInt extDFA.clone_start
  ++ "\n  ▶ unusedId: " ++ String.fromInt extDFA.unusedId

debugDFA_ : String -> DFARecord a -> DFARecord a
debugDFA_ s dfa =
  Debug.log (s ++ ":\n" ++ printDFA dfa) () |> \_ -> dfa

debugExtDFA_ : String -> ExtDFA -> ExtDFA
debugExtDFA_ s extDFA =
  Debug.log (s ++ ":\n" ++ printExtDFA extDFA) () |> \_ -> extDFA

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