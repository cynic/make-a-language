module Automata.Verification exposing (..)
import Automata.Data exposing (..)
import Graph exposing (NodeId, Graph)
import IntDict
import Set exposing (Set)
import Result.Extra
import List.Extra as List
import AutoSet

numNodes : AutomatonGraph -> Int
numNodes dawg =
  Graph.size dawg.graph

numEdges : AutomatonGraph -> Int
numEdges dawg =
  List.length <| Graph.edges dawg.graph

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

processStack : List (Node, String, Bool) -> List String -> Graph Entity Connection -> List String
processStack stack acc graph =
  case stack of
    [] -> acc
    (n, s, f)::rest ->
      processStack
        (explore n s graph ++ rest)
        (if f then s::acc else acc)
        graph

recognizedWordsFrom : AutomatonGraph -> Node -> Result String (List String)
recognizedWordsFrom dawg root =
  case Graph.checkAcyclic dawg.graph of
    Err edge ->
      Err <| "Edge " ++ String.fromInt edge.from ++ "→" ++ String.fromInt edge.to ++ " creates a cycle; this is not a DAWG."
    Ok _ ->
      Ok <| processStack [(root, "", False)] [] dawg.graph

-- Entry point function
recognizedWords : AutomatonGraph -> List String
recognizedWords dawg =
  Maybe.map
    (recognizedWordsFrom dawg >> Result.map List.sort >> Result.mapError identity)
    (Graph.get dawg.root dawg.graph)
  |> Maybe.withDefault (Err "Couldn't find the root in the DAWG…!  What on earth is going on?!")
  |> Result.Extra.extract (\e -> [e])

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

type alias Partition = Set NodeId
type alias HopcroftRecord =
  { w : List Partition -- still to be processed.
  , p : List Partition -- partitions
  }

minimality : AutomatonGraph -> List (List Int)
minimality g =
  -- This is Hopcroft's Algorithm
  let
    edges = -- Edge (Transition)
      Graph.edges g.graph
      |> List.concatMap
        (\{from, to, label} ->
          AutoSet.toList label
          |> List.map (\t -> (from, to, t))
        )
    (finals, nonFinals) = -- the initial partition.
      -- those which lead to finality, and those which don't.
      List.partition (\(_, _, {isFinal}) -> isFinal) edges
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
