module Automata.Verification exposing (..)
import Automata.Data exposing (..)
import Graph exposing (NodeId)
import Set exposing (Set)
import List.Extra as List
import AutoSet

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
      -- those which lead to finality—however that happens—and those which don't.
      List.partition (\(_, _, t) -> .isFinal t) edges
      |> \(a, b) -> ( List.map (\(_,v,_) -> v) a |> Set.fromList, List.map (\(_,v,_) -> v) b |> Set.fromList )
      -- |> debug_log "[minimality] Finals and non-finals"
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
              -- |> debug_log ("[minimality] `X` set, given `A` of " ++ (Debug.toString (Set.toList a)))
            refine_for_input_and_y : Partition -> Partition -> Partition -> List Partition -> List Partition -> (List Partition, List Partition)
            refine_for_input_and_y y further_split remaining_after w p =
              ( if List.member y w then
                  (further_split :: remaining_after :: List.remove y w)
                  -- |> debug_log "[minimality] Refining w, stage Ⅱa"
                else
                  if Set.size further_split <= Set.size remaining_after then
                    (further_split :: w)
                    -- |> debug_log "[minimality] Refining w, stage Ⅱb"
                  else
                    (remaining_after :: w)
                    -- |> debug_log "[minimality] Refining w, stage Ⅱc"
              , (further_split :: remaining_after :: List.remove y p)
                -- |> debug_log "[minimality] Refining p, stage Ⅱ"
              )
            refine_for_input : Transition -> Partition -> List Partition -> List Partition -> (List Partition, List Partition)
            refine_for_input _ x w p = -- really, the transition _ is only there for potential debugging.
              let
                candidate_sets =
                  List.filterMap
                    (\potential_y ->
                      let
                        further_split =
                          Set.intersect x potential_y
                          -- |> debug_log ("Intersection of " ++ Debug.toString x ++ " and " ++ Debug.toString potential_y)
                        remaining_after =
                          Set.diff potential_y x
                          -- |> debug_log ("Subtraction: " ++ Debug.toString potential_y ++ " minus " ++ Debug.toString x)
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
                    -- println ("Refining for input " ++ transitionToString t)
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
