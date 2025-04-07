module DAWG.Simplify3 exposing (..)
import DAWG.Data exposing (..)
import DAWG.Debugging exposing (..)
import IntDict
import Set exposing (Set)
import Maybe.Extra as Maybe
import List.Extra as List
import Set.Extra as Set
import Graph
import Dict

{-| Like foldl, but do something special with the last one.
-}
-- this is modified from the elm/core 1.0.5 source.
foldlSpecial : (a -> b -> b) -> (a -> b -> c) -> b -> List a -> Maybe c
foldlSpecial func lastItem acc list =
  case list of
    [] ->
      Nothing

    [x] ->
      Just <| lastItem x acc

    x :: xs ->
      foldlSpecial func lastItem (func x acc) xs

expressionToDAWG : ExprAST -> (Int, Int) -> ToDawgRecord -> ToDawgRecord
expressionToDAWG expr (start, end) r =
  case expr {- |> debugLog ("Processing " ++ symbolFor expr ++ " fragment") (\e -> "s=" ++ String.fromInt start ++ ", e=" ++ String.fromInt end ++ ", " ++ dawgRecordToString r ++ ", expr=" ++ exprASTToString e) -} of
    V data ->
        -- this is the main case, where we create or reuse an edge.
        -- println ("Creating #" ++ String.fromInt start ++ "➜#" ++ String.fromInt end ++ " for " ++ transitionToString data)
        { r
          | incoming = IntDict.update end (Maybe.map (\l -> (start, data) :: l) >> Maybe.orElseLazy (\_ -> Just <| [(start, data)])) r.incoming
          , outgoing = IntDict.update start (Maybe.map (Set.insert (end, data)) >> Maybe.orElseLazy (\_ -> Just <| Set.singleton (end, data))) r.outgoing
          , unused = r.unused + 1
        }
    M (x::_ as xs) ->
      -- println "Processing ×"
      foldlSpecial
        (\item (acc, (start_, end_)) ->
          expressionToDAWG item (start_, end_) acc
          -- |> debugLog "Moving to next item" dawgRecordToString
          -- I cannot share in the middle of a sequence, can I?
          |> \acc2 -> ( { acc2 | unused = acc2.unused + 1 }, (end_, acc2.unused) )
          -- |> debugLog "Post-update ×-accumulator" (\(a,(s,e)) -> "s=" ++ String.fromInt s ++ ", e=" ++ String.fromInt e ++ ", " ++ dawgRecordToString a)
        )
        (\item (acc, (start_, _)) ->
          -- I can share at the end of a sequence.
          expressionToDAWG item (start_, end) acc
          |> \acc2 -> ( { acc2 | unused = acc2.unused + 1 } )
          -- |> debugLog "After handling last ×-item in sequence" dawgRecordToString
        )
        ( { r | unused = r.unused + 1 }, (start, r.unused) )
        xs
      |> Maybe.withDefaultLazy (\_ -> debug_log "ERROR! M without multiple items!" <| expressionToDAWG x (start, end) r)
    A xs ->
      -- println "Processing +"
      List.foldl
        (\item acc -> expressionToDAWG item (start, end) acc)
        r
        xs
    M [] ->
      r |> debug_log "ERROR — found an M value with ZERO items!"

heads : List ExprAST -> Set Transition
heads xs =
  let
    handle_add add_terms acc =
      case add_terms of
        [] -> acc
        V t :: rest -> handle_add rest (Set.insert t acc)
        M (V t::_) :: rest -> handle_add rest (Set.insert t acc)
        _::rest -> handle_add rest acc
    heads_inner remaining acc =
      case remaining of
        [] -> acc
        M ((V t)::_)::rest -> heads_inner rest (Set.insert t acc)
        V t :: rest -> heads_inner rest (Set.insert t acc)
        M (A inner::_)::rest ->
          heads_inner rest (handle_add inner acc)
        _::rest -> heads_inner rest acc
  in
  -- get all the initial transitions from "Multiply" values,
  -- AND all the single variables
  heads_inner xs Set.empty

-- samePrefixMuls : List ExprAST -> List ExprAST
-- samePrefixMuls xs =
--   let
--     samePrefixMuls_inner remaining idx acc =
--       case remaining of
--         M (V (ch,isFinal)::rem) :: rest ->
--           samePrefixMuls_inner rest (idx + 1) <|
--             Dict.update ch
--               (\datum ->
--                 case datum of
--                   Nothing ->
--                     Just ([idx], isFinal == 1, [rem])
--                   Just (otherIdxes, isKnownFinal, previouslyFound) ->
--                     Just (idx::otherIdxes, isKnownFinal || isFinal == 1, rem :: previouslyFound)
--               )
--               acc
--         M (A inner :: rem) :: rest ->
--           samePrefixMuls_inner rest (idx + 1) <|
--   in

{-|This applies Rule #9, Common Prefix Collapse, to an “Add” value.
-}
commonPrefixCollapse : (List ExprAST) -> List ExprAST
commonPrefixCollapse xs =
  let
    samePrefixMuls =
      -- get all the "Multiply" values
      List.filter
        (\v ->
          case v of
            M _ -> True
            _ -> False
        )
        -- (debugLog "Prefix-collapse; received" (A)
        xs
        -- )
      -- triple-check to ensure that the same values are structurally the same
      |> List.map sortAST
      |> List.gatherEqualsBy
        (\v ->
          case v of
            M (x::_) -> Just x
            _ -> Nothing
        )
      -- |> debug_log "All heads"
      |> List.filter (not << (Tuple.second >> List.isEmpty))
      |> List.map (\(fst, rest) -> fst::rest)
      -- |> debug_log "Same-prefix heads"

    -- get all the "Multiply" values
    replacements =
      samePrefixMuls
      |> List.filterMap
        (\l ->
          case l of
            M (h::_)::_ ->
              Just
                ( {- debug_log "Common prefix" -} h
                , List.concatMap
                    (\m ->
                      case m of
                        M [_, A inner] -> inner
                        M [_, v] -> [v]
                        M (_::rest) -> [M rest]
                        _ -> debug_log "SHOULD NOT REACH HERE!" [m] -- should not reach here!
                    )
                    l
                )
            _ -> Nothing
        )
      |> List.map
        (\(head, tails) ->
          M [head, A tails]
          -- |> debug_log "replacement"
        )
  in
    case samePrefixMuls of
      [] -> -- no common prefix collapse. Rely on the caller to simplify within.
        xs
      sames ->
        -- debug_log "Rule #9: ☝🏾 subject to: Common Prefix Collapse" () |> \_ ->
        -- remove all the "same-prefix" items from inside.
        List.foldl
          (\same state -> List.foldl List.remove state same)
          xs sames
        -- and replace them with the replacements
        |> (\l -> l ++ replacements)
        |> List.map sortAST
        -- |> debug_log "Prefix-collapse result"

{-|This applies Rule #10, Finality Primacy, to an “Add” value.

Implemented: ẋ + x.y = ẋ + ẋ.y
…which is slightly different to my notes, but I think it should work.
-}
finalityPrimacy : List ExprAST -> List ExprAST
finalityPrimacy xs =
  let
    -- get all the initial transitions from "Multiply" values,
    -- AND all the single variables
    collected_transitions = heads xs
      -- |> debug_log "Heads & single values"
    to_bump =
      Set.filterMap
        (\(c, _) ->
          if Set.member (c, 0) collected_transitions && Set.member (c, 1) collected_transitions then
            Just c
          else
            Nothing
        )
        collected_transitions
      |> Set.toList
      -- |> debug_log "Found primacy items"
  in
    case to_bump of
      [] -> -- Nothing to promote.  Rely on caller to simplify.
        xs
      _ ->
        -- debug_log "Rule #10: ☝🏾 subject to: Finality Primacy" () |> \_ ->
        List.foldl
          (\ch state ->
            List.filterMap
              (\item ->
                case item of
                  M (V x::rest) ->
                    if x == (ch, 0) then
                      Just <| M (V (ch, 1)::rest)
                    else
                      Just item
                  V (x, _) ->
                    if x == ch then
                      Nothing
                    else
                      Just <| item
                  M (A inner::rest) ->
                    List.map
                      (\x ->
                        case x of
                          V t ->
                            if t == (ch, 0) then
                              V (ch, 1)
                            else
                              V t
                          M (V t::rest_) ->
                            if t == (ch, 0) then
                              M <| V (ch, 1)::rest_
                            else
                              M <| V t::rest_
                          _ -> x
                      )
                      inner
                      -- |> debug_log "After map"
                    |> \out -> Just <| M (A out::rest)
                  x -> Just x
              )
              state
          )
          xs
          to_bump

{-|This applies Rule #5, Common Suffix Collapse, to an “Add” value.
-}
commonSuffixCollapse : (List ExprAST) -> List ExprAST
commonSuffixCollapse xs =
  let
    sameSuffixMuls =
      -- get all the "Multiply" values
      List.filter
        (\v ->
          case v of
            M _ -> True
            _ -> False
        )
        -- (debugLog "Suffix-collapse; received" exprASTsToString
        xs
        -- )
      -- triple-check to ensure that the same Multiply values look the same
      |> List.map sortAST
      -- find the ones which have the same last part
      |> List.gatherEqualsBy
          (\v ->
            case v of
              M sequence ->
                List.last sequence
                -- |> debugLog "Got a potential" (Maybe.map exprASTToString)
              _ -> Nothing
          )
      |> List.filter (not << (Tuple.second >> List.isEmpty))
      |> List.map (\(fst, rest) -> fst::rest)
    to_remove =
      sameSuffixMuls |> List.concatMap identity
    replacements =
      sameSuffixMuls -- this is a list of lists.  Each inner list contains a grouping for a different common suffix.
      |> List.filterMap
        (\groupItems ->
          let
            common_suffix =
              case groupItems of
                M items::_ ->
                  List.last items
                  -- |> debug_log "Common suffix"
                _ -> Nothing
            add_part =
              List.filterMap
                (\v ->
                  case v of
                    M items ->
                      List.unconsLast items
                      |> Maybe.map
                        (\(_, pre) ->
                          case pre of
                            [one] -> one
                            more -> M more
                        )
                    _ -> Nothing
                )
                groupItems
          in
            Maybe.map
              (\suffix -> M [A add_part, suffix])
              common_suffix
        )
  in
    case sameSuffixMuls of
      [] -> -- no common suffix collapse.
        xs
      _ ->
          -- debug_log "Rule #5: ☝🏾 subject to: Common Suffix Collapse" () |> \_ ->
          -- remove all the "same-suffix" items from inside.
          List.foldl List.remove xs to_remove
          -- and replace them with the replacements
          |> (\l -> l ++ replacements)
          |> List.map sortAST
          -- |> debug_log "Result of suffix-collapse"

simplify_inner : (ExprAST -> ExprAST) -> List ExprAST -> (List ExprAST -> ExprAST) -> ExprAST
simplify_inner s inner wrap =
  let
    inner_simplified =
      -- println "Simplifying the inner values individually."
      List.map s inner
  in
    case ( inner_simplified, inner_simplified /= inner ) of
      ( [x], True ) ->
        -- println "?-Inner reduced to one value; re-running."
        s x
      ( _, True ) ->
        -- println "Changes made to ?-inner; re-running with outer nesting."
        s <| wrap inner_simplified
      ( [x], False ) ->
        -- println "?-Inner reduced to one value; returning."
        x
      ( _, False ) ->
        -- println "No changes made to ?-inner; wrapping in ? and returning."
        wrap inner_simplified

simplifyWith : (List ExprAST -> List ExprAST) -> ExprAST -> ExprAST
simplifyWith simplifier e =
  case e of
    A xs ->
      -- debugLog "Simplifying +" exprASTToString e |> \_ ->
      let
        post_simplification = simplifier xs
      in
        -- println "In +" |> \_ ->
        if post_simplification /= xs then
          case post_simplification of
            [x] ->
              -- println "A-Simplification resulted in the removal of A-nesting; simplifying the result."
              simplifyWith simplifier x
            _ ->
              -- println "A-Simplification resulted in changes; re-running."
              simplifyWith simplifier <| A post_simplification
        else
          simplify_inner (simplifyWith simplifier) post_simplification
            ( List.concatMap
                (\v ->
                  case v of
                    A inner -> inner
                    x -> [x]
                )
            >> A
            )

      -- Also see if we can apply Rule 5: Common Suffix Collapse
    M xs -> -- generic.
      -- debugLog "Simplifying ×" exprASTToString e |> \_ ->
      simplify_inner (simplifyWith simplifier) xs
        ( List.concatMap
            (\v ->
              case v of
                M inner -> inner
                x -> [x]
            )
        >> M
        )
    V x ->
      -- println "In V"
      V x -- base case

simplify : ExprAST -> ExprAST
simplify =
  simplifyWith (finalityPrimacy >> commonPrefixCollapse >> commonSuffixCollapse)

-- JUST AS AN EXAMPLE!  This leads to a NAFSA, of course.
-- simplifyBack : ExprAST -> ExprAST
-- simplifyBack =
--   simplifyWith (finalityPrimacy >> commonSuffixCollapse >> commonPrefixCollapse)

sortAST : ExprAST -> ExprAST
sortAST e =
  let
    compare a b =
      case (a, b) of
        (V x, V y) ->
          Basics.compare x y
        (V _, _) ->
          LT
        (_, V _) ->
          GT
        (M (x::_), M (y::_)) ->
          compare x y
        (A (x::_), A (y::_)) ->
          compare x y
        _ ->
          EQ
        
  in
    case e of
      V x -> V x
      M xs -> M (List.map sortAST xs)
      -- the .unique here implements Rule #3
      A xs ->
        case List.unique xs |> List.sortWith compare of
          [y] -> y
          ys -> A ys

collapse_nodes : Int -> List Int -> List (Int, (Int, Transition)) -> ToDawgRecord -> ToDawgRecord
collapse_nodes new_target to_remove edges_to_redirect r =
  { r
    | incoming =
        IntDict.update new_target
          (Maybe.map
            (\xs ->
              Set.union
                (Set.fromList <| List.map (\(a, (_, t)) -> (a, t)) edges_to_redirect)
                (Set.fromList xs)
              |> Set.toList
            )
          )
          r.incoming
    , outgoing =
        List.foldl IntDict.remove r.outgoing to_remove
        |>( \o ->
            List.foldl (\(k, (old, t)) o_ -> IntDict.update k (Maybe.map (Set.remove (old, t) >> Set.insert (new_target, t))) o_) o edges_to_redirect
          )
  }

type alias MergeModification =
  { newTarget : Int
  , toRemove : List Int
  , edgesToRedirect : List (Int, (Int, Transition))
  }

merge_modifications : Int -> ToDawgRecord -> List MergeModification
merge_modifications dst r =
  -- this will give me a list containing sets of (src, (dest, transition)) pairs.
  -- If /n/ of those sets are the same, then we can merge them!
  let
    in_nodes =
      IntDict.get dst r.incoming
      |> Maybe.map (List.map Tuple.first >> List.unique)
      |> Maybe.withDefault []
      -- |> debug_log ("incoming (@ #" ++ String.fromInt dst ++ ")")
    combinable_nodes =
      List.filterMap (\n -> IntDict.get n r.outgoing |> Maybe.map (\v -> ( n, v ) )) in_nodes
      |> List.gatherEqualsBy Tuple.second
      |> List.filterMap
        (\( (src, _), same ) ->
          case same of
            [] -> -- okay, this is the only one.  That's fine then.  We cannot merge with it.
              Nothing
            _ ->
              Just <|
                { newTarget = src
                , toRemove = List.map Tuple.first same
                , edgesToRedirect =
                    List.concatMap
                      (\(d, _) ->
                        IntDict.get d r.incoming -- get all the incoming nodes, which should be redirected.
                        |> Maybe.map (\xs -> List.map (\(src_, t) -> (src_, (d, t))) xs)
                        |> Maybe.withDefault []
                      )
                      same
                } -- these are all the ones that can be merged.
        )
      -- |> debug_log "[ ( NewTarget, (ToRemove, EdgesToRedirect) ) ]"
    -- Should now be able to remove the excess node(s) from the "incoming" and "outgoing" lists.
  in
    combinable_nodes

collapse : List Int -> ToDawgRecord -> ToDawgRecord
collapse remaining r =
  case remaining of
    [] -> r
    x::rest ->
      case merge_modifications x r of
        [] -> collapse rest r
        mods ->
          let
            reCheck =
              List.concatMap (\{edgesToRedirect} -> List.map Tuple.first edgesToRedirect) mods
              -- |> debug_log "List for rechecking"
          in
            List.foldl
              (\mod ->
                collapse_nodes mod.newTarget mod.toRemove mod.edgesToRedirect
                -- >> debugLog "Now" dawgRecordToString
              )
              r
              mods
            -- This doesn't work when it's `reCheck ++ rest`, because it needs to take into
            -- account other changes that happen.
            |> collapse (rest ++ reCheck)

coalesceToGraphNodes : ToDawgRecord -> List (Graph.Edge Connection)
coalesceToGraphNodes {outgoing} =
  outgoing
  |> IntDict.map
    (\from outgoings ->
      -- Find the outgoing edges, grouped by destination, and turn them into connections.
      Set.toList outgoings
      |> List.gatherEqualsBy Tuple.first
      |> List.map
        (\((to, t), xs) ->
            { from = from
            , to = to
            , label = Set.fromList (t :: List.map Tuple.second xs)
            }
        )
    )
  |> IntDict.values
  |> List.concat

algebraToDAWG : ExprAST -> DAWG
algebraToDAWG e =
  let
    flattened = sortAST e -- |> debugLog "flattened" exprASTToString
    simplified = simplify flattened {- |> debug_log "Simplified, raw" -} |> debugLog "Simplified, round-trip" exprASTToRTString
    graphEdges =
      expressionToDAWG simplified (0, 1) (ToDawgRecord 2 IntDict.empty IntDict.empty)
      -- |> debugLog "Expression to DAWG" dawgRecordToString
      |> (\r -> collapse (IntDict.keys r.incoming) r)
      -- |> debugLog "Post-collapse" dawgRecordToString
      -- |> .edges
      -- |> List.map (\{start, end, data} -> Graph.Edge start end (Set.singleton data))
      |> coalesceToGraphNodes
      -- |> redirectPrefixGraphNodes
    (maxId, nodes) =
      graphEdges
      |> List.foldl
        (\{from, to} acc -> (Set.insert from >> Set.insert to) acc)
        Set.empty
      |> Set.toList
      |> \l -> (List.last l, List.map (\n -> Graph.Node n ()) l)
  in
    Maybe.map (\max ->
      DAWG (Graph.fromNodesAndEdges nodes graphEdges) max 0 (Just 1)
      -- |> \dawg -> List.foldl (checkForCollapse) dawg (1::(List.map (\n -> n.id) nodes))
    ) maxId
    |> Maybe.withDefault empty
    -- |> debugDAWG "tada"
