module Utility exposing (ag, dfa, mkDFA_input, ag_equals, dfa_equals)
import Parser exposing (Parser, (|=), (|.))
import Test exposing (..)
import Automata.Data exposing (AutomatonGraph, DFARecord)
import Automata.DFA exposing (mkAutomatonGraph, mkDFA)
import Expect
import Set
import Graph exposing (NodeId)
import IntDict
import List
import Dict
import Automata.Data exposing (DFARecord)
import Automata.Debugging exposing (debugAutomatonGraph)
import List.Extra
import IntDict exposing (IntDict)
import Set exposing (Set)
import AutoSet
import AutoDict
import Maybe.Extra
import Automata.Data exposing (transitionToString)
import Automata.Data exposing (acceptConditionToString)
import Automata.Data exposing (printableAcceptCondition)

-- Parser for converting string representation to DFA transitions
dfa_transitionsParser : Parser (List (Int, Char, Int))
dfa_transitionsParser =
    Parser.oneOf
        [ Parser.succeed []
            |. Parser.end
        , Parser.loop [] dfa_transitionsHelp
        ]

dfa_transitionsHelp : List (Int, Char, Int) -> Parser (Parser.Step (List (Int, Char, Int)) (List (Int, Char, Int)))
dfa_transitionsHelp revTransitions =
    Parser.oneOf
        [ Parser.succeed (\transition -> Parser.Loop (transition :: revTransitions))
            |= dfa_transitionParser
            |. Parser.oneOf
                [ Parser.symbol " "
                , Parser.succeed ()
                ]
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revTransitions))
        ]

dfa_transitionParser : Parser (Int, Char, Int)
dfa_transitionParser =
    Parser.succeed (\src label dest -> (src, label, dest))
        |= Parser.int
        |. Parser.symbol "-"
        |= dfa_labelParser
        |. Parser.symbol "-"
        |= Parser.int

dfa_labelParser : Parser Char
dfa_labelParser =
    Parser.succeed (String.toList >> List.head >> Maybe.withDefault '🤪')
        |= Parser.getChompedString
            (Parser.succeed ()
                |. Parser.chompIf (\c -> c /= '-' && c /= ' ')
                |. Parser.chompWhile (\c -> c /= '-' && c /= ' ')
            )

mkDFA_input : String -> List (Int, Char, Int)
mkDFA_input input =
  case Parser.run dfa_transitionsParser input of
    Ok transitions ->
      transitions
    Err _ ->
      []

ag : String -> AutomatonGraph {}
ag = mkAutomatonGraph << Automata.Data.mkAG_input

dfa : String -> List Int -> Automata.Data.DFARecord {} {}
dfa s f = mkDFA_input s |> \xs -> mkDFA xs f

type PairResult a
  = One a
  | Many (List a)
ag_equals : AutomatonGraph a -> AutomatonGraph a -> Expect.Expectation
ag_equals g_expected g_actual =
  let
    getEdges g =
      Graph.edges g.graph
      |> List.map
        (\{ from, to, label } ->
          ( from
          , to
          , AutoSet.toList label
            |> List.map (Tuple.mapFirst Automata.Data.printableAcceptCondition)
          )
        )
      |> List.sortBy (\(_, _, v) -> v)
    edges1 = getEdges g_expected
    edges2 = getEdges g_actual
    partition : List (NodeId, NodeId, comparable) -> List (NodeId, NodeId, comparable) -> Set NodeId -> IntDict NodeId -> Maybe String
    partition edges_e edges_a seen mapping =
      case ( edges_e, edges_a ) of
        ( [], [] ) ->
          Nothing -- alhamdulillah!
        ( xs, [] ) ->
          debugAutomatonGraph "Expected" g_expected |> \_ ->
          debugAutomatonGraph "Actual" g_actual |> \_ ->
          Debug.log "Remaining edges" xs |> \_ ->
          Just "Expected graph had more edges than actual graph."
        ( [], ys ) ->
          debugAutomatonGraph "Expected" g_expected |> \_ ->
          debugAutomatonGraph "Actual" g_actual |> \_ ->
          Debug.log "Remaining edges" ys |> \_ ->
          Just "Actual graph had more edges than expected graph."
        ( xs, ys ) ->
          -- find all edges that start with `checking`.
          let
            (checking_e, checking_a) =
              Set.foldl (IntDict.remove) mapping seen
              |> IntDict.findMin
              |> Maybe.Extra.withDefaultLazy
                (\() ->
                  Debug.log "HUH?? How can I be here! HDIYNI" (-1, -1)
                )
            newSeen = Set.insert checking_e seen
            (xs_edges, xs_rest) =
              List.partition (\(from, _, _) -> from == checking_e) xs
            (ys_edges, ys_rest) =
              List.partition (\(from, _, _) -> from == checking_a) ys
            gather vs =
              List.Extra.gatherEqualsBy (\(_, _, c) -> c) vs
              |> List.map
                (\(a, a_s) ->
                  case a_s of
                    [] -> One a
                    _ -> Many (a::a_s)
                )
            gathered_xs = gather xs_edges
            gathered_ys = gather ys_edges
            add_to_mapping =
              List.foldl
                (\(e, a) acc ->
                  Result.andThen
                    (\m ->
                      case IntDict.get e m of
                        Just v ->
                          if v == a then
                            Ok m
                          else
                            Err <|
                              "Inconsistent state correlation: #" ++ String.fromInt a
                              ++ " vs. #" ++ String.fromInt v ++ " in actual, when trying to map #"
                              ++ String.fromInt e ++ " from expected. These graphs are not equivalent."
                        Nothing ->
                          Ok <| IntDict.insert e a m
                    )
                    acc
                )
            update_mapping (e_from, e_to, e_label) (a_from, a_to, a_label) acc =
              Result.andThen
                (\m ->
                  if a_label /= e_label then
                    Err <| "Node #" ++ String.fromInt checking_e ++ " (in expected) / #" ++ String.fromInt checking_a ++ " (in actual) have differing transitions out."
                  else
                    -- now I know that e_from is correlated with a_from, and
                    -- e_to is correlated with a_to
                    add_to_mapping (Ok m) [(e_from, a_from), (e_to, a_to)]
                )
                acc
            pair_up e_list a_list m =
              case ( e_list, a_list ) of
                ( [], [] ) ->
                  partition xs_rest ys_rest newSeen m
                ( One (e_from, e_to, e_label)::rest_e, One (a_from, a_to, a_label)::rest_a) ->
                  if a_label /= e_label then
                    Just <| "Node #" ++ String.fromInt checking_e ++ " (in expected) / #" ++ String.fromInt checking_a ++ " (in actual) have differing transitions out."
                  else
                    -- now I know that e_from is correlated with a_from, and
                    -- e_to is correlated with a_to
                    case add_to_mapping (Ok m) [(e_from, a_from), (e_to, a_to)] of
                      Err nope ->
                        Just nope
                      Ok updated ->
                        pair_up rest_e rest_a updated
                ( Many es::rest_e, Many a_s::rest_a) ->
                  -- Now this is an interesting one.
                  -- We'll only reach here when we have an NFA.
                  -- We don't know WHICH of these is actually going to be
                  -- the correct correlation.  So it's going to be
                  -- backtracking time for us.
                  if List.length es /= List.length a_s then
                    Just <| "Node #" ++ String.fromInt checking_e ++ " (in expected) / #" ++ String.fromInt checking_a ++ " (in actual) have differing transitions out."
                  else
                    let
                      allCombinations =
                        List.Extra.cartesianProduct [ List.Extra.permutations es, List.Extra.permutations a_s ]
                        |> List.filterMap
                          (\l ->
                            case l of
                              [a, b] -> Just ( a, b )
                              _ -> Nothing
                          )
                      mappings_without_immediate_conflicts =
                        List.filterMap
                          (\(ex, ax) ->
                            -- ex is a list of (from, to, label).
                            -- ax is also a list of (from, to, label).
                            -- Now, if the graphs are equivalent, ALL pairings will work.
                            List.foldl
                              (\(eDatum, aDatum) -> update_mapping eDatum aDatum)
                              (Ok m)
                              (List.Extra.zip ex ax)
                            -- now we have an "Ok updated" if this worked, or an Err if it didn't.
                            |> Result.toMaybe
                            -- If it worked, we'll now have a Just.
                          )
                          allCombinations
                      -- Now I have a list of mappings, with each mapping containing a possible
                      -- pairing that ISN'T immediately insane.
                      -- As I follow these mappings, I may find differently.
                      -- As I follow through now, if the result is Nothing, then I've found no
                      -- error.  If I find a Just, that's what the error is.
                      errorStrings =
                        List.filterMap (pair_up rest_e rest_a) mappings_without_immediate_conflicts
                      -- and what I'm hoping for here is a totally empty list, because
                      -- that will indicate that there are no errors that arose due to
                      -- the way that I have paired things up here.
                    in
                      case errorStrings of
                        [] -> Nothing
                        e::_ -> Just e
                -- this next case covers: ( [], _ ); ( _, [] ); (One _::_, Many _::_); and (Many _::_, One _::_)
                _ ->
                  Just <| "Node #" ++ String.fromInt checking_e ++ " (in expected) / #" ++ String.fromInt checking_a ++ " (in actual) have differing transitions out."
          in
            -- now, see if I can match them up.
            pair_up gathered_xs gathered_ys mapping
  in
    case partition edges1 edges2 Set.empty (IntDict.singleton g_expected.root g_actual.root) of
      Just err ->
        debugAutomatonGraph "Expected" g_expected |> \_ ->
        debugAutomatonGraph "Actual" g_actual |> \_ ->
        Expect.fail err
      Nothing ->
        Expect.pass

dfa_equals : DFARecord a b -> DFARecord a b -> Expect.Expectation
dfa_equals dfa1 dfa2 =
  let
    -- Build a canonical representation of the DFA structure starting from the start state
    -- This creates a mapping from original state IDs to canonical state IDs based on structure
    buildCanonicalMapping : DFARecord a b -> Result String (IntDict Int, Set Int)
    buildCanonicalMapping dfaRecord =
      let
        -- BFS traversal to assign canonical state IDs based on reachability from start
        traverse : List Int -> IntDict Int -> Int -> Result String (IntDict Int, Set Int)
        traverse queue mapping nextId =
          case queue of
            [] ->
              -- Determine which canonical states are final
              let
                canonicalFinals =
                  Set.foldl
                    (\originalState acc ->
                      case IntDict.get originalState mapping of
                        Just canonicalState -> Set.insert canonicalState acc
                        Nothing -> acc
                    )
                    Set.empty
                    dfaRecord.finals
              in
              Ok (mapping, canonicalFinals)
            
            currentState :: restQueue ->
              case IntDict.get currentState mapping of
                Just _ ->
                  -- Already processed this state
                  traverse restQueue mapping nextId
                Nothing ->
                  -- Assign canonical ID to this state
                  let
                    newMapping = IntDict.insert currentState nextId mapping
                    
                    -- Get all destination states from current state
                    destinations =
                      IntDict.get currentState dfaRecord.transition_function
                      |> Maybe.map (AutoDict.values >> List.filter (\dest -> not (IntDict.member dest newMapping)))
                      |> Maybe.withDefault []
                    
                    newQueue = restQueue ++ destinations
                  in
                  traverse newQueue newMapping (nextId + 1)
      in
      traverse [dfaRecord.start] IntDict.empty 0
    
    -- Convert DFA to canonical form using the mapping
    toCanonicalForm : DFARecord a b -> Result String (List (Int, String, Int), Int, Set Int)
    toCanonicalForm dfaRecord =
      buildCanonicalMapping dfaRecord
      |> Result.andThen (\(mapping, canonicalFinals) ->
          let
            canonicalStart =
              IntDict.get dfaRecord.start mapping
              |> Result.fromMaybe ("Start state not found in mapping")
            
            canonicalTransitions =
              IntDict.toList dfaRecord.transition_function
              |> List.concatMap (\(src, transitions) ->
                  case IntDict.get src mapping of
                    Nothing -> []
                    Just canonicalSrc ->
                      AutoDict.toList transitions
                      |> List.filterMap (\(acceptanceCondition, dest) ->
                          IntDict.get dest mapping
                          |> Maybe.map
                            (\canonicalDest ->
                              ( canonicalSrc
                              , printableAcceptCondition acceptanceCondition
                              , canonicalDest)
                              )
                        )
                )
              |> List.sort
          in
          canonicalStart
          |> Result.map (\start -> (canonicalTransitions, start, canonicalFinals))
        )
    
    -- Compare the canonical forms
    result1 = toCanonicalForm dfa1 |> Debug.log "Canonical form 0"
    result2 = toCanonicalForm dfa2 |> Debug.log "Canonical form 1"
  in
  case (result1, result2) of
    (Err err1, _) ->
      Expect.fail ("Error processing first DFA: " ++ err1)
    (_, Err err2) ->
      Expect.fail ("Error processing second DFA: " ++ err2)
    (Ok (transitions1, start1, finals1), Ok (transitions2, start2, finals2)) ->
      if start1 /= start2 then
        Expect.equal start1 start2
      else if transitions1 /= transitions2 then
        Expect.equal transitions1 transitions2
      else if finals1 /= finals2 then
        Expect.equal finals1 finals2
      else
        Expect.pass