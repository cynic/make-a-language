module DAWG exposing (..)
import Graph exposing (Graph, NodeContext, NodeId, Node, Adjacency)
import Set exposing (Set)
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe
import Basics.Extra exposing (..)
import IntDict
import Result.Extra
import Set.Extra
import Parser as P exposing (Parser, (|.), (|=), succeed, symbol, oneOf, lazy, spaces, end, getChompedString, chompIf, sequence, loop, Step(..))
import Char.Extra

type ExprAST
  = M (List ExprAST)
  | A (List ExprAST)
  | V Transition

debug_log : String -> a -> a
debug_log s x =
  -- Debug.log s x
  x

-- Top-level parser
expressionParser : Parser ExprAST
expressionParser =
  term |. spaces |. end

combine : (List ExprAST -> ExprAST) -> ExprAST -> List ExprAST -> ExprAST
combine f head tail =
  case tail of
    [] -> head
    _ -> f (head :: tail)

-- Term: Handles addition (+)
term : Parser ExprAST
term =
  succeed (combine A)
    |= factor
    |= loop [] (termHelper "+" factor)

-- Factor: Handles multiplication (.)
factor : Parser ExprAST
factor =
  succeed (combine M)
    |= primary
    |= loop [] (termHelper "." primary)

-- Helper for building lists of operations
termHelper : String -> Parser ExprAST -> List ExprAST -> Parser (Step (List ExprAST) (List ExprAST))
termHelper operator parser tail =
  oneOf
    [ succeed (\elem -> Loop (elem :: tail))
        |. symbol operator
        |. spaces
        |= parser
    , succeed (Done (List.reverse tail))
    ]

-- Primary: Variables or grouped terms
primary : Parser ExprAST
primary =
  oneOf
    [ variable
    , grouped
    ]

-- Single-character variable parser
variable : Parser ExprAST
variable =
  let
    cParser = \c -> not (Char.Extra.isSpace c || c == '+' || c == '.' || c == '(' || c == ')' || c == '!')
    parseChar finality =
      chompIf cParser
      |> getChompedString
      |> P.andThen
        (\s ->
          case String.uncons s of
            Just (c, "") -> succeed <| V (c, finality)
            _ -> P.problem "Variables must be single characters"
        )
  in
    oneOf
      [ symbol "!!" |> P.andThen (\_ -> succeed <| V ('!', 1))
      , symbol "!" |> P.andThen (\_ -> parseChar 1)
      , parseChar 0
      ]

-- Grouped expressions (parentheses)
grouped : Parser ExprAST
grouped =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> term)
        |. spaces
        |. symbol ")"

{-
  I always know where to attach FROM: it is some node I've already created or know about or have seen.
  But I don't know where to attach TO always, and I think that it is tricky because when I have something
  like a+b+c+d, all of them must attach to the SAME end-point… but where will that be?  Well, when going in,
  I am using prefix notation, so I already know the operation and I also know the operands.

  When it is multiplication, things are fairly easy, because I know that they all follow on from each other.

  However, when it is addition, then all the splits have to converge somewhere.  Let's look at one:

  a.b.(x+c.d.(e+g)+f).p
  gives us
  (× (× (× a b) (+ (+ x (× (× c d) (+ e g))) f)) p)

  I will see this as
    Mul0 (Mul1, Var p) where
      Mul1 (Mul2, Add0) where
        Mul2 (Var a, Var b) and
        Add0 (Add1, Var f) where
          Add1 (Var x, Mul3) where
            Mul3 (Mul4, Add2) where
              Mul4 (Var c, Var d) and
              Add2 (Var e, Var g).

  So this looks, from the parsing perspective, a little more messy.  Okay.  So, how do I sort it out?
  Well, I can try making bits of the graph and then joining them together as I go up again.  That way,
  I always have the start and end of things, because a Var always joins two nodes.  Basically, each Var
  will form an Edge.  Now an Edge must go from somewhere to somewhere else.  INITIALLY… let's say that
  it goes from Nothing to Nothing, and then we will fill in those blanks as we go.

  Okay, so: how would we handle the above?

  1. When we get `Mul0 (Mul1, Var p)`, create (Nothing, {p}, Nothing).  Since it's at the end, it will be the
     destination for the end of `Mul1`.  Now descend; we'll come back here in a bit.
  2. In `Mul1 (Mul2, Add0)`, we see two parts.  We will need to join them later.  For now, handle each
     individually, and we'll make a join-point later.  Descend into Mul2.
  3. In `Mul2 (Var a, Var b)` we can create a good old-fashioned sequence: (Nothing, {a}, 1) and (1, {b}, Nothing).
     Return functions to set the "start" and "end" of this sequence.
  4. And now we will be back in (2).  We descend into `Add0`.
  5. In `Add0`, we see `Var f` and can create (Nothing, {f}, Nothing).  We also see `Add1`, so we descend.
  6. In `Add1`, we see `Var f` and can create (Nothing, {x}, Nothing).  We then descend into `Mul3`.
  7. In Mul3, we see `Mul4` and `Add2`.  Let's descend into `Mul4`.
  8. In Mul4, we see `Var c` and `Var d`.  Great!  We create (Nothing, {c}, 2) and (2, {d}, Nothing).  Return functions
     to set the "start" and "end" of this sequence.
  9. Now we are back in (7), so let us descend into `Add2`.
  10.In `Add2`, we see `Var e` and `Var g`.  We can create (Nothing, {e}, Nothing) and (Nothing, {g}, Nothing), and
     we can pass back appropriate functions.  Because this is an `Add`, when we set the "start", we would be setting
     the start of BOTH of these things; and likewise for the end.
  11.This takes us back to (7).  Now we have the results of the `Mul4` and the `Add2`.  Choose the next available
     number and use it to set the "end" of the first item and the "start" of the second item to the same value.  Then
     return the appropriate "start" and "end" functions, as received from the called functions.
  12.This takes us back to (6).  We can change (Nothing, {f}, )…

-}

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

type alias EdgeRecord = Graph.Edge Transition

type alias ToDawgRecord =
  { unused : Int
  , incoming : IntDict.IntDict (List (Int, Transition)) -- source nodes, keyed by destination
  , outgoing : IntDict.IntDict (Set (Int, Transition)) -- destination nodes, keyed by source
  }

-- dawgRecordToString : ToDawgRecord -> String
-- dawgRecordToString r =
--   "unused=" ++ String.fromInt r.unused ++ ", incoming=" ++ (IntDict.toList r.incoming |> Debug.toString) ++ ", outgoing=" ++ (IntDict.toList r.outgoing |> Debug.toString)

-- symbolFor : ExprAST -> String
-- symbolFor expr =
--   case expr of
--     V _ -> "⏺"
--     M _ -> "×"
--     A _ -> "+"

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
    heads =
      -- get all the initial transitions from "Multiply" values,
      -- AND all the single variables
      List.concatMap
        (\v ->
          case v of
            M ((V t)::_) -> [t]
            V t -> [t]
            M (A inner::_) ->
              List.filterMap
                (\v_ ->
                  case v_ of
                    V t -> Just t
                    M (V t::_) -> Just t
                    _ -> Nothing
                )
                inner
            _ -> []
        ) xs
      |> List.unique
      -- |> debug_log "Heads & single values"
    collected_transitions =
      List.foldl Set.insert Set.empty heads
    to_bump =
      Set.Extra.filterMap
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

parseAlgebra : String -> Result (List P.DeadEnd) ExprAST
parseAlgebra =
  P.run expressionParser

stringToExpression : String -> ExprAST
stringToExpression s =
  case String.toList s of
    [] -> A []
    [x] -> V (x, 1)
    _ -> wordToTransitions s |> List.map V |> M

wordsToAlgebra : List String -> ExprAST
wordsToAlgebra xs =
  case xs of
    [] -> A []
    [x] -> stringToExpression x
    _ -> A <| List.map stringToExpression xs

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
      |> \l -> (List.last l, List.map (\n -> Node n ()) l)
  in
    Maybe.map (\max ->
      DAWG (Graph.fromNodesAndEdges nodes graphEdges) max 0 (Just 1)
      -- |> \dawg -> List.foldl (checkForCollapse) dawg (1::(List.map (\n -> n.id) nodes))
    ) maxId
    |> Maybe.withDefault empty
    -- |> debugDAWG "tada"

fromLines : String -> DAWG
fromLines s =
  s
  |> String.replace " " ""
  |> String.replace "\n" ""
  |> parseAlgebra
  |> Result.map algebraToDAWG
  |> Result.withDefault empty

-- Note: Graph.NodeId is just an alias for Int. (2025).

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.
type alias Transition = (Char, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias Connection = Set Transition -- a Connection is a link between two nodes.
type alias Connections = IntDict.IntDict Connection
type alias Node = NodeContext () Connection -- a Node itself does not carry any data, hence the ()
type alias DAWGGraph = Graph () Connection -- finally, the complete directed acyclic word graph.
type alias DAWG =
  { graph : DAWGGraph
    {- The maximum ID-value in this DAWG graph -}
  , maxId : NodeId
  , root : NodeId
  , final : Maybe NodeId
  }


{-| True if at least one transition terminates at this node -}
isTerminalNode : Node -> Bool
isTerminalNode node =
  IntDict.foldl
    (\_ conn state ->
      state || 
        Set.foldl
          (\(_, isFinal) state_ -> state_ || isFinal == 1)
          False
          conn
    )
    False
    (node.incoming)

{--
  Output/debugging functions
--}

exprASTToRTString : ExprAST -> String
exprASTToRTString e =
  case e of
    V transition ->
      case transition of
        (ch, 0) -> String.fromChar ch
        (ch, _) -> "!" ++ String.fromChar ch
    M xs ->
      List.map exprASTToRTString xs |> String.join "."
    A xs ->
      "(" ++ (List.map exprASTToRTString xs |> String.join " + ") ++ ")"

exprASTToString : ExprAST -> String
exprASTToString e =
  case e of
    V t ->
      transitionToString t
    M xs ->
      -- "[× " ++ (List.map exprASTToString xs |> String.join ", ") ++ "]"
      List.map exprASTToString xs |> String.join "."
    A xs ->
      -- "[+ " ++ (List.map exprASTToString xs |> String.join ", ") ++ "]"
      "[" ++ (List.map exprASTToString xs |> String.join " + ") ++ "]"

exprASTsToString : List ExprAST -> String
exprASTsToString es =
  List.map exprASTToString es |> String.join "; "

graphEdgeToString : Graph.Edge Connection -> String
graphEdgeToString {from, to, label} =
  "#" ++ String.fromInt from ++ "➜#" ++ String.fromInt to ++ " (" ++ connectionToString label ++ ")"

transitionsToString : List Transition -> String
transitionsToString transitions =
  String.join "," (List.map transitionToString transitions)

transitionToString : Transition -> String
transitionToString transition =
  case transition of
    (ch, 0) ->
      String.fromChar ch
    (ch, _) ->
      "\u{0307}" ++ String.fromChar ch

connectionToString : Connection -> String
connectionToString =
  Set.map transitionToString
  >> Set.toList
  >> String.join "\u{FEFF}" -- zero-width space. Stops terminality-marker from disappearing on subsequent characters.

graphToString : DAWGGraph -> String
graphToString graph =
  Graph.toString
    (\_ -> Nothing)
    (Just << connectionToString)
    graph

debugGraph : String -> DAWGGraph -> DAWGGraph
debugGraph txt graph =
  debug_log txt (graphToString graph)
  |> \_ -> graph

debugDAWG : String -> DAWG -> DAWG
debugDAWG txt dawg =
  debug_log txt
    (graphToString dawg.graph)
  |> \_ -> dawg

println : String -> a -> a
println txt x =
  debug_log txt () |> \_ -> x

debugLog : String -> (a -> b) -> a -> a
debugLog s f v =
  debug_log s (f v) |> \_ -> v

{--
  User-facing functions (and a few helpers thereof)
--}

wordToTransitions : String -> List Transition
wordToTransitions txt =
  Maybe.map
    (\(last, rest) ->
      List.map (\ch -> (ch, 0)) rest
      |> \transitions -> transitions ++ [(last, 1)]
    )
    (txt |> String.toList |> List.unconsLast)
  |> Maybe.withDefault [] -- don't accept an empty-string as valid.

empty : DAWG
empty =
  let
    initial =
      { node = Node 0 ()
      , incoming = IntDict.empty
      , outgoing = IntDict.empty
      }
  in
    { graph = Graph.insert initial Graph.empty
    , maxId = 0
    , root = 0
    , final = Nothing
    }

fromWords : List String -> DAWG
fromWords =
  wordsToAlgebra >> algebraToDAWG

numNodes : DAWG -> Int
numNodes dawg =
  Graph.size dawg.graph

numEdges : DAWG -> Int
numEdges dawg =
  List.length <| Graph.edges dawg.graph

{-| Explores incrementally in a breadth-first manner, returning a
    LIST of (node-found, new-string, is-final) -}
explore : Node -> String -> DAWGGraph -> List (Node, String, Bool)
explore node s graph =
  node.outgoing
  |> IntDict.map
      (\k conn ->
          Graph.get k graph
          |> Maybe.map
              (\outnode ->
                  Set.toList conn
                  |> List.map
                    (\(ch, isFinal) ->
                        (outnode, s ++ String.fromChar ch, isFinal == 1)
                    )
              )
      )
  |> IntDict.values
  |> List.filterMap identity
  |> List.concat

processStack : List (Node, String, Bool) -> List String -> DAWGGraph -> List String
processStack stack acc graph =
  case stack of
    [] -> acc
    (n, s, f)::rest ->
      processStack
        (explore n s graph ++ rest)
        (if f then s::acc else acc)
        graph

recognizedWordsFrom : DAWG -> Node -> Result String (List String)
recognizedWordsFrom dawg root =
  case Graph.checkAcyclic dawg.graph of
    Err edge ->
      Err <| "Edge " ++ String.fromInt edge.from ++ "→" ++ String.fromInt edge.to ++ " creates a cycle; this is not a DAWG."
    Ok _ ->
      Ok <| processStack [(root, "", False)] [] dawg.graph

-- Entry point function
recognizedWords : DAWG -> List String
recognizedWords dawg =
  Maybe.map
    (recognizedWordsFrom dawg >> Result.map List.sort >> Result.mapError identity)
    (Graph.get dawg.root dawg.graph)
  |> Maybe.withDefault (Err "Couldn't find the root in the DAWG…!  What on earth is going on?!")
  |> Result.Extra.extract (\e -> [e])

exploreDeterministic : Node -> NodeId -> DAWGGraph -> Result String (List Node)
exploreDeterministic node finalId graph =
  let
    foundNonDeterminism =
      if IntDict.size node.outgoing <= 1 then
        Nothing
      else
        let
          allSets =
            node.outgoing
            |> IntDict.values
            |> List.map (Set.map Tuple.first) -- |> debug_log ("CHECK for #" ++ String.fromInt node.node.id)
          allTransitions =
            List.foldl Set.union Set.empty allSets -- |> debug_log "All transitions"
          duplicate =
            List.foldl
              (\currentSet (result, all) ->
                Set.diff all currentSet -- (debug_log "Checking against" currentSet)
                |> (\diff -> (Set.diff (Set.union currentSet result) all, diff)) -- |> debug_log "now")
              )
              (Set.empty, allTransitions)
              allSets
            |> Tuple.first
        in
          if Set.isEmpty duplicate then
            Nothing
          else
            Just (Set.toList duplicate)
  in
    if IntDict.isEmpty node.outgoing && node.node.id /= finalId then
      -- not strictly about being deterministic but eh, while I'm here, right?
      Err "More than one 'final' node was found, which is incorrect."
    else
      case foundNonDeterminism of
        Nothing -> -- No intersection, or no outgoing values—same difference here.
          node.outgoing
          |> IntDict.map (\k _ -> Graph.get k graph)
          |> IntDict.values
          |> List.filterMap identity
          |> Ok
        Just found ->
          Err ("Transition(s) «" ++ String.fromList found ++ "» from node #" ++ String.fromInt node.node.id ++ " are not deterministic.")

findNonDeterministic : List Node -> NodeId -> DAWGGraph -> Maybe String
findNonDeterministic stack finalId graph =
  case stack of
    [] -> Nothing
    n::rest ->
      case exploreDeterministic n finalId graph of
        Err e -> Just e
        Ok nodes ->
          findNonDeterministic
            (nodes ++ rest)
            finalId
            graph

{-| Same as recognizedWords, but also verifies that the graph is deterministic. -}
verifiedRecognizedWords : DAWG -> List String
verifiedRecognizedWords dawg =
  let
    nonDeterministic =
      case Graph.checkAcyclic dawg.graph of
        Err _ ->
          Just "The DAWG is not acyclic."
        Ok _ ->
          case dawg.final of
            Nothing ->
              Just "The DAWG has no final node."
            Just final ->
              Graph.get dawg.root dawg.graph
              |> Maybe.andThen
                (\root -> findNonDeterministic [root] final dawg.graph)
  in
    case nonDeterministic of
      Nothing ->
        recognizedWords dawg
      Just e ->
        [e]

type alias TupleEdge = (NodeId, NodeId, Transition)
type alias Partition = Set NodeId
type alias HopcroftRecord =
  { w : List Partition -- still to be processed.
  , p : List Partition -- partitions
  }

minimality : DAWG -> List (List Int)
minimality dawg =
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
            refine_for_input t x w p = -- really, the ch is only there for potential debugging.
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
    -- (\l ->
    --   case l of
    --     [] -> Ok dawg
    --     _ -> Err <| "Not minimal; can combine: " ++ (List.map (\s -> "[" ++ (Set.toList s |> List.map String.fromInt |> String.join ", ") ++ "]") l |> String.join "; ")
    -- )
