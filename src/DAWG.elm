module DAWG exposing (..)
import Graph exposing (Graph, NodeContext, NodeId, Node, Adjacency)
import Set exposing (Set)
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe
import Basics.Extra exposing (..)
import IntDict
import Set exposing (Set)
import Result.Extra
import Set.Extra
import Parser as P exposing (Parser, (|.), (|=), succeed, symbol, oneOf, lazy, spaces, end, getChompedString, chompIf, sequence, loop, Step(..))
import Char.Extra
import Dict
import Svg.Attributes exposing (x)
import Svg.Attributes exposing (in_)

type ExprAST
  = M (List ExprAST)
  | A (List ExprAST)
  | V Transition

-- AST for algebraic expressions
-- type Expr
--   = Variable Connection
--   | Add Expr Expr
--   | Multiply Expr Expr

-- -- Parser for algebraic expressions
-- expressionParser : Parser Expr
-- expressionParser =
--   Debug.log ("------------------------------ NEW PARSE BEGINNING") () |> \_ ->
--   succeed identity
--     |= term
--     |. spaces
--     |. end

-- -- Term: Handles addition
-- term : Parser Expr
-- term =
--   let
--     termHelper left =
--       oneOf
--         [ succeed (Loop << Add left)
--             |. spaces
--             |. symbol "+"
--             |. spaces
--             |= lazy (\_ -> factor)
--         , succeed (Done left)
--         ]
--   in
--     succeed identity
--       |= factor
--       |> P.andThen (\left -> loop left termHelper)

-- -- Factor: Handles multiplication
-- factor : Parser Expr
-- factor =
--   succeed identity
--     |= primary -- obtain what will either be a variable or a group.
--     |> P.andThen
--       (\initial -> -- with that variable, proceed.
--           loop initial -- It will be used as the initial value for a (potential) product.
--             (\left -> -- `initial` is the first value here; but if we loop, then this will be a left-associative Multiply.
--               oneOf
--                 [ succeed (Loop << Multiply left) -- Structure & return an indication to continue parsing within Factor.
--                     |. spaces
--                     |. symbol "." -- IF we find a '.', then we take this branch and…
--                     |. spaces
--                     |= lazy (\_ -> primary) -- …grab another primary after the '.'
--                 , succeed (Done left) -- If there is no '.', then this will be hit. Notice that we ONLY return `left` here.
--                 ]
--             )
--       )

-- -- Variable: Parses alphabetic variable names
-- variable : Parser Expr
-- variable =
--   let
--     cParser = \c -> not (Char.Extra.isSpace c || c == '+' || c == '.' || c == '(' || c == ')' || c == '!')
--     updateSetParser finality state s =
--       case String.toList s of
--         '!'::[c] ->
--           updateSetParser finality state (String.fromList [c])
--         [c] ->
--           if Set.member (c, 1) state then
--             succeed <| Loop state
--           else if Set.member (c, 0) state then
--             succeed <| Loop <| (Set.remove (c, 0) >> Set.insert (c, finality)) state
--           else
--             succeed <| Loop <| Set.insert (c, finality) state
--         _ -> P.problem <| "Expected a single character, but got \"" ++ s ++ "\" instead."

--     tParser state =
--       oneOf
--         [ symbol "!!!" |> P.andThen (\_ -> updateSetParser 1 state "!")
--         , symbol "!!" |> P.andThen (\_ -> updateSetParser 0 state "!")
--         , symbol "!"
--             |. P.chompIf cParser
--             |> getChompedString
--             |> P.andThen (updateSetParser 1 state)
--         , chompIf cParser
--           |> getChompedString
--           |> P.andThen (updateSetParser 0 state)
--         , if Set.isEmpty state then
--             P.problem "Expected a variable"
--           else
--             succeed (Done state)
--         ]
--   in
--     loop Set.empty tParser
--     |> map Variable


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

-- exprToString : Expr -> String
-- exprToString e =
--   case e of
--     Variable conn ->
--       connectionToString conn
--     Multiply a b ->
--       "(× " ++ exprToString a ++ " " ++ exprToString b ++ ")"
--     Add a b ->
--       "(+ " ++ exprToString a ++ " " ++ exprToString b ++ ")"

type alias EdgeRecord =
  { start : Int
  , data : Transition
  , end : Int
  }

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

-- newVar : Connection -> (Int, Int) -> (Adjuster, Adjuster, ToDawgRecord)
-- newVar conn state =
--   let
--     k = state.maxId + 1
--   in
--     ( \n r -> { r | connections = IntDict.update k (Maybe.map <| \v -> { v | start = Just (Debug.log ("Adjusting start of " ++ connectionToString v.data ++ ": was " ++ (Maybe.map String.fromInt >> Maybe.withDefault "unset") v.start ++ ", now") n) }) r.connections }
--     , \n r -> { r | connections = IntDict.update k (Maybe.map <| \v -> { v | end = Just (Debug.log ("Adjusting  end  of " ++ connectionToString v.data ++ ": was " ++ (Maybe.map String.fromInt >> Maybe.withDefault "unset") v.start ++ ", now") n) }) r.connections }
--     , { state
--         | maxId = k
--         , connections = IntDict.insert k (EdgeRecord Nothing (debugLog "Nothing↔️Nothing edge initialized for" connectionToString conn) Nothing) state.connections
--       }
--     )

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

type alias ToDawgRecord =
  { edges : List EdgeRecord
  , unused : Int
  }

expressionToDAWG : ExprAST -> (Int, Int) -> ToDawgRecord -> ToDawgRecord
expressionToDAWG expr (start, end) r =
  case expr {- |> debugLog "Processing fragment" exprASTToString-} of
    V data ->
      --println ("Creating #" ++ String.fromInt start ++ "➜#" ++ String.fromInt end ++ " for " ++ transitionToString data)
      { r | edges = EdgeRecord start data end :: r.edges }
    M (x::_ as xs) ->
      foldlSpecial
        (\item (acc, (start_, end_)) ->
          expressionToDAWG item (start_, end_) acc
          |> \acc2 -> ( { acc2 | unused = acc2.unused + 1 }, (end_, acc2.unused) )
        )
        (\item (acc, (start_, _)) ->
          expressionToDAWG item (start_, end) acc
        )
        ( { r | unused = r.unused + 1 }, (start, r.unused))
        xs
      |> Maybe.withDefaultLazy (\_ -> Debug.log "ERROR! M without multiple items!" <| expressionToDAWG x (start, end) r)
    A xs ->
      List.foldl
        (\item acc ->
          expressionToDAWG item (start, end) acc
        )
        { r | unused = r.unused + 1 }
        xs
    M [] ->
      r |> Debug.log "ERROR — found an M value with ZERO items!"

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
      -- |> Debug.log "All heads"
      |> List.filter (not << (Tuple.second >> List.isEmpty))
      |> List.map (\(fst, rest) -> fst::rest)
      -- |> Debug.log "Same-prefix heads"
    replacements =
      samePrefixMuls
      |> List.filterMap
        (\l ->
          case l of
            M (h::_)::_ ->
              Just
                ( Debug.log "Common prefix" h
                , List.concatMap
                    (\m ->
                      case m of
                        M [_, A inner] -> inner
                        M [_, v] -> [v]
                        M (_::rest) -> [M rest]
                        _ -> Debug.log "SHOULD NOT REACH HERE!" [m] -- should not reach here!
                    )
                    l
                )
            _ -> Nothing
        )
      |> List.map
        (\(head, tails) ->
          M [head, A tails]
          -- |> Debug.log "replacement"
        )
  in
    case samePrefixMuls of
      [] -> -- no common prefix collapse. Rely on the caller to simplify within.
        xs
      sames ->
        Debug.log "Rule #9: ☝🏾 subject to: Common Prefix Collapse" () |> \_ ->
        -- remove all the "same-prefix" items from inside.
        List.foldl
          (\same state -> List.foldl List.remove state same)
          xs sames
        -- and replace them with the replacements
        |> (\l -> l ++ replacements)
        |> List.map sortAST
        -- |> Debug.log "Prefix-collapse result"

{-|This applies Rule #10, Finality Primacy, to an “Add” value.

Implemented: ẋ + x.y = ẋ + ẋ.y
…which is slightly different to my notes, but I think it should work.
-}
finalityPrimacy : (List ExprAST) -> List ExprAST
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
      -- |> Debug.log "Heads & single values"
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
      -- |> Debug.log "Found primacy items"
  in
    case to_bump of
      [] -> -- Nothing to promote.  Rely on caller to simplify.
        xs
      _ ->
        Debug.log "Rule #10: ☝🏾 subject to: Finality Primacy" () |> \_ ->
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
                      -- |> Debug.log "After map"
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
        -- (Debug.log "Suffix-collapse; received"
        xs
        -- )
      -- triple-check to ensure that the same Multiply values look the same
      |> List.map sortAST
      -- find the ones which have the same last part
      |> List.gatherEqualsBy
          (\v ->
            case v of
              M sequence -> List.last sequence
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
                  |> Debug.log "Common suffix"
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
          Debug.log "Rule #5: ☝🏾 subject to: Common Suffix Collapse" () |> \_ ->
          -- remove all the "same-suffix" items from inside.
          List.foldl List.remove xs to_remove
          -- and replace them with the replacements
          |> (\l -> l ++ replacements)
          |> List.map sortAST
          -- |> Debug.log "Result of suffix-collapse"

simplify : ExprAST -> ExprAST
simplify e =
  case debugLog "Simplifying" exprASTToString e of
    A (A xs::rest) ->
      -- println "Rephrasing nested +"
      simplify <| A (xs ++ rest)
    M (M xs::rest) ->
      -- println "Rephrasing nested ×"
      simplify <| M (xs ++ rest)
    A xs ->
      -- See if we can apply Rule 9: Common Prefix Collapse
      let
        -- p-collapse, subsumption, s-collapse
        post_simplification =
          ( finalityPrimacy 
          >>commonPrefixCollapse
          >>commonSuffixCollapse
          ) xs
      in
        -- println "In +" |> \_ ->
        if post_simplification /= xs then
          case post_simplification of
            [x] -> simplify x
            _ -> simplify <| A post_simplification
        else
          let
            inner_simplified = List.map simplify post_simplification
          in
            case ( inner_simplified, inner_simplified /= post_simplification ) of
              ( [x], True ) ->
                simplify x
              ( _, True ) ->
                simplify <| A inner_simplified
              ( [x], False ) ->
                x
              ( _, False ) ->
                A inner_simplified
      -- Also see if we can apply Rule 5: Common Suffix Collapse
    M xs -> -- generic.
      -- println "In ×"
      M (List.map simplify xs)
    V x ->
      -- println "In V"
      V x -- base case

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

parseAlgebra : String -> Result (List P.DeadEnd) ExprAST
parseAlgebra =
  P.run expressionParser

coalesceToGraphNodes : List EdgeRecord -> List (Graph.Edge Connection)
coalesceToGraphNodes edges =
  List.gatherEqualsBy
    (\{start, end} -> (start, end))
    (Debug.log "Raw edges" edges)
  |> debugLog "Coalesce: found grouped" (List.filter (Tuple.second >> (not << List.isEmpty)))
  |> List.map
    (\({start, data, end}, xs) ->
        { from = start
        , to = end
        , label = Set.fromList (data::List.map .data xs)
        }
    )

graphEdgeToString : Graph.Edge Connection -> String
graphEdgeToString {from, to, label} =
  "#" ++ String.fromInt from ++ "➜#" ++ String.fromInt to ++ " (" ++ connectionToString label ++ ")"

redirectPrefixGraphNodes : List (Graph.Edge Connection) -> List (Graph.Edge Connection)
redirectPrefixGraphNodes edges =
  List.gatherEqualsBy
    (\{from, label} -> (from, Set.toList label))
    (
    edges
    |> debugLog "Redirect: original edges" (List.map graphEdgeToString)
    )
  |> debugLog "Redirect: gathered edges" ((List.filter (Tuple.second >> (not << List.isEmpty))) >> List.map (\(edge, xs) -> ( graphEdgeToString edge, List.map graphEdgeToString xs)))
  -- any edges that have the same start and the same data
  -- are actually edges that must be joined together into
  -- one. We know that such edges may arise because of
  -- cases such as sy-spw-ow; and because of the order of
  -- simplification (suffix-collapse before prefix-collapse),
  -- this can result in non-determinism at the start of a
  -- split. This is fine, because it is ALWAYS
  -- non-determinism that can be joined! (PROOF??)
  |> List.foldl
      (\(edge, xs) ( acc, redirectDict ) ->
        ( edge :: acc
        , List.foldl
            (\x d ->
              IntDict.insert x.to edge d -- keyed by the old-destination
            )
            redirectDict
            xs
        )
      )
      ( [], IntDict.empty )
  |> debugLog "( accepted graph-edges, initial redirectDict )" (\(a, d) -> (List.map graphEdgeToString a, (IntDict.toList) d))
  -- Now, two things:
  -- 1. if an edge was going FROM a now-redirected value, it should now go FROM the new value.
  -- 2. if an edge was going TO a now-redirected value, it should now go TO the new value.
  |> (\(links, redirectDict) ->
      -- now get the outgoing nodes related to redirectDict.
      ( links
      , IntDict.map
          (\key {to} ->
            List.filterMap (\({from} as outgoing) ->
              -- if from == to then
              --   Just outgoing
              -- else
                if from == key then
                  Just { outgoing | from = to }
                else
                  Nothing
            ) links
          )
          redirectDict
      )
    )
  |> debugLog "( final redirectDict )" (\(_, d) -> (IntDict.toList >> List.map (\(k, v) -> (k, List.map graphEdgeToString v))) d)
  |> (\(links, redirectDict ) ->
      List.concatMap
        (\{from, label, to} ->          
          let
            newStart =
              IntDict.get from redirectDict |> Maybe.andThen List.head |> Maybe.map .from |> Maybe.withDefault from
          in
            IntDict.get to redirectDict
            |> Maybe.map (\outgoingEdges ->
              case outgoingEdges of
                [] ->
                  [ Graph.Edge newStart to label |> debugLog "confirmed edge B" graphEdgeToString ]
                h::_ ->
                  (Graph.Edge from to label |> debugLog "confirmed edge C" graphEdgeToString) ::
                  -- if I am here, then one of the replaced values had this as an input.
                  -- That could cause a loop (SHOULD I CHECK?!!).
                  -- Instead, insert an edge.
                  (List.map (\outgoingEdge ->
                    Graph.Edge to outgoingEdge.to outgoingEdge.label |> debugLog "inserted edge" graphEdgeToString
                  ) outgoingEdges)
            )
            |> Maybe.withDefaultLazy (\_ ->
              [ Graph.Edge newStart to label |> debugLog "confirmed edge A" graphEdgeToString ]
            )
        )
        links
     )
  |> debugLog "Post-redirection" (List.map graphEdgeToString)

fromAlgebra : String -> DAWG
fromAlgebra s =
  s
  |> String.replace " " ""
  |> String.replace "\n" ""
  |> parseAlgebra
  |> Result.map
    (\e ->
      let
        flattened = sortAST e |> debugLog "flattened" exprASTToString
        simplified = simplify flattened |> debugLog "Simplified, round-trip" exprASTToRTString
        graphEdges =
          expressionToDAWG simplified (0, 1) (ToDawgRecord [] 2)
          |> .edges
          -- |> List.map (\{start, end, data} -> Graph.Edge start end (Set.singleton data))
          |> coalesceToGraphNodes
          |> redirectPrefixGraphNodes
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
          |> \dawg -> List.foldl (checkForCollapse) dawg ((max)::(List.map (\n -> n.id) nodes))
        ) maxId
        |> Maybe.withDefault empty
        -- |> debugDAWG "tada"
    )
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

notFinalState : Int
notFinalState = 0
isFinalState : Int
isFinalState = 1

isRoot : NodeId -> Bool
isRoot nodeid =
  nodeid == 0

isConfluence : Connection -> Bool
isConfluence connection =
  Set.size connection > 1

isTerminal : Transition -> Bool
isTerminal (_, f) =
  f == 1

isConfluenceConnection : NodeId -> NodeId -> DAWG -> Bool
isConfluenceConnection node1 node2 dawg =
  Maybe.map2
    (\a b ->
      IntDict.get b.node.id a.outgoing
      |> Maybe.map isConfluence
      |> Maybe.withDefault False
    )
    (Graph.get node1 dawg.graph)
    (Graph.get node2 dawg.graph)
  |> Maybe.withDefault False

isForwardSplit : Node -> Bool
isForwardSplit node =
  IntDict.size node.outgoing > 1

isBackwardSplit : NodeId -> DAWG -> Bool
isBackwardSplit nodeid dawg =
  Graph.get nodeid dawg.graph
  |> Maybe.map (\node -> IntDict.size node.incoming > 1)
  |> Maybe.withDefault False

isLeaf : Node -> Bool
isLeaf node =
  IntDict.size node.outgoing == 0

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

isSingle : Node -> Bool
isSingle node =
  IntDict.size node.outgoing == 1 &&
    List.all (Set.size >> (==) 1) (IntDict.values node.outgoing)
    -- |> Debug.log ("isSingle for #" ++ String.fromInt node.node.id)

isFinalNode : NodeId -> DAWG -> Bool
isFinalNode nodeid dawg =
  dawg.final
  |> Maybe.andThen (flip Graph.get dawg.graph)
  |> Maybe.map (\finalNode -> nodeid == finalNode.node.id)
  |> Maybe.withDefault False

forwardsFollowable : Char -> NodeId -> DAWGGraph -> Maybe Node
forwardsFollowable ch nodeid graph =
  Maybe.andThen
    (\node ->
      IntDict.foldl
        (\k conn state ->
          state
          |> Maybe.orElseLazy
            (\() ->
              if Set.member (ch, 0) conn || Set.member (ch, 1) conn then
                Just k
              else
                Nothing
            )
        )
        Nothing
        node.outgoing
      |> Maybe.andThen (flip Graph.get graph)
    )
    (Graph.get nodeid graph)

backwardsFollowable : Node -> Char -> DAWGGraph -> List Node
backwardsFollowable node ch graph =
  IntDict.foldl
    (\k conn state ->
      if Set.member (ch, 0) conn || Set.member (ch, 1) conn then
        k :: state
      else
        state
    )
    []
    node.incoming
  |> List.filterMap (flip Graph.get graph)

compatibleBackwardsFollowable : NodeId -> Transition -> DAWGGraph -> List Node
compatibleBackwardsFollowable nodeid transition graph =
  Maybe.map
    ( .incoming
      >> IntDict.foldl
        (\k conn state ->
          if Set.member transition conn then
            k :: state
          else
            state
        )
        []
      >> List.filterMap (flip Graph.get graph)
    )
    (Graph.get nodeid graph)
  |> Maybe.withDefault []

{--
  DAWG-modification functions
-}

dawgUpdate : NodeId -> (Node -> Node) -> DAWG -> DAWG
dawgUpdate nodeid f dawg =
  { dawg
    | graph = Graph.update nodeid (Maybe.map f) dawg.graph
  }

tryDawgUpdate : NodeId -> (Node -> Maybe Node) -> DAWG -> DAWG
tryDawgUpdate nodeid f dawg =
  { dawg
    | graph = Graph.update nodeid (Maybe.andThen f) dawg.graph
  }

removeTransitionFromConnection : Transition -> Connection -> Maybe Connection
removeTransitionFromConnection transition connection =
  let
    s = Set.remove transition connection
  in
    if Set.isEmpty s then Nothing
    else Just s

addTransitionToConnection : Transition -> Connection -> Connection
addTransitionToConnection transition connection =
  Set.insert transition connection

updateConnectionWith : Transition -> Connection -> Connection
updateConnectionWith transition conn =
  case transition of
    (ch, 1) ->
      addTransitionToConnection transition conn
      |> Set.remove (ch, 0)
    (ch, _) ->
      if Set.member (ch, 1) conn then
        conn
      else
        addTransitionToConnection transition conn

{-| Merge connections, returning the merged connection.  When a
    transition character occurs in both with different terminal status,
    the transition with the terminal value is preserved.
-}
mergeConnectionWith : Connection -> Connection -> Connection
mergeConnectionWith =
  Set.foldl updateConnectionWith

{-| Don't use this; instead, use connectTo. -}
createOrUpdateOutgoingConnection : Transition -> Node -> NodeId -> IntDict.IntDict Connection
createOrUpdateOutgoingConnection transition from to =
  IntDict.update to
    ( Maybe.map (updateConnectionWith transition)
      >> Maybe.orElseLazy (\() -> Just (Set.singleton transition))
    )
    from.outgoing

createOrUpdateIncomingConnection : Transition -> NodeId -> Node -> Connections
createOrUpdateIncomingConnection transition from to =
  IntDict.update from
    ( Maybe.map (updateConnectionWith transition)
      >> Maybe.orElseLazy (\() -> Just (Set.singleton transition))
    )
    to.incoming

createOrMergeConnections : Connections -> Connections -> Connections
createOrMergeConnections a b =
  IntDict.uniteWith (\_ -> mergeConnectionWith) a b

cloneAndMergeOutgoingConnectionsOfNode : NodeId -> NodeId -> DAWG -> DAWG
cloneAndMergeOutgoingConnectionsOfNode from to dawg = -- from = dr, to = ds
  let
    fromOutgoing_ = Graph.get from dawg.graph |> Maybe.map .outgoing
  in
    Maybe.map
      (\fromOutgoing ->
        dawgUpdate to
          (\toNode -> { toNode | outgoing = createOrMergeConnections fromOutgoing toNode.outgoing })
          dawg
      )
      fromOutgoing_
    |> Maybe.withDefault dawg

cloneAndMergeIncomingConnectionsOfNode : NodeId -> NodeId -> DAWG -> DAWG
cloneAndMergeIncomingConnectionsOfNode from to dawg = -- from = dr, to = ds
  let
    fromIncoming_ = Graph.get from dawg.graph |> Maybe.map .incoming
  in
    Maybe.map
      (\fromIncoming ->
        dawgUpdate to
          (\toNode -> { toNode | incoming = createOrMergeConnections fromIncoming toNode.incoming })
          dawg
      )
      fromIncoming_
    |> Maybe.withDefault dawg

{- Connect to a particular node with a particular transition, returning
    the updated `from` node

  This is a fairly high-level function. It will
  - create the connection if it needs to, and update it otherwise.
  - ensure that if the transition is final, there isn't a competing
    non-final transition.
-}
{-| Use this if you absolutely need to, but you probably want createTransitionBetween -}
connectTo : NodeId -> Transition -> Node -> Node
connectTo to transition from =  
  { from
    | outgoing = createOrUpdateOutgoingConnection transition from to
  }

{-| Connect from a particular node with a particular transition, returning
    the updated `to` node

  This is a fairly high-level function. It will
  - create the connection if it needs to, and update it otherwise.
  - ensure that if the transition is final, there isn't a competing
    non-final transition.
-}
connectFrom : NodeId -> Transition -> Node -> Node
connectFrom from transition to =
  { to
    | incoming = createOrUpdateIncomingConnection transition from to
  }


disconnectFrom : NodeId -> Node -> Node
disconnectFrom to from =
  { from
    | outgoing = IntDict.remove to (Debug.log ("Before disconnection of " ++ String.fromInt to) from.outgoing) |> Debug.log "After disconnection"
  }

obtainConnectionFrom : NodeId -> Node -> Maybe (Node, NodeId, Connection)
obtainConnectionFrom connectionDestination from =
  from.outgoing
  |> IntDict.get connectionDestination
  |> Maybe.map (\conn -> (from, connectionDestination, conn))

redirectConnectionTo : NodeId -> Maybe (Node, NodeId, Connection) -> Maybe Node
redirectConnectionTo to maybeRedirect =
  maybeRedirect
  |> Maybe.map
    (\(from, old, conn) ->
        { from
          | outgoing =
              IntDict.remove old from.outgoing
              |> IntDict.insert to conn
        }
    )

mergeConnectionTo : NodeId -> Maybe (Node, NodeId, Connection) -> Maybe Node
mergeConnectionTo to maybeRedirect =
  maybeRedirect
  |> Maybe.map
    (\(from, _, conn) ->
        { from
          | outgoing =
              IntDict.uniteWith
                (\_ a b -> Set.foldl (updateConnectionWith) a b)
                (IntDict.singleton to conn)
                from.outgoing
        }
    )

createNewSuccessorNodeWithConnection : Connection -> NodeId -> DAWG -> (DAWG, NodeId)
createNewSuccessorNodeWithConnection connection srcNode dawg =
  let
    newNode =
      { node = Node (dawg.maxId + 1) ()
      , incoming = IntDict.singleton srcNode connection
      , outgoing = IntDict.empty
      }
  in
    ( { dawg
        | graph = Graph.insert newNode dawg.graph
        , maxId = dawg.maxId + 1
      }
    , newNode.node.id
    )

{-| Create a transition to a new node, returning the DAWG and the new node.
-}
createNewSuccessorNode : Transition -> NodeId -> DAWG -> (DAWG, NodeId)
createNewSuccessorNode transition srcNode dawg =
  createNewSuccessorNodeWithConnection (Set.singleton transition) srcNode dawg

{-| Create a transition to a new node, returning the DAWG and the new node.
-}
createNewPrecursorNode : Transition -> NodeId -> DAWG -> (DAWG, NodeId)
createNewPrecursorNode transition destNode dawg =
  let
    newNode =
      { node = Node (dawg.maxId + 1) ()
      , outgoing = IntDict.singleton destNode (Set.singleton transition)
      , incoming = IntDict.empty
      }
  in
    ( { dawg
        | graph = Graph.insert newNode dawg.graph
        , maxId = dawg.maxId + 1
      }
    , newNode.node.id
    )

{-| Internal function.  Merges a series of transitions into the graph prefix.

ASSUMPTIONS: the last Transition is terminal; all others are non-terminal.
-}
prefixMerge : List Transition -> NodeId -> DAWG -> DAWG
prefixMerge transitions currentNode dawg =
  case println ("When currentNode = " ++ String.fromInt currentNode ++ ", transitions = " ++ transitionsToString transitions) transitions of
    [] -> -- we are at the end.
      dawg

    (w, isFinal)::transitions_remaining ->
      case forwardsFollowable w currentNode dawg.graph of
        Just someNode ->
          if isFinal == 1 then
            println ("[Prefix 2.1.1] Created/updated terminal transition to #" ++ String.fromInt someNode.node.id ++ ".  Updating existing transition to be final & exiting unconditionally.")
            createTransitionBetween (w, 1) currentNode someNode.node.id dawg
          else
            if isFinalNode someNode.node.id dawg || isBackwardSplit someNode.node.id dawg || isConfluenceConnection currentNode someNode.node.id dawg then
              println ("[Prefix 2.2.2] Graph node #" ++ String.fromInt someNode.node.id ++ " is backward-split, or the #" ++ String.fromInt currentNode ++ " → #" ++ String.fromInt someNode.node.id ++ " connection is a confluence, or #" ++ String.fromInt someNode.node.id ++ " is the final-node.  Going to suffix-merging.")
              mergeSuffixes (List.reverse transitions) currentNode dawg
            else
              println ("[Prefix 2.2.3] Graph node #" ++ String.fromInt someNode.node.id ++ " is single.  Continuing prefix-merge.")
              prefixMerge transitions_remaining someNode.node.id dawg
        Nothing -> -- there is nothing to follow forward.  Start merging from the other side.
          mergeSuffixes (List.reverse transitions) currentNode dawg

mergeNodes : (Adjacency Connection, NodeId) -> NodeId -> DAWG -> DAWG
mergeNodes (adjacency, oldDestination) newDestination dawg =
  { dawg
    | graph =
        IntDict.foldl
          (\sourceNodeId _ graph ->
              Graph.update sourceNodeId
                (Maybe.andThen (obtainConnectionFrom oldDestination >> mergeConnectionTo newDestination))
                graph
          )
          dawg.graph
          adjacency
  }

redirectNodes : (Adjacency Connection, NodeId) -> NodeId -> DAWG -> DAWG
redirectNodes (adjacency, oldDestination) newDestination dawg =
  { dawg
    | graph =
        IntDict.foldl
          (\sourceNodeId _ graph ->
              Graph.update sourceNodeId
                (Maybe.andThen (obtainConnectionFrom oldDestination >> redirectConnectionTo newDestination))
                graph
          )
          dawg.graph
          adjacency
  }

replaceNode : NodeId -> NodeId -> DAWG -> DAWG
replaceNode oldid newid dawg =
  cloneAndMergeIncomingConnectionsOfNode oldid newid dawg
  |> cloneAndMergeOutgoingConnectionsOfNode oldid newid
  |> \dawg_ -> { dawg_ | graph = Graph.remove oldid dawg_.graph }

{-| Take all the previous incoming-connections of the old final-node and
    redirect them to the new final-node.
-}
redirectNodesToFinal : (Adjacency Connection, NodeId) -> DAWG -> DAWG
redirectNodesToFinal redirection dawg =
  dawg.final
  |> Maybe.map (flip (redirectNodes redirection) dawg)
  |> Maybe.withDefault dawg

{-| Combine the `incoming` of nodes `kill` and `keep`; delete `kill` and retain only `keep`.
-}
combineNodes : NodeId -> NodeId -> DAWG -> DAWG
combineNodes kill keep dawg =
  cloneAndMergeIncomingConnectionsOfNode kill keep dawg
  |> \dawg_ -> { dawg_ | graph = Graph.remove kill dawg_.graph }
  --|> debugDAWG "After"

checkForCollapse : NodeId -> DAWG -> DAWG
checkForCollapse nodeid dawg =
  (Graph.get nodeid dawg.graph)
  |> Maybe.map
    (\node ->
      let
        sets =
          node.incoming |> IntDict.toList
          |> List.gatherEqualsBy (Tuple.first >> (flip allTransitionsFrom dawg))
          |> List.filter (Tuple.second >> (not << List.isEmpty))
      in
        case sets of
          [] -> dawg
          ((k, s), _)::_ ->
            -- … yeah this isn't the MOST effecient, but let's see how it goes.
            -- println ("[Collapsing] Collapsing #" ++ String.fromInt nodeid ++ " backwards")
            Set.toList s |> List.head
            |> Maybe.map (\transition -> createTransitionBetween transition k nodeid dawg)
            |> Maybe.withDefault dawg
    )
  |> Maybe.withDefault dawg

{-| Create a connection between two existing nodes, with the specified
    transition.  If such a connection already exists, it is updated with
    the specified transition.  If there is an incoming connection to the
    destination, and it can be merged, then it is merged and we then check
    OUR incoming nodes to see whether a merge is possible.
-}
createTransitionBetween : Transition -> NodeId -> NodeId -> DAWG -> DAWG
createTransitionBetween transition from to dawg =
  let
    to_ = Graph.get to dawg.graph
    from_ = Graph.get from dawg.graph
    from_outgoing = -- amend the outgoing to what it _would_ be
      Maybe.map (\node -> createOrUpdateOutgoingConnection transition node to |> IntDict.toList) from_
      --|> Debug.log "cmp. base"
    proposedConnection =
      Maybe.map (\f ->
        IntDict.get to f.outgoing
        |> Maybe.map (updateConnectionWith transition)
        |> Maybe.withDefaultLazy (\() -> Set.singleton transition)
      ) from_
    -- now have a quick look at the "to"'s incoming.  Is this connection already there?  If so, what is its source?
    existingConnections =
      Maybe.andThen2 (\t proposedConnection_ ->
        IntDict.filter (\_ v -> v == proposedConnection_) t.incoming
        |> IntDict.keys -- |> Debug.log ("potentially combinables with #" ++ String.fromInt from)
        -- now filter these, so that only the nodes that are combinable are left.
        -- These are the nodes where the outgoing connections are identical.
        |> List.filterMap
            (\e ->
              if from == e then
                Nothing -- this can happen if you try to put in the same word that's already in.
              else
                Maybe.andThen2
                  (\f_o other ->
                    if f_o == {-Debug.log "cmp. with"-} (IntDict.toList other.outgoing) then
                      Just (from, e)
                    else
                      Nothing
                  )
                  from_outgoing
                  (Graph.get e dawg.graph)
            )
        |> \l -> if List.isEmpty l then Nothing else Just l
      ) to_ proposedConnection --|> Debug.log "combinables"
  in
    -- Debug.log ("Request to connect #" ++ String.fromInt from ++ " → #" ++ String.fromInt to ++ " with transition " ++ transitionToString transition) () |> \_ ->
    case existingConnections of
      Nothing ->
        dawgUpdate from (connectTo to transition) dawg
      Just [] -> -- yeah, this one should be impossible to reach, but 🤷 it keeps the compiler quiet.
        dawgUpdate from (connectTo to transition) dawg
      Just combinables -> -- oh my.  Well, what can we do about it?
        -- the first element of these tuples is the `from` that we've been passed.

        -- I'm guaranteed to have a valid connection in this case.
        combinables
        |> List.foldl
          (\(a, b) ->
            combineNodes b a
            >> debugDAWG ("Collapsed #" ++ String.fromInt a ++ "|#" ++ String.fromInt b ++ ", both →#" ++ String.fromInt to ++ " via " ++ transitionToString transition ++ ", into one.")
          )
          (dawgUpdate from (connectTo to transition)
          (debugDAWG "Pre-collapse" dawg))
        |> checkForCollapse from

{-| Unconditionally creates a chain, using all the specified transitions and
    creating nodes as necessary, that begins at `from` and terminates at `to`.
-}
createTransitionChainBetween : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
createTransitionChainBetween transitions from to dawg =
  case transitions of
    [] ->
      dawg
    [t] ->
      createTransitionBetween t from to dawg
    _ -> -- we know that `rest` is NOT [] here; otherwise, it'd be caught at [t]
      Maybe.map2
        (\(lastTransition, rest) fromNode ->
          List.foldl
            (\t (dawg_, prevNode) ->
              createNewSuccessorNode t prevNode dawg_
            )
            (dawg, fromNode.node.id)
            rest
          |> \(dawg_, prevNode) -> createTransitionBetween lastTransition prevNode to dawg_
        )
        (List.unconsLast transitions)
        (Graph.get from dawg.graph)
      |> Maybe.withDefaultLazy (\() -> debugDAWG "👽 BUG!! 👽 in createTransitionChainBetween." dawg)

-- returns the nodeId we stopped on and the number of transitions we consumed
followSuffixes : List Transition -> List Transition -> NodeId -> NodeId -> DAWG -> (NodeId, Int)
followSuffixes transitions followed prefixEnd currentNode dawg =
  if False then -- mark as True to totally remove all suffix-following (just to experiment).
    --createChain (List.reverse transitions) prefixEnd currentNode followed dawg
    ( currentNode, 0 ) 
  else
    println ("[Suffix 2] Suffixes to follow from #" ++ String.fromInt currentNode ++ ", moving back towards #" ++ String.fromInt prefixEnd ++ ": " ++ transitionsToString transitions) |> \_ ->
    case transitions of
      [] ->
        ( currentNode, List.length followed )
        -- dawg
      (w, isFinal)::transitions_remaining ->
        case compatibleBackwardsFollowable currentNode (w, isFinal) dawg.graph of
          [] ->
            ( currentNode, List.length followed )
            -- createChain (List.reverse transitions) prefixEnd currentNode followed dawg
          candidateBackwardNodes ->
            case List.partition isSingle candidateBackwardNodes of
              ([single], _) ->
                case transitions_remaining of
                  [] ->
                    if prefixEnd == single.node.id then
                      println ("[Suffix 3.1] No more transitions to follow; ending as expected at prefix-node #" ++ String.fromInt prefixEnd ++ ".")
                      ( currentNode, List.length followed )
                      -- dawg
                    else
                      ( currentNode, List.length followed )
                      -- createChain (List.reverse transitions) prefixEnd currentNode followed dawg
                  _ ->
                    if prefixEnd == single.node.id then
                      println ("[Suffix 2.1.1] Word is longer than graph; we must add in nodes.")
                      ( currentNode, List.length followed )
                      -- createChain (List.reverse transitions) prefixEnd currentNode followed dawg
                    else
                      println ("[Suffix 2.2.1] Single backwards-node (#" ++ String.fromInt single.node.id ++ ") found for " ++ transitionToString (w, isFinal) ++ ".  Following back.")
                      followSuffixes transitions_remaining ((w, isFinal)::followed) prefixEnd single.node.id dawg
              ([], dcdf) ->
                Debug.log ("[Suffix 2.2.2] Confluence/forward-split found for backwards-split from " ++ String.fromInt currentNode ++ " via " ++ transitionToString (w, isFinal) ++ "; stopping backtrack here.")
                  dcdf
                |> \_ ->
                ( currentNode, List.length followed )
                  -- createChain (List.reverse transitions) prefixEnd currentNode followed dawg
              (possible, _) ->
                println ("[Suffix] Multiple backwards nodes found for " ++ transitionToString (w, isFinal) ++ " from #" ++ String.fromInt currentNode ++ ".  Should they have been merged??  In any event, I'll pick the longest suffix I can.")
                List.map (\n -> followSuffixes transitions_remaining ((w, isFinal)::followed) prefixEnd n.node.id dawg) possible
                |> List.maximumBy Tuple.second
                |> Maybe.withDefault ( currentNode, List.length followed )
                  -- dawg

addFinalNode : DAWG -> (Node, DAWG)
addFinalNode dawg =
  let
    newFinalNode =
      { node = Node (dawg.maxId + 1) ()
      , incoming = IntDict.empty
      , outgoing = IntDict.empty
      }
  in
    ( newFinalNode
    , { dawg
        | graph = Graph.insert newFinalNode dawg.graph
        , maxId = dawg.maxId + 1
        , final = Just <| dawg.maxId + 1
      }
    )

{-| Find the outgoing node from the given node for a given transition-character. -}
outgoingConnectionWith : Char -> NodeId -> DAWG -> Maybe (Node, Transition)
outgoingConnectionWith w nodeid dawg =
  Graph.get nodeid dawg.graph
  |> Maybe.andThen
    (\node ->
      IntDict.filter
        (\_ v -> Set.member (w, 0) v || Set.member (w, 1) v)
        node.outgoing
      |> IntDict.toList
      |> List.filterMap
        (\(k, conn) ->
          Graph.get k dawg.graph
          |> Maybe.map (\existing -> (existing, if Set.member (w, 0) conn then (w, 0) else (w, 1)))
        )
      |> List.head
    )

maxTerminality : Transition -> Transition -> Transition
maxTerminality (ch, t0) (_, t1) =
  if t0 > t1 then (ch, t0) else (ch, t1)

transitionMatching : Char -> Connection -> Bool
transitionMatching ch connection =
  Set.member (ch, 0) connection || Set.member (ch, 1) connection

getConnection : NodeId -> NodeId -> DAWG -> Maybe Connection
getConnection from to dawg =
  Graph.get from dawg.graph
  |> Maybe.andThen (\{ outgoing } -> IntDict.get to outgoing)

duplicateOutgoingConnectionsExcluding : NodeId -> NodeId -> NodeId -> DAWG -> DAWG
duplicateOutgoingConnectionsExcluding excluded from to dawg =
  Maybe.map2
    (\fromNode toNode ->
      { dawg
        | graph =
            Graph.update to
              (\_ -> Just <|
                { toNode
                  | outgoing =
                      createOrMergeConnections
                        (IntDict.remove excluded fromNode.outgoing)
                        toNode.outgoing
                }
              )
              dawg.graph
      }
    )
    (Graph.get from dawg.graph)
    (Graph.get to dawg.graph)
  |> Maybe.withDefaultLazy (\() -> debugDAWG ("👽 BUG 👽 in duplicateOutgoingConnectionsExcluding? Could not find either/both of #" ++ String.fromInt from ++ " or #" ++ String.fromInt to) dawg)

duplicateOutgoingConnectionsExcludingTransitions : List Transition -> NodeId -> NodeId -> DAWG -> DAWG
duplicateOutgoingConnectionsExcludingTransitions transitions from to dawg =
  let
    to_remove = Set.fromList transitions
  in
  Maybe.map2
    (\fromNode toNode ->
      { dawg
        | graph =
            Graph.update to
              (\_ -> Just <|
                { toNode
                  | outgoing =
                      ( fromNode.outgoing
                        |> IntDict.map (\_ s -> Set.diff s to_remove)
                        |> IntDict.filter (\_ -> not << Set.isEmpty) -- kill invalid connections!
                      )
                      |> createOrMergeConnections toNode.outgoing
                }
              )
              dawg.graph
      }
    )
    (Graph.get from dawg.graph)
    (Graph.get to dawg.graph)
  |> Maybe.withDefaultLazy (\() -> debugDAWG ("👽 BUG 👽 in duplicateOutgoingConnectionsExcludingTransition? Could not find either/both of #" ++ String.fromInt from ++ " or #" ++ String.fromInt to) dawg)

duplicateOutgoingConnections : NodeId -> NodeId -> DAWG -> DAWG
duplicateOutgoingConnections from to dawg =
  Maybe.map2
    (\fromNode toNode ->
      let
        merged = createOrMergeConnections fromNode.outgoing toNode.outgoing
      in
      { dawg
        | graph = Graph.update to (\_ -> Just { toNode | outgoing = merged }) dawg.graph
      }
      -- I _could_ include this.  But it has nasty consequences for later duplicate checks
      -- because the node might not exist!!  Better to do this in the caller, after all
      -- duplicate checks are completed?
      -- |> withOutgoingNodesOf [to] checkForCollapse
    )
    (Graph.get from dawg.graph)
    (Graph.get to dawg.graph)
  |> Maybe.withDefaultLazy (\() -> debugDAWG ("👽 BUG 👽 in duplicateOutgoingConnections? Could not find either/both of #" ++ String.fromInt from ++ " or #" ++ String.fromInt to) dawg)

type alias CurrentNodeData =
  { chosenTransition : Transition -- tC in text. Maximum-terminality among available options.
  , otherOutgoingConnectionsOfPrefix : Connections
  , id : NodeId -- d' in text
  , completeIncoming : Connections
  , isFinal : Bool
  }

type alias LinkingForwardData =
  { graphPrefixEnd : NodeId -- dP in text
  -- there is always precisely one connection from graphPrefixEnd (dP) to currentNode (d')
  , lastConstructed : Maybe NodeId -- c in text
  , graphSuffixEnd : NodeId -- dS in text
  , suffixPath : List Transition
  }

{-| Updates incoming nodes to exclude a particular transition to a target.
    If no incoming nodes remain, then the connection itself is removed.
-}
incomingWithoutTransitionFrom : Transition -> NodeId -> Connections -> Connections
incomingWithoutTransitionFrom (w, _) target incoming =
  IntDict.update target
    (Maybe.andThen <| (\conn ->
      Set.remove (w, 0) conn
      |> Set.remove (w, 1)
      |> \removed ->
          if Set.isEmpty removed then Nothing
          else Just removed
    ))
    incoming


{-| A helper function for `createForwardsChain` -}
getForwardNodeData : NodeId -> Transition -> DAWG -> Maybe CurrentNodeData
getForwardNodeData nodeid (w, isFinal) dawg_ =
  Graph.get nodeid dawg_.graph
  |> Maybe.andThen
    (\{ outgoing } ->
      IntDict.filter (\_ -> transitionMatching w) outgoing
      |> IntDict.toList
      |> List.head
      |> Maybe.andThen
        (\(k, v) ->
          Maybe.map
            (\d_ ->
              let
                t = if Set.member (w, 0) v then (w, 0) else (w, 1)
              in
                { chosenTransition = maxTerminality t (w, isFinal)
                , id = k
                , otherOutgoingConnectionsOfPrefix =
                    IntDict.remove k d_.outgoing
                , completeIncoming = d_.incoming
                , isFinal = IntDict.isEmpty d_.outgoing
                }
            )
            (Graph.get k dawg_.graph)
        )
    )

connectsToFinalNode : NodeId -> DAWG -> Maybe NodeId
connectsToFinalNode nodeid dawg =
  dawg.final
  |> Maybe.andThen
    (\final ->
      Graph.get nodeid dawg.graph
      |> Maybe.andThen
        (\node ->
          IntDict.get final node.outgoing
          |> Maybe.map (\_ -> final)
        )
    )

type SplitPathResult
  = SplitOff NodeId
  | Straight NodeId

{-| Accepts a `from` and `to` node, which MUST already be Connected by one or more transitions
   including `transition`.  Accepts a continuation to pass forward to.
   
   - If `from`→`to` is a confluence, it will be split off and the new node will be passed forward.
   - If `to` is a backwards-split, it will be split off and the new node will be passed forward.
   - Otherwise, `to` will be passed forward verbatim.
-}
splitAwayPathThenContinue : NodeId -> NodeId -> Transition -> (SplitPathResult -> DAWG -> DAWG) -> DAWG -> DAWG
splitAwayPathThenContinue from to transition continuation dawg =
  if isConfluenceConnection from to dawg then
    println ("Found #" ++ String.fromInt from ++ "→#" ++ String.fromInt to ++ " confluence " ++ (getConnection from to dawg |> Maybe.map connectionToString |> Maybe.withDefault "ERROR!NoConnection!") ++ ".  Chosen transition is " ++ transitionToString transition ++ ".")
    -- remove the transition from the confluence node
    dawgUpdate to (\d_ -> { d_ | incoming = incomingWithoutTransitionFrom transition from d_.incoming }) dawg
    |> createNewSuccessorNode transition from
    |> \(dawg_, successor) -> continuation (SplitOff successor) dawg_
  else if isBackwardSplit to dawg then
    -- by now, we are sure that we DON'T have a confluence.  This makes the logic easier!
    println ("Found backward-split centered on #" ++ String.fromInt to ++ ".  Chosen transition is " ++ transitionToString transition ++ ".")
    -- remove the transition from the backward-split
    dawgUpdate to (\d_ -> { d_ | incoming = incomingWithoutTransitionFrom transition from d_.incoming }) dawg
    |> createNewSuccessorNode transition from
    |> \(dawg_, successor) -> continuation (SplitOff successor) dawg_
  else
    continuation (Straight to) dawg

withSplitPath : NodeId -> NodeId -> Transition -> (NodeId -> DAWG -> DAWG) -> DAWG -> DAWG
withSplitPath from to transition continuation dawg =
  splitAwayPathThenContinue from to transition
    (\splitResult dawg_ ->
      case splitResult of
        Straight _ ->
          debugDAWG "👾 BUG 👾 Impossible path hit." empty
        SplitOff c -> continuation c dawg_
    )
    dawg

allTransitionsFrom : NodeId -> DAWG -> Set Transition
allTransitionsFrom nodeid dawg =
  Graph.get nodeid dawg.graph
  |> Maybe.map
    (\node -> IntDict.values node.outgoing |> List.foldl Set.union Set.empty)
  |> Maybe.withDefault Set.empty

getTransitionFrom : NodeId -> Char -> DAWG -> Maybe Transition
getTransitionFrom source w dawg =
  allTransitionsFrom source dawg
  |> \s ->
    if Set.member (w, 1) s then
      Just (w, 1)
    else if Set.member (w, 0) s then
      Just (w, 0)
    else
      Nothing

removeTransitionBetweenNodes : NodeId -> NodeId -> Transition -> DAWG -> DAWG
removeTransitionBetweenNodes source destination transition dawg =
  dawgUpdate source
    (\node ->
      { node
        | outgoing =
            IntDict.update destination
              (Maybe.andThen (removeTransitionFromConnection transition))
              node.outgoing
      }
    )
    dawg

addTransitionBetweenNodes : NodeId -> NodeId -> Transition -> DAWG -> DAWG
addTransitionBetweenNodes source destination transition dawg =
  dawgUpdate source
    (\node ->
      { node
        | outgoing =
            IntDict.update destination
              (Maybe.map (addTransitionToConnection transition)
              >> Maybe.orElseLazy (\() -> Just <| Set.singleton transition)
              )
              node.outgoing
      }
    )
    dawg

{-| Returns conflicting transitions betweeen 'a' and 'b' -}
outgoingConflict : NodeId -> NodeId -> DAWG -> List Transition
outgoingConflict a b dawg =
  let
    a_out = allTransitionsFrom a dawg
    b_out = allTransitionsFrom b dawg
    a_chars = Set.map Tuple.first a_out
    b_chars = Set.map Tuple.first b_out
  in
    Set.intersect a_chars b_chars
    |> Set.toList
    |> List.map
      (\w ->
        maxTerminality
          (if Set.member (w, 0) a_out then (w, 0) else (w, 1))
          (if Set.member (w, 0) b_out then (w, 0) else (w, 1))
      )

switchTransitionToNewPath : Transition -> LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
switchTransitionToNewPath transition linking d dawg =
  case outgoingConflict d.id linking.graphSuffixEnd dawg of
    [] ->
      println ("Switching an existing transition (" ++ transitionToString transition ++ "), originating at #" ++ String.fromInt linking.graphPrefixEnd ++ ", from #" ++ String.fromInt d.id ++ " to #" ++ String.fromInt linking.graphSuffixEnd ++ ".")
      createNewSuccessorNode transition linking.graphPrefixEnd dawg
      |> \(dawg_, successor) ->
        removeTransitionBetweenNodes linking.graphPrefixEnd d.id transition dawg_
        -- |> addTransitionBetweenNodes linking.graphPrefixEnd linking.graphSuffixEnd transition
        |> duplicateOutgoingConnections linking.graphSuffixEnd successor
        |> duplicateOutgoingConnections d.id successor
        |> withOutgoingNodesOf [linking.graphSuffixEnd, d.id] checkForCollapse
    transitions -> -- e.g. teste-ne-neste
      -- shift the window forward & reconsider?  If I try to do a direct link, I will end up with
      -- a nondeterministic graph.
      case linking.suffixPath of
        [] -> debugDAWG "[M] Impossible case." empty
        h::rest ->
          Debug.log ("Conflicting transitions (" ++ transitionsToString transitions ++ ") of #" ++ String.fromInt d.id ++ " and #" ++ String.fromInt linking.graphSuffixEnd ++ " prevent a direct switch.  Shifting window & retrying instead.") () |> \_ ->
          case forwardsFollowable (Tuple.first h) linking.graphSuffixEnd dawg.graph of
            Nothing ->
              debugDAWG "[M-2] Impossible case." empty
            Just newSuffix ->
              traceForwardChainTo transition [h]
                { linking | graphSuffixEnd = newSuffix.node.id, suffixPath = rest }
                d dawg

connectIndependentPaths : Transition -> List Transition -> LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
connectIndependentPaths transition rest linking d dawg =
  -- e.g. a-ab
  -- I connect directly to the final node, AND `d` is the final node.  The path has not
  -- split, so I know that the prefix is exact.  If this is the final transition, then
  -- whether this is straight, confluence, or backward-split does NOT matter: the "suffix"
  -- is correct anyway because the suffix is final.

  -- however, if /w/ is longer than the graph, then there will be are additional transitions.
  -- then we must split any confluence/backward-split, and rejoin at the final node after.
  case rest of
    [] -> 
      -- The word in the graph is actually a prefix of the word that is being added.
      -- Within the graph, there is a suffix that will correctly extend the word.
      -- We must preserve both the word in the graph AND extend it.
      let
        outgoingIsSuperset : NodeId -> NodeId -> Bool
        outgoingIsSuperset newDest currentDest =
          allTransitionsFrom newDest dawg
          |> Set.Extra.isSupersetOf (allTransitionsFrom currentDest dawg)
      in
        if isConfluenceConnection linking.graphPrefixEnd d.id dawg && not (outgoingIsSuperset linking.graphSuffixEnd d.id) then
          -- Alright!  This looks to be a valid test for when we must create a chain INSTEAD of joining.
          -- The essential point is this: if, by switching the transition, we would affect an existing
          -- graph-word, then we must create a chain instead.  And how do we know if we would affect an
          -- existing graph word?  Well, the outgoing connections of the suffix must be a superset of the
          -- outgoing connections of the `d` node (which we are currently looking at).  If they are NOT,
          -- then we would be affecting an existing graph word.
          case outgoingConflict d.id linking.graphSuffixEnd dawg of
            [] ->
              -- e.g. ax-gx-kp-gp
              println ("Switching to meet the independent suffix would destroy an existing graph word. Cloning outgoing connections to a new " ++ transitionToString transition ++ " node instead.")
              withSplitPath linking.graphPrefixEnd d.id transition
                (\c ->
                  duplicateOutgoingConnections linking.graphSuffixEnd c
                  >> duplicateOutgoingConnections d.id c
                )
                dawg
            transitions -> -- e.g. teste-ne-neste
              -- shift the window forward & reconsider?  If I try to do a direct link, I will end up with
              -- a nondeterministic graph.
              case linking.suffixPath of
                [] -> debugDAWG "[O] Impossible case." empty
                h::rest_ -> -- e.g. axax-bx-cx-cxax
                  -- if there is an existing prefix/suffix pair that feeds forward with this, then let's shift the window and re-check.
                  Debug.log ("Conflicting transitions (" ++ transitionsToString transitions ++ ") of #" ++ String.fromInt d.id ++ " and #" ++ String.fromInt linking.graphSuffixEnd ++ " prevent a direct switch.  Shifting window & retrying instead.") () |> \_ ->
                  case forwardsFollowable (Tuple.first h) linking.graphSuffixEnd dawg.graph of
                    Nothing ->
                      debugDAWG "[O-2] Impossible case." empty
                    Just newSuffix ->
                      traceForwardChainTo transition (rest ++ [h])
                        { linking | graphSuffixEnd = newSuffix.node.id, suffixPath = rest_ }
                        d dawg

        else if d.isFinal == False && ([linking.graphPrefixEnd, linking.graphSuffixEnd] |> List.map (flip Graph.get dawg.graph) |> List.all (Maybe.map (\n -> IntDict.member d.id n.outgoing) >> Maybe.withDefault False)) then
          -- e.g. ttal-tyal-ntl-ntal
          println "Not enough space to enact a join; must create an ancillary node to compensate."
          -- `dS`-`d` already exists.  (the if-statement checks for this).
          -- We will redirect it later, but first, we will create the node
          -- that it will be redirect _to_.
          -- We will now create a new node from `d`.
          getConnection linking.graphSuffixEnd d.id dawg
          |> Maybe.map
            (\ds_d_connection ->
              createNewSuccessorNodeWithConnection ds_d_connection {-root-}d.id dawg
              |> \(dawg_, successor) ->
                println ("Created new redirection node #" ++ String.fromInt successor ++ ", linked from #" ++ String.fromInt d.id)
                -- now, redirect ds_d_connection to meet the successor node.
                tryDawgUpdate linking.graphSuffixEnd
                  (obtainConnectionFrom d.id >> redirectConnectionTo successor)
                  dawg_
                |> debugDAWG ("Redirected #" ++ String.fromInt linking.graphSuffixEnd ++ "→#" ++ String.fromInt d.id ++ " connection to #" ++ String.fromInt successor)
                -- lastly, replicate all of the outgoing connections from `d` to `successor`,
                -- EXCEPT for the connection that we created to `successor`.
                |> duplicateOutgoingConnectionsExcluding successor d.id successor
                |> debugDAWG ("Duplicated outgoing connections from #" ++ String.fromInt d.id ++ " to #" ++ String.fromInt successor)
            )
          |> Maybe.withDefaultLazy (\() -> debugDAWG "👽 BUG!! 👽 in connectIndependentPaths, second case" dawg)
        else
          -- This is a valid test for when we must create a chain INSTEAD of joining.
          -- The essential point is this: if, by switching the transition, we would affect an existing
          -- graph-word, then we must create a chain instead.
          -- println ("d = #" ++ String.fromInt d.id ++ ", dP = #" ++ String.fromInt linking.graphPrefixEnd ++ ", dS = #" ++ String.fromInt linking.graphSuffixEnd ++ ".")

          
          -- println ("There is an existing suffix for this word elsewhere in the graph. Connecting the path to that suffix.")
          switchTransitionToNewPath transition linking d dawg
    _ ->
      splitAwayPathThenContinue linking.graphPrefixEnd d.id transition
        (\splitResult dawg_ ->
          case splitResult of
            Straight g -> -- e.g. a-ab
              -- We are still on a straight path past the final; I can extend straight out.
              println ("[J] On a straight prefix #" ++ String.fromInt g ++ ", past the final; extending straight to new final & redirecting.")
              addFinalNode dawg_
              |> \(newFinal, dawg__) ->
                  createTransitionChainBetween rest g newFinal.node.id dawg__
                  |> redirectNodesToFinal (IntDict.remove linking.graphPrefixEnd d.completeIncoming, d.id)
            SplitOff c -> -- e.g. xa-y-yaa
              println ("[J] On an alt-path now (#" ++ String.fromInt c ++ "), continuing to follow, upcoming transitions are: " ++ transitionsToString rest)
              createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just c } dawg_
        )
        dawg

{-| In a case such as teve-ceve-ce , the graph-suffix starts with the same
    character that the graph-prefix ends with (i.e., 'e', in this case).  This
    can also happen for longer runs (see ayxpayx… in tests).  In that case,
    we should consider moving along the prefix rather than connecting to the
    suffix.  This function checks for whether a repeated suffix exists, and
    should only be called once we have reached the end of the transitions.
-}
prefixRepeatsSuffix : List Transition -> NodeId -> NodeId -> DAWG -> Bool
prefixRepeatsSuffix suffixTransitions_ lastPrefix_ firstSuffix_ dawg =
  let
    prefixRepeatsSuffixReal : List Transition -> NodeId -> NodeId -> Bool
    prefixRepeatsSuffixReal suffixTransitions lastPrefix firstSuffix =
      if lastPrefix == firstSuffix then
        False -- e.g. pqt-zvt-zvxt
      else
        case suffixTransitions of
          [] ->
            -- can't be true if we don't end at the final.
            isFinalNode firstSuffix dawg
          (w, _)::rest ->
            case ( forwardsFollowable w lastPrefix dawg.graph, forwardsFollowable w firstSuffix dawg.graph ) of
              ( Just inPrefix , Just inSuffix ) ->
                prefixRepeatsSuffixReal rest inPrefix.node.id inSuffix.node.id
              _ ->
                False
  in
    if List.isEmpty suffixTransitions_ then
      -- d'uh. 🤦 infinite loop in caller otherwise, eh? Silly billy.
      False
    else
      prefixRepeatsSuffixReal suffixTransitions_ lastPrefix_ firstSuffix_

traceForwardOnThisChain : Transition -> List Transition -> LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
traceForwardOnThisChain transition rest linking d dawg =
  debugDAWG ("There is no alt-chain; I am tracing forward on the main chain, #" ++ String.fromInt linking.graphPrefixEnd ++ "→#" ++ String.fromInt d.id ++ ".  Before doing anything") dawg |> \_ ->
  case ( connectsToFinalNode linking.graphPrefixEnd dawg, d.isFinal ) of
    ( Nothing, True ) ->
      debugDAWG "[F] Impossible path" empty
    _ ->
      -- e.g. kp-gx-ax-gp , zv-kv-rv-kva , an-tn-x-tx , x-b-bc-ac-bx
      connectIndependentPaths transition rest linking d dawg

withOutgoingNodesOf : List NodeId -> (NodeId -> DAWG -> DAWG) -> DAWG -> DAWG
withOutgoingNodesOf nodeids f dawg =
  List.filterMap (flip Graph.get dawg.graph >> Maybe.map (.outgoing >> IntDict.keys)) nodeids
  |> List.concat
  |> List.unique |> Debug.log "folding with"
  |> List.foldl f dawg

traceForwardOnAlternateChain : Transition -> List Transition -> LinkingForwardData -> CurrentNodeData -> NodeId -> DAWG -> DAWG
traceForwardOnAlternateChain transition rest linking d c dawg =
  -- `c` is the alternate chain.
  -- `d` is the trace on the main chain.
  debugDAWG ("Tracing forward on alt-chain that ends on #" ++ String.fromInt c ++ "; on main chain, the trace is from, #" ++ String.fromInt linking.graphPrefixEnd ++ "→#" ++ String.fromInt d.id ++ ".  Before doing anything") dawg |> \_ ->
  case ( connectsToFinalNode linking.graphPrefixEnd dawg, rest, d.isFinal ) of
    ( Nothing, [], False ) -> -- e.g. ato-cto-atoz
      if List.isEmpty linking.suffixPath then -- if this is terminal
          -- e.g. ato-cto-at
        println ("[G] Inserted word is a prefix of an existing word. Connecting alt-path #" ++ String.fromInt c ++ " to encountered node #" ++ String.fromInt d.id ++ " and exiting.")
        -- createTransitionBetween transition c d.id dawg
        duplicateOutgoingConnections linking.graphPrefixEnd c dawg
        |> dawgUpdate c (connectTo d.id transition) -- in case the transition is terminal, this will merge
        |> checkForCollapse d.id
      else
        case outgoingConflict d.id linking.graphSuffixEnd dawg of
          [] ->
            -- e.g. tsbl-nsbl-nsl
            createNewSuccessorNode d.chosenTransition c dawg
            |> (\(dawg_, successor) ->
                println ("[G] Inserted word is a (possibly partial) prefix. Created #" ++ String.fromInt successor ++ ", and duplicating the outgoing transitions of #" ++ String.fromInt linking.graphSuffixEnd ++ " and #" ++ String.fromInt d.id ++ " to it.")
                duplicateOutgoingConnectionsExcludingTransitions [transition] linking.graphPrefixEnd c dawg_ -- CHECK HERE!!!
                |> duplicateOutgoingConnections linking.graphSuffixEnd successor
                |> duplicateOutgoingConnections d.id successor
                |> withOutgoingNodesOf [successor, d.id] checkForCollapse
              )
          transitions -> -- e.g. teste-ne-neste
            -- shift the window forward & reconsider?  If I try to do a direct link, I will end up with
            -- a nondeterministic graph.
            case linking.suffixPath of
              [] -> debugDAWG "[N] Impossible case." empty
              h::rest_ ->
                Debug.log ("Conflicting transitions (" ++ transitionsToString transitions ++ ") of #" ++ String.fromInt d.id ++ " and #" ++ String.fromInt linking.graphSuffixEnd ++ " prevent a direct switch.  Shifting window & retrying instead.") () |> \_ ->
                case forwardsFollowable (Tuple.first h) linking.graphSuffixEnd dawg.graph of
                  Nothing ->
                    debugDAWG "[N-2] Impossible case." empty
                  Just newSuffix ->
                    traceForwardChainTo transition (rest ++ [h])
                      { linking | graphSuffixEnd = newSuffix.node.id, suffixPath = rest_ }
                      d dawg
    ( Nothing, _, False) ->
      createNewSuccessorNode d.chosenTransition c dawg
      |> \(dawg_, successor) ->
        println ("[G] Trace-forward with an alt-path.  Duplicating past nodes of #" ++ String.fromInt linking.graphPrefixEnd ++" to #" ++ String.fromInt c ++ ", creating new alt-path node #" ++ String.fromInt successor ++ ", linked from #" ++ String.fromInt c ++ ", then continuing.")
        duplicateOutgoingConnectionsExcludingTransitions [transition] linking.graphPrefixEnd c dawg_ -- CHECK HERE!!!
        |> createForwardsChain rest { linking | graphPrefixEnd = d.id, lastConstructed = Just successor }
        |> checkForCollapse successor
    ( Just final, _, True ) ->
      -- I am on an alt path and the graph connects to a final.  Am I also ending, though?
      -- Let me connect myself to the graphSuffixEnd, using the current transition.
      println ("[L] On an alt-path; the graph ends here. My remaining transitions are " ++ transitionsToString rest ++ ".  Creating chain between #" ++ String.fromInt c ++ " and #" ++ String.fromInt linking.graphSuffixEnd)
      duplicateOutgoingConnectionsExcludingTransitions [transition] linking.graphPrefixEnd c dawg -- CHECK HERE!!!
      |> checkForCollapse c
      |> createTransitionChainBetween (transition::rest) c linking.graphSuffixEnd 
    ( Nothing, _, True ) ->
      debugDAWG "[H] Impossible path" empty
    ( Just final, _, False ) ->
      debugDAWG "[K] Impossible path" empty

{-| Called when there IS a path forward from `.graphPrefixEnd` to `d`. -}
traceForwardChainTo : Transition -> List Transition -> LinkingForwardData -> CurrentNodeData -> DAWG -> DAWG
traceForwardChainTo transition rest linking d dawg =
  if List.isEmpty rest && prefixRepeatsSuffix linking.suffixPath d.id linking.graphSuffixEnd dawg then
    println ("The prefix (#" ++ String.fromInt linking.graphPrefixEnd ++ ") mirrors the suffix (#" ++ String.fromInt linking.graphSuffixEnd ++ ") using transitions " ++ transitionsToString linking.suffixPath ++ ".  I will shift the prefix-suffix window and reconsider what to do.")
    dawg.final
    |> Maybe.map (\final ->
      createForwardsChain (transition::linking.suffixPath)
        { linking
          | graphSuffixEnd = final
          , suffixPath = []
        }
        dawg
    )
    |> Maybe.withDefaultLazy (\() -> Debug.log "wtf??  final is not set?!!" empty)
  else
    case linking.lastConstructed of
      Nothing ->
        traceForwardOnThisChain transition rest linking d dawg
      Just c ->
        traceForwardOnAlternateChain transition rest linking d c dawg
    -- if there is a connection to final BUT `d` is NOT the final node, that is a separate case!

{-| When there is NO corresponding forward-move on the graph, we call this function
    to forge a path forward.  There is at least one forward-transition, so this will
    involve forging some alternate path/connection, at a minimum.
-}
forgeForwardChain : Transition -> List Transition -> LinkingForwardData -> DAWG -> DAWG
forgeForwardChain transition rest linking dawg =
  case ( connectsToFinalNode linking.graphPrefixEnd dawg, linking.lastConstructed ) of
    ( Nothing, Nothing ) ->
      -- e.g.: a
      -- make a new chain between prefix and suffix, and we are done.
      createTransitionChainBetween (transition::rest) linking.graphPrefixEnd linking.graphSuffixEnd dawg
      |> debugDAWG "[A] No forward-path, no alt-path, no final-connection: connect prefix to suffix, and exit."
    ( Nothing, Just c ) ->
      duplicateOutgoingConnections linking.graphPrefixEnd c dawg
      |> createTransitionChainBetween (transition::rest) c linking.graphSuffixEnd
      |> debugDAWG "[B] No forward-path, no final-connection, but we have an alt-path: connect alt-path to suffix, and exit."
    ( Just final, Nothing ) ->
      -- e.g. a-b
      -- Straightforward, create a confluence or connection and we are done.
      createTransitionChainBetween (transition::rest) linking.graphPrefixEnd linking.graphSuffixEnd dawg
      |> debugDAWG ("[C] No forward-path from " ++ String.fromInt linking.graphPrefixEnd ++ " using " ++ transitionToString transition ++ " to #" ++ String.fromInt linking.graphSuffixEnd ++ "; I'll create one.")
    ( Just final, Just c) -> -- ato-cto-ati
      duplicateOutgoingConnections linking.graphPrefixEnd c dawg |> debugDAWG "step 1"
      |> createTransitionChainBetween (transition::rest) c linking.graphSuffixEnd
      |> debugDAWG ("[D] On an alt-path. No forward-path from #" ++ String.fromInt linking.graphPrefixEnd ++ " to #" ++ String.fromInt linking.graphSuffixEnd ++ " using " ++ transitionToString transition ++ "; I'll create one.")

{-| Create a forwards-chain going from dP (the prefix-node) to dS (the
    suffix-node).  The suffix-node might be the final (dω).
-}
createForwardsChain : List Transition -> LinkingForwardData -> DAWG -> DAWG
createForwardsChain transitions linking dawg =
  Debug.log ("[Chaining] Remaining transitions " ++ transitionsToString transitions ++ " with linking ") linking |> \_ ->
  case transitions of
    [] ->
      Debug.log ("[Chaining 1] NO TRANSITIONS?? Probably a bug!") linking
      |> \_ -> dawg
    transition::rest ->
      case getForwardNodeData linking.graphPrefixEnd transition dawg of
        Nothing ->
          -- There's nothing that will take me forward.
          -- Therefore, if I'm going to connect to the suffix, I've got to
          -- forge a new forward chain.
          forgeForwardChain transition rest linking dawg
        Just d ->
          -- I have something that can take me forward, so I may be able
          -- to follow it.
          traceForwardChainTo d.chosenTransition rest linking d dawg

createChain : List Transition -> NodeId -> NodeId -> List Transition -> DAWG -> DAWG
createChain transitions prefixEnd suffixEnd suffixTransitions dawg =
  println ("[Chaining] Creating chain from #" ++ String.fromInt prefixEnd ++ " to #" ++ String.fromInt suffixEnd ++ " with transitions " ++ transitionsToString transitions ++ ".  Suffix-path was " ++ transitionsToString suffixTransitions ++ ".")
  createForwardsChain
    transitions
    { graphPrefixEnd = prefixEnd
    , lastConstructed = Nothing
    , graphSuffixEnd = suffixEnd
    , suffixPath = suffixTransitions
    }
    dawg

-- transitions are provided in REVERSE order to mergeSuffixes.
mergeSuffixes : List Transition -> NodeId -> DAWG -> DAWG
mergeSuffixes transitions prefixEnd dawg =
  case dawg.final of
    Nothing ->
      -- create a final-terminated chain going backwards, culminating at `prefixEnd`
      println "[Suffix] No final exists; creating it."
      addFinalNode dawg
      |> \(finalnode, dawg_) ->
        mergeSuffixes transitions prefixEnd dawg_
    Just final ->
      println ("[Suffix] Using final #" ++ String.fromInt final ++ ", following suffixes back to get close to prefix-node #" ++ String.fromInt prefixEnd)
      followSuffixes transitions [] prefixEnd final dawg
      |> \(suffixStart, numTaken) ->
        let
          (a, b) = List.splitAt numTaken transitions
          followed = List.reverse a
          to_chain_with = List.reverse b
        in
          createChain to_chain_with prefixEnd suffixStart followed dawg

{--
  Output/debugging functions
--}

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
  Debug.log txt (graphToString graph)
  |> \_ -> graph

debugDAWG : String -> DAWG -> DAWG
debugDAWG txt dawg =
  Debug.log txt
    (graphToString dawg.graph)
  |> \_ -> dawg

println : String -> a -> a
println txt x =
  Debug.log txt () |> \_ -> x

debugLog : String -> (a -> b) -> a -> a
debugLog s f v =
  Debug.log s (f v) |> \_ -> v

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

isEmpty : DAWG -> Bool
isEmpty d =
  d.maxId == 0

addString : String -> DAWG -> DAWG
addString txt dawg =
  wordToTransitions txt
  |> \transitions -> prefixMerge transitions dawg.root dawg

fromWordsGraphing : List String -> DAWG
fromWordsGraphing =
  List.foldl (\s a -> addString s a |> debugDAWG ("🔻 Post-insertion of '" ++ s ++ "'")) empty

fromWordsAlgebraic : List String -> DAWG
fromWordsAlgebraic =
  List.map (String.replace " " "" >> String.replace "\n" "")
  >> List.filterMap
    (\s ->
      case (String.dropRight 1 s, String.right 1 s) of
        ( _, "" ) -> Nothing
        ( "", x ) -> Just <| "!" ++ x
        ( a, b ) ->
          String.toList a
          |> List.intersperse '.'
          |> String.fromList
          |> \initial -> initial ++ ".!" ++ b
          |> Just
    )
  >> String.join "+"
  >> Debug.log "Sending to algebra…"
  >> fromAlgebra

fromWords : List String -> DAWG
fromWords = fromWordsGraphing

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
            |> List.map (Set.map Tuple.first) -- |> Debug.log ("CHECK for #" ++ String.fromInt node.node.id)
          allTransitions =
            List.foldl Set.union Set.empty allSets -- |> Debug.log "All transitions"
          duplicate =
            List.foldl
              (\currentSet (result, all) ->
                Set.diff all currentSet -- (Debug.log "Checking against" currentSet)
                |> (\diff -> (Set.diff (Set.union currentSet result) all, diff)) -- |> Debug.log "now")
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

isMinimal : DAWG -> Result String DAWG
isMinimal dawg =
  -- This is Hopcroft's Algorithm
  let
    edges = -- Edge (Transition)
      Graph.edges dawg.graph
      |> List.concatMap
        (\{from, to, label} ->
          Set.toList label |> List.map (\l -> (from, to, l))
        )
    (finals, nonFinals) = -- the initial partition.
      -- those which lead to finality, and those which don't.
      List.partition (\(_, _, (_, isFinal)) -> isFinal == 1) edges
      |> \(a, b) -> ( List.map (\(_,v,_) -> v) a |> Set.fromList, 0::List.map (\(_,v,_) -> v) b |> Set.fromList )
      -- |> Debug.log "Finals and non-finals"
    refine : HopcroftRecord -> List Partition
    refine r =
      case (r {- |> Debug.log "hopcropft" -}).w of
        [] ->
          r.p
        a::w_rest ->
          let
            xs =
              List.filterMap
                (\(from, to, (ch, _)) ->
                  if Set.member to a then
                    Just (ch, from)
                  else
                    Nothing
                ) edges
              |> List.gatherEqualsBy Tuple.first
              |> List.map (\((ch, h), t) -> (ch, Set.fromList (h::List.map Tuple.second t))) -- Now I should have a list of (ch, {states_from_w_which_go_to_`a`})
              -- |> Debug.log ("`X` set, given `A` of " ++ (Debug.toString (Set.toList a)))
            refine_for_input_and_y : Partition -> Partition -> Partition -> List Partition -> List Partition -> (List Partition, List Partition)
            refine_for_input_and_y y further_split remaining_after w p =
              ( if List.member y w then
                  (further_split :: remaining_after :: List.remove y w)
                  -- |> Debug.log "Refining w, stage Ⅱa"
                else
                  if Set.size further_split <= Set.size remaining_after then
                    (further_split :: w)
                    -- |> Debug.log "Refining w, stage Ⅱb"
                  else
                    (remaining_after :: w)
                    -- |> Debug.log "Refining w, stage Ⅱc"
              , (further_split :: remaining_after :: List.remove y p)
                -- |> Debug.log "Refining p, stage Ⅱ"
              )
            refine_for_input : Char -> Partition -> List Partition -> List Partition -> (List Partition, List Partition)
            refine_for_input ch x w p = -- really, the ch is only there for potential debugging.
              let
                candidate_sets =
                  List.filterMap
                    (\potential_y ->
                      let
                        further_split = Set.intersect x potential_y -- |> Debug.log ("Intersection of " ++ Debug.toString x ++ " and " ++ Debug.toString potential_y)
                        remaining_after = Set.diff potential_y x -- |> Debug.log ("Subtraction: " ++ Debug.toString potential_y ++ " minus " ++ Debug.toString x)
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
              List.foldl (\(ch, x) (w, p) -> refine_for_input ch x w p) (w_rest, r.p) xs
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
    -- |> Debug.log "Hopcroft raw result"
    -- |> debugLog "Hopcroft result" (List.map Set.size)
    |> List.filter (\s -> Set.size s > 1)
    |>
    (\l ->
      case l of
        [] -> Ok dawg
        _ -> Err <| "Not minimal; can combine: " ++ (List.map (\s -> "[" ++ (Set.toList s |> List.map String.fromInt |> String.join ", ") ++ "]") l |> String.join "; ")
    )
