module Automata.Data exposing (..)
import IntDict exposing (IntDict(..))
import Set exposing (Set)
import AutoSet
import Graph exposing (Graph, NodeContext, NodeId)
import AutoDict
import Parser exposing (Parser, (|=), (|.))
import Uuid exposing (Uuid)
import List.Extra
import Color

-- Note: Graph.NodeId is just an alias for Int. (2025).

type alias RequestedChangePath = List Transition -- transitions going from the start to the node.

type LinkDestination
  = NoDestination
  | ExistingNode NodeId
  | NewNode ( Float, Float ) -- with X and Y coordinates

type alias GraphModification =
  { source : NodeId
  , dest : LinkDestination
  , transitions : Connection
  }

type alias ConnectionAlteration =
  { source : NodeId
  , dest : NodeId
  , transitions : Connection
  }

type alias Split =
  { to_split : NodeId
  , left : Connection
  , right : Connection
  }

type alias Entity =
  -- fits into the mould of Force.Entity AND Automata.Data.StateData.
  -- I've annotated the fields to make it blindingly obvious what's
  -- from where…
  { x : Float -- F.E
  , y : Float -- F.E
  , vx : Float -- F.E
  , vy : Float -- F.E
  , id : NodeId -- F.E
  , effect : NodeEffect -- A.D.SD
  }

-- the Bool indicates whether it is a Final position or not.
-- This is superior to marking finality on a vertex, because
-- multiple transitions—some final, some not—could end on a
-- vertex.

-- **NOTE**
-- If AcceptVia is modified, also modify the corresponding AcceptChoice
-- data structure in ForceDirectedGraph.
type AcceptVia
  = ViaCharacter Char
  | ViaGraphReference Uuid

type NodeEffect
  = NoEffect
  | SomeEffectFigureItOutLater

type alias StateData extending =
  { extending |
    effect : NodeEffect
  }

type alias Transition = (AcceptVia, Int) -- INSANELY, Bool is NOT `comparable`. So, 0=False, 1=True. 🤪.
type alias Connection = AutoSet.Set String Transition -- a Connection is a link between two nodes.
type alias Node a = NodeContext (StateData a) Connection -- a Node itself does not carry any data, hence the ()
type alias AutomatonGraph a =
  { graph : Graph (StateData a) Connection -- finally, the complete graph.
    {- The maximum ID-value in this Automaton graph -}
  , maxId : NodeId
  , root : NodeId
  }

{- So with an DFA, I can basically do anything that's deterministic.

Looping back to previous nodes? Go ahead.
Looping back to ourselves? Definitely!

Now with that said, nodes without outgoing edges SHOULD be terminal nodes.
And everything is connected.  And outgoing nodes have deterministic transitions.
-}

type alias DFARecord extending label =
  { extending |
    states : IntDict (StateData label)
  , transition_function: IntDict (AutoDict.Dict String AcceptVia NodeId) -- NodeId × Char → NodeId
  , start : NodeId
  , finals : Set NodeId
  }

type alias ExtDFA label =
  { states : IntDict (StateData label)
  , transition_function: IntDict (AutoDict.Dict String AcceptVia NodeId)
  , start : NodeId
  , finals : Set NodeId
  , register : Set NodeId
  , clone_start : NodeId
  , queue_or_clone : List NodeId
  , unusedId : NodeId
  }
type alias ExecutionData =
  { transitionsTaken : List (NodeId, Transition) -- (src, transition)
  , remainingData : List AcceptVia
  , currentNode : NodeId
  }

type ExecutionState
  = Accepted ExecutionData
  | Rejected ExecutionData
  | RequestedNodeDoesNotExist ExecutionData -- this is an internal error
  | NoPossibleTransition ExecutionData

type ExecutionResult
  = EndOfInput ExecutionState
  | EndOfComputation ExecutionState -- no transition possible, though more input exists
  | CanContinue ExecutionState
  | InternalError

type TestExpectation
  = ExpectAccepted
  | ExpectRejected

type alias Test =
  { input : String
  , expectation : TestExpectation
  , result : ExecutionResult
  }

empty : AutomatonGraph a
empty =
  { graph = Graph.empty
  , maxId = 0
  , root = 0
  }

graphToAutomatonGraph : NodeId -> Graph (StateData a) Connection -> AutomatonGraph a
graphToAutomatonGraph start graph =
  { graph = graph
  , maxId = Graph.nodeIdRange graph |> Maybe.map Tuple.second |> Maybe.withDefault 0
  , root = start
  }

{-| Convert an AcceptVia to a round-trip string.

*NOTE*: This is NOT to be used for printable output!

Use `printableAcceptCondition` for that.
-}
acceptConditionToString : AcceptVia -> String
acceptConditionToString v =
  case v of
    ViaCharacter ch ->
      String.fromChar ch
    ViaGraphReference uuid ->
      Uuid.toString uuid

{-| Convert an AcceptVia to a printable-for-output string. -}
printableAcceptCondition : AcceptVia -> String
printableAcceptCondition v =
  case v of
    ViaCharacter ch ->
      String.fromChar ch
    ViaGraphReference uuid ->
      Uuid.toString uuid
      |> String.left 8
      |> \s -> s ++ "..."

{-| True if at least one transition terminates at this node -}
isTerminalNode : NodeContext a Connection -> Bool
isTerminalNode node =
  -- IntDict.isEmpty node.outgoing &&
  ( IntDict.foldl
    (\_ conn state ->
      state ||
        AutoSet.foldl
          (\(_, isFinal) state_ -> state_ || isFinal == 1)
          False
          conn
    )
    False
    (node.incoming)
  )

isTerminal : NodeId -> Graph x Connection -> Bool
isTerminal id graph =
  Graph.get id graph
  |> Maybe.map isTerminalNode
  |> Maybe.withDefault False

graphEdgeToString : Graph.Edge Connection -> String
graphEdgeToString {from, to, label} =
  "#" ++ String.fromInt from ++ "➜#" ++ String.fromInt to ++ " (" ++ connectionToString label ++ ")"

transitionToString : Transition -> String
transitionToString transition =
  case transition of
    (ViaCharacter ch, 0) ->
      String.fromChar ch
    (ViaCharacter ch, _) ->
      "\u{0307}" ++ String.fromChar ch
    (ViaGraphReference uuid, 0) ->
      Uuid.toString uuid
    (ViaGraphReference uuid, _) ->
      "!" ++ Uuid.toString uuid

connectionToString : Connection -> String
connectionToString =
  AutoSet.map identity transitionToString
  >> AutoSet.toList
  >> String.join "\u{2008}" -- punctuation space. Stops terminality-marker from disappearing on subsequent characters.

graphToString : (a -> Maybe String) -> Graph a Connection -> String
graphToString printer graph =
  Graph.toString
    printer
    (Just << connectionToString)
    graph






-- Parser for converting string representation to AutomatonGraph transitions
-- Example: "0-!av-1 0-b!vk!z-2 2-p-0" -> [(0, "!av", 1), (0, "b!vk!z", 2), (2, "p", 0)]
transitionsParser : Parser (List (Int, String, Int))
transitionsParser =
    Parser.oneOf
        [ Parser.succeed []
            |. Parser.end
        , Parser.loop [] transitionsHelp
        ]


transitionsHelp : List (Int, String, Int) -> Parser (Parser.Step (List (Int, String, Int)) (List (Int, String, Int)))
transitionsHelp revTransitions =
    Parser.oneOf
        [ Parser.succeed (\transition -> Parser.Loop (transition :: revTransitions))
            |= transitionParser
            |. Parser.oneOf
                [ Parser.symbol " "
                , Parser.succeed ()
                ]
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revTransitions))
        ]


transitionParser : Parser (Int, String, Int)
transitionParser =
    Parser.succeed (\src label dest -> (src, label, dest))
        |= Parser.int
        |. Parser.symbol "-"
        |= labelParser
        |. Parser.symbol "-"
        |= Parser.int


labelParser : Parser String
labelParser =
    Parser.succeed identity
        |= Parser.getChompedString
            (Parser.succeed ()
                |. Parser.chompIf (\c -> c /= '-' && c /= ' ')
                |. Parser.chompWhile (\c -> c /= '-' && c /= ' ')
            )

-- Helper function that converts string to transitions and handles errors
mkAG_input : String -> List (Int, String, Int)
mkAG_input input =
  case Parser.run transitionsParser input of
    Ok transitions ->
      transitions
    Err _ ->
      []

-- https://en.wikipedia.org/wiki/Factorial_number_system
-- …which also, conveniently, gives me indices to pull out
-- for the /n/th permutation ^_^!
factorialRepresentation : Int -> List Int
factorialRepresentation n_ =
  let
    helper n base acc =
      if n == 0 then
        acc
      else
        helper (n // base) (base + 1) ((n |> modBy base) :: acc)
  in
    helper n_ 2 [0]

-- r = number of choices that we are making
-- l = the list that we are choosing from
-- k = index of the choice that we are interested in
-- Thanks go to:
-- - https://stackoverflow.com/a/1776884
-- - https://www.geeksforgeeks.org/dsa/program-calculate-value-ncr/
kthCombination : Int -> List a -> Int -> List a
kthCombination k l r =
  let
    -- nCr = exp( log(n!) - log(r!) - log((n-r)!))
    nCr : Int -> Int -> Int
    nCr n r_ =
      List.range 1 r_
      |> List.foldl (\i acc -> acc * (toFloat n - toFloat r_ + toFloat i) / toFloat i) 1.0
      |> floor
  in
    if r == 0 then
      []
    else if List.length l == r then
      l
    else
      let
        i = nCr (List.length l - 1) (r - 1)
      in
        case l of
          [] -> []
          h::t ->
            if k < i then
              h :: kthCombination k t (r - 1)
            else
              kthCombination (k - i) t r

hexCharToInt : Char -> Maybe Int
hexCharToInt ch =
  case ch of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    'a' -> Just 10
    'b' -> Just 11
    'c' -> Just 12
    'd' -> Just 13
    'e' -> Just 14
    'f' -> Just 15
    _ -> Nothing

base16ToInt : String -> Maybe Int
base16ToInt s =
  let
    helper remaining multiplier acc =
      case remaining of
        [] -> Just acc
        ch::rest ->
          hexCharToInt ch
          |> Maybe.andThen (\d -> helper rest (multiplier * 16) (acc + multiplier * d))
  in
    helper (String.toList s |> List.reverse) 1 0

getPalette : Uuid -> List Color.Color
getPalette uuid =
  let
    hexChars =
      Uuid.toString uuid
      |> String.replace "-" ""
      |> String.toList
      |> List.filterMap hexCharToInt
  in
    case hexChars of
      a::b::c::rest ->
        let
          paletteChoices =
            kthCombination (a * 16 * 16 + b * 16 + c) distinctHexColors 16
          pixels =
            List.filterMap
              (\idx -> List.Extra.getAt idx paletteChoices)
              (List.take 9 rest ++ List.drop 10 rest)
        in
          if List.length pixels /= 28 then
            Debug.log ("ERROR XK<()IDHI>* " ++ String.fromInt (List.length pixels)) [] -- Failed, somehow, to obtain colors!
          else
            pixels
      _ ->
        Debug.log "MBXF*G><DI" []

{-
Sasha Trubetskoy is AWESOME!

Years later, I'm still falling back to this amazing list that was put up all the
way back in 2017.  I'm so glad that I found it, so long ago…

'#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9', '#ffffff', '#000000'
-}

-- and here's a slightly more curated version, excluding
-- black & white.  That gives us 20 colours in total.
-- The ones I've marked with a * should have black writing on them.
-- The others look good with white writing.
distinctHexColors : List Color.Color
distinctHexColors =
  [ -- the first of these are bold, bright colours
    "e6194B" -- Red
  , "3cb44b" -- Green
  , "ffe119" -- *Yellow
  , "4363d8" -- Blue
  , "f58231" -- Orange
  , "911eb4" -- Purple
  , "42d4f4" -- *Cyan
  , "f032e6" -- Magenta
  , "bfef45" -- *Lime
  , "a9a9a9" -- Grey
  -- Now we have more pale subtle colours
  , "fabed4" -- *Pink
  , "dcbeff" -- *Lavender
  , "fffac8" -- *Beige
  , "aaffc3" -- *Mint
  , "ffd8b1" -- *Apricot
  -- These are the darker colours next
  , "000075" -- Navy
  , "9A6324" -- Brown
  , "808000" -- Olive
  , "800000" -- Maroon
  , "469990" -- Teal
  -- Lastly, pure black and white
  -- , "#ffffff" -- *White
  -- , "#000000" -- Black
  ]
  |> List.filterMap
    (\s ->
      case String.toList s of
        r0::r1::g0::g1::b0::b1::[] ->
          Maybe.map3 Color.rgb255
            (base16ToInt (String.fromList [r0, r1]))
            (base16ToInt (String.fromList [g0, g1]))
            (base16ToInt (String.fromList [b0, b1]))
        _ -> Nothing
    )

-- A UUID has the format
-- xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx
-- where the 'M' is a constant, thus losing us 4 bits out of 128.
-- The 'N' is pretty predictable but still has at least 1-2 bits of
-- randomness.
-- I have 20 colours to choose from, but I actually only need to draw 16.
-- P(20, 16) = 101 370 917 007 360 000, which is a bit over 2^56.
-- That's pretty useful, because I can use the first 56 bits to pick a
-- distinct palette: the /n/th permutation.
-- That leaves me with 124 - 56 = 68 bits to deal with.
-- One pixel can be uniquely chosen by 4 bits, and there are 256 pixels
-- (16x16) to go through. So, that would use up 1024 bits.
-- Or we could double the size of the pixels, thus leaving 8x8 = 64
-- to go through. Then we'd deal with 256 bits.  If we double the size
-- of the "pixel" again, we have a 4x4 = 16 space, and then we would be
-- able to use up 64 bits.  Which leaves us with 4 bits that we wouldn't
-- actually use.  And if we leave out `N` entirely, that means that we
-- ignore 1-2 bits of randomness, but we get bigger areas of colour…
-- I think that might work.
--
-- Now of course, this could break down very badly indeed. The first part
-- selects a palette, and the second parts picks from it. Given different
-- palettes, different choices in the second part could lead to identical
-- outputs.  So in fact, I probably have only 2^64 bits of randomness
-- that I'm actually representing.
--
-- So let's think: could I avoid that? Well, I could use combinations
-- instead.  So C(20, 16) = 4845, and that uses up 12 bits of randomness
-- (2^12 = 4096) though it also leaves 769 possible combinations on the
-- table. That's okay; I want uniqueness of palette, and I get it.
-- That leaves me with 124-12 = 112 bits to deal with.   One pixel can be
-- uniquely chosen by 4 bits, so there are 28 pixels that can be set with
-- 112 bits.  Technically speaking, that's 7x4 = 28.  If we have "pixels"
-- that take up 4 actual pixels each, and we have 7 columns, then the
-- width × height of our displayed area will be 28 x 16.  I think that's
-- doable.