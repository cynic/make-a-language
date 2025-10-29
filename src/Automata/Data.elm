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
import Time
import Random.Pcg.Extended as Random
import Force
import Binary
import SHA

-- Note: Graph.NodeId is just an alias for Int. (2025).

{-------------------------------------------------------
  Main.elm
--------------------------------------------------------}

type LeftPanelIcon
  = ComputationsIcon
  | SearchIcon
  | GitIcon
  | TestsIcon
  | ExtensionsIcon

type ExecutionStage
  = Ready
  | NotReady
  | ExecutionComplete
  | StepThrough

type BottomPanel
  = AddTestPanel
  | EditDescriptionPanel

type alias GraphPackage =
  { userGraph : AutomatonGraph -- the UUID is inside the model's .userGraph.graphIdentifier
  , dimensions : ( Float, Float )
  , description : Maybe String
  , created : Time.Posix -- for ordering
  , currentTestKey : Uuid
  , tests : AutoDict.Dict String Uuid Test
  , undoBuffer : List (AutomatonGraph)
  , redoBuffer : List (AutomatonGraph)
  }

type alias Main_Model =
  { fdg_model : FDG_Model
  , mainPanelDimensions : ( Float, Float )
  , leftPanelOpen : Bool
  , selectedIcon : Maybe LeftPanelIcon
  , leftPanelWidth : Float
  , rightBottomPanelOpen : Bool
  , rightBottomPanelHeight : Float
  , rightTopPanelDimensions : ( Float, Float )
  , isDraggingHorizontalSplitter : Bool
  , isDraggingVerticalSplitter : Bool
  , executionStage : ExecutionStage
  , selectedBottomPanel : BottomPanel
  }

type alias Flags =
  { width : Float
  , height : Float
  , initialSeed : Int
  , extendedSeeds : List Int
  , startTime : Time.Posix
  , packages : List GraphPackage
  }

{-------------------------------------------------------
  ForceDirectedGraph.elm
--------------------------------------------------------}

type UserOperation
  = Splitting Split -- split a node into two, so I can work on the parts separately
  | Dragging NodeId -- move a node around (visually)
  | ModifyingGraph AcceptChoice GraphModification -- add a new link + node / new link between existing nodes
  | AlteringConnection AcceptChoice ConnectionAlteration
  | Executing AutomatonGraph ExecutionResult

type AcceptChoice
  = ChooseCharacter
  -- the first parameter is the thumbnail index to focus on
  | ChooseGraphReference Int

type alias FDG_Model =
  { currentOperation : Maybe UserOperation
  , currentPackage : GraphPackage
    -- Instead of repeating here, I could just supply a Uuid -> GraphPackage
    -- "resolver" function. But I choose to repeat, because such a function
    -- would be totally opaque to any tools, and if something ever goes wrong
    -- with an out-of-date resolver, I'm going to spend days poring over where
    -- that problem is.
    --
    -- No, I'll take the ability to examine the internals over that future
    -- morass.
  , packages : AutoDict.Dict String Uuid GraphPackage
  , simulation : Force.State NodeId
  , dimensions : (Float, Float) -- (w,h) of svg element
  , basicForces : List (Force.Force NodeId) -- EXCLUDING the "center" force.
  , viewportForces : List (Force.Force NodeId)
  , specificForces : IntDict.IntDict (List (Force.Force NodeId))
  , zoom : Float -- zoom-factor
  , pan : (Float, Float) -- panning offset, x and y
  , mouseCoords : ( Float, Float )
  , mouseIsHere : Bool
  , disconnectedNodes : Set NodeId
  , currentTime : Time.Posix
  , uuidSeed : Random.Seed
  }

{-------------------------------------------------------
  Other
--------------------------------------------------------}
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

{-
When we make a union of DFAs, each transition is tagged with a set of values that represent
which DFA(s) it is present in (i.e., which ones it "comes from").  When we are executing on
that union'd DFA, we begin with the full set of tags from all of its constituents.  The rule
is: we can only take a transition if its tag is in the allowed set.  As we select transitions,
the tags get reduced at each step to the intersection between the allowed set and the tags
on the followed transition.  This means that we end up with a smaller set of allowable
outbound links that we can possibly take, while maintaining a single graph.

Why do we want to do this?  To be able to represent multiple graphs in the union WITHOUT
invalid cross-language transitions happening.  For example, consider:

X : [ (0, p, 1), (1, !q, 0) ]
Y : [ (0, !v, 1) ]

Their union might be phrased as:

Z : [ (0, p, 1), (1, !q, 0), 0, !v, 2) ]

…but now, have a look at what's permitted.  Suddenly, the sequence "pqv" is accepted when it
was NOT accepted in either X or Y.  But now, let's give them some tags in the union.

Z : [ (0, {X}p, 1), (1, {X}!q, 0), 0, {Y}!v, 2) ]

We begin with {X,Y}.  And given the input "pqv", we follow "p", and our allowed-set shrinks to
{X,Y}-union-{X}={X}. That still lets us take "!q".  But we can no longer take "v", because the
allowed-set does not contain {Y}, so we end our journey here.
-}

{-
A thought comes up: what am I supposed to do if I have a "sub-graph" whose transitions
might be sub-graph transitions OR super-graph transitions?

For example:

@r : a → @b → c → !d

where @b is: !k → c → !d ?

Now, given the input "akcd", should we complete the sub-graph or the super-graph?

Well, let's start by flattening.  We flatten by inserting the sub-graph, and then
attaching the destination node of the connection to each of the terminals of that
sub-graph.  In this case, this would result in:

a → {b}!k → {b}c → {b}!d → c → !d [dest-node linked to sub-graph !d]
     `-→ c → !d [dest-node linked to sub-graph !k]

Which flattens to

a → {b}!k → {b}c → {b}!d → c → !d , which is a final graph that will accept anything.

If @b : k → !j , then flattening would end up with:

a → {b}k → {b}!j → c → !d .

If @b : [ (0, !k, 1), (1, !z, 0) ], then flattening gives us:

@r : [ (0, a, 1), (2, c, 3), (3, !d, 4), (1, !k, 2), (2, !z, 1), (2, !z, 2) ]

-}

type alias Transition =
  {
  --| finality is specific to a particular tag; and tags are specific to a particular DFA.
  -- But if I have a tag-specific dict, then what happens if I can't find a matching tag?
  -- Or, what happens if the tag-set is empty?
  -- Then, I think:
  -- (1) It is an error in the code, and I should spit out a debugging message;
  --     (AND I should write some tests for this!)
  -- (2) I should, by default, say that it is NOT final.
  --
  -- But what happens when there are >1 tags in an allowed-set, during execution, and
  -- one of them is final and the other one is not?  Then it is the finality which
  -- takes preference, because legitimately, at least one of the "unioned" DFAs DOES
  -- end at this point; and if the input ends here, for example, then that's a
  -- definite case of "ACCEPT".
    isFinal : Bool
  , via : AcceptVia
  }

type alias Connection = AutoSet.Set String Transition -- a Connection is a link between two nodes.
type alias Node = NodeContext Entity Connection -- a Node itself does not carry any data, hence the ()
type alias AutomatonGraph =
  { graph : Graph Entity Connection -- finally, the complete graph.
    {- The maximum ID-value in this Automaton graph -}
  , graphIdentifier : Uuid
  , root : NodeId
  }

-- type alias FlatTransition =
--   {
--     isFinal : Bool
--   , via : Char
--   }
-- type alias FlatConnection = AutoSet.Set String Transition -- a Connection is a link between two nodes.
-- type alias FlattenedGraph =
--   { graph : Graph Entity FlatConnection
--   , graphIdentifier : Uuid
--   , root : NodeId
--   }

{- So with an DFA, I can basically do anything that's deterministic.

Looping back to previous nodes? Go ahead.
Looping back to ourselves? Definitely!

Now with that said, nodes without outgoing edges SHOULD be terminal nodes.
And everything is connected.  And outgoing nodes have deterministic transitions.
-}

type alias DFARecord extending =
  { extending |
    states : IntDict Entity
  , transition_function: IntDict (AutoDict.Dict String AcceptVia NodeId) -- NodeId × Char → NodeId
  , start : NodeId
  , finals : Set NodeId
  }

type alias ExtDFA =
  { states : IntDict Entity
  , transition_function: IntDict (AutoDict.Dict String AcceptVia NodeId)
  , start : NodeId
  , w_dfa_orig : DFARecord {}
  , finals : Set NodeId
  , register : Set NodeId
  , clone_start : NodeId
  , queue_or_clone : List NodeId
  , unusedId : NodeId
  }

type alias TransitionTakenData =
  { dest : NodeId
  , matching : Transition
  }

type alias ExecutionData =
  { transitions : List TransitionTakenData
  , remainingData : List Char
  , currentNode : NodeId
  , computation : AutomatonGraph
  , resolutionDict : AutoDict.Dict String Uuid AutomatonGraph
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

empty : Uuid -> AutomatonGraph
empty uuid =
  { graph = Graph.empty
  , graphIdentifier = uuid
  , root = 0
  }

maxId : AutomatonGraph -> Int
maxId {graph} =
  Graph.nodeIdRange graph
  |> Maybe.map Tuple.second
  |> Maybe.withDefault 0

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
      |> String.left 6
      |> \s -> s ++ "…"

truncate_uuid : Uuid -> String
truncate_uuid uuid =
  (Uuid.toString uuid |> String.left 4) ++ "…"

-- honestly I don't know why this function is here 😅…
-- These kinds of functions are just calming and fun to make.
-- I never had an actual use for it, and probably never will…
-- uuid_string : Uuid -> String
-- uuid_string uuid =
--   let
--     lookup =
--       "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%^&*_=|`~§±¶÷×¤¢£¥©®™¿¡ØøÅåÇçÑñßÐðÞþŽžŠšŒœĐđĦħĲĳĿŀŁłŊŋŒœŦŧŊŋΘΛΞΣΦΨαβγδεζηθιλμξπρστυφωאבגדהוזחטיךכלםמןנסעףפץצקרשת٠١٢٣٤٥٦٧٨٩БГДЕЖЗИЙЛПУФЦЧШЪЫЭЮЯбвгдежзийклмнопрстуфхцчшщъыьэюя※Ω∆∞≠≈≡√∫∂↕⇑⇓♠♣♥♦♪♫"
--       |> String.toList
--       |> List.indexedMap (\i x -> (i, x))
--       |> Dict.fromList
--   in
--     ( Uuid.toString uuid
--       |> String.replace "-" ""
--       |> Binary.fromHex
--       |> Binary.chunksOf 8
--       |> List.map (\chunk -> Binary.toDecimal chunk)
--       |> List.filterMap (\i -> Dict.get i lookup)
--       |> List.take 4
--       |> String.fromList
--     ) ++ "…"

{-| True if at least one transition terminates at this node,
    within the given execution context -}
isTerminalNode : NodeContext a Connection -> Bool
isTerminalNode node =
  -- IntDict.isEmpty node.outgoing &&
  ( IntDict.foldl
    (\_ conn state ->
      state ||
        AutoSet.foldl
          (\t state_ -> state_ || .isFinal t)
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
transitionToString {via, isFinal} =
  let
    prevChar =
      if isFinal then
        case via of
          ViaCharacter _ ->
            "\u{0305}"
          ViaGraphReference _ ->
            "!"
      else
        ""
  in
    case via of
      ViaCharacter ch ->
        prevChar ++ String.fromChar ch
      ViaGraphReference uuid ->
        prevChar ++ Uuid.toString uuid

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

-- Parser for converting string representation to AutomatonGraph
-- Example: "0-!av-1 0-b!vk!z-2 2-p-0" -> [(0, "!av", 1), (0, "b!vk!z", 2), (2, "p", 0)]
transitionsParser : Parser (List (Int, String, Int))
transitionsParser =
    Parser.oneOf
        [ Parser.succeed []
            |. Parser.end
        , Parser.loop [] transitionsHelp
        ]

setX : Float -> { a | x : Float } -> { a | x : Float }
setX new_x node =
  { node | x = if isNaN new_x then node.x + 0.0023 else new_x }

setY : Float -> { a | y : Float } -> { a | y : Float }
setY new_y node =
  { node | y = if isNaN new_y then node.y + 0.002 else new_y }

setXY : Float -> Float -> { a | x : Float, y : Float } -> { a | x : Float, y : Float }
setXY new_x new_y node =
  node |> setX new_x |> setY new_y

------------------------------------
-- BEGIN :: Copied & adapted from Force.elm
------------------------------------
{-| This is a convenience function for wrapping data up as Entities. The initial position of entities is arranged
in a [phylotaxic pattern](https://elm-visualization.netlify.app/Petals/). Goes well with `List.indexedMap`.
-}
entity : Int -> NodeEffect -> Entity
entity index v =
  let
    initialRadius = 10
    initialAngle =
        pi * (3 - sqrt 5)
    radius =
        sqrt (0.5 + toFloat index) * initialRadius

    angle =
        toFloat index * initialAngle
  in
    { x = 0.1
    , y = 0.1
    , vx = 0.0
    , vy = 0.0
    , id = index
    , effect = v
    } |> setXY (radius * cos angle) (radius * sin angle)
------------------------------------
-- END :: Copied & adapted from Force.elm
------------------------------------

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
    'A' -> Just 10
    'B' -> Just 11
    'C' -> Just 12
    'D' -> Just 13
    'E' -> Just 14
    'F' -> Just 15
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

uuidFromHash : Binary.Bits -> Maybe Uuid
uuidFromHash =
  let
    mask = -- "xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx"
      "000000000000F000C000000000000000"
      |> Binary.fromHex
      |> Binary.not
    formatBits =
      Binary.fromHex "00000000000040008000000000000000"
  in
    -- take the first 128 bits only
    Binary.shiftRightZfBy 128
    >> Binary.dropLeadingZeros
    -- mask off the format bits, and replace them
    >> Binary.and mask
    >> Binary.or formatBits
    -- convert to the correct string format
    >> Binary.toHex
    >> (\s ->
          let
            (a, a_rest) = (String.left 8 s, String.dropLeft 8 s)
            (b, b_rest) = (String.left 4 a_rest, String.dropLeft 4 a_rest)
            (c, c_rest) = (String.left 4 b_rest, String.dropLeft 4 b_rest)
            (d, d_rest) = (String.left 4 c_rest, String.dropLeft 4 c_rest)
            e = String.left 12 d_rest
          in
            a ++ "-" ++ b ++ "-" ++ c ++ "-" ++ d ++ "-" ++ e
      )
    >> Uuid.fromString

charToUuid : Char -> Maybe Uuid
charToUuid ch =
  Binary.fromStringAsUtf8 (String.fromChar ch)
  |> SHA.sha256
  |> uuidFromHash
