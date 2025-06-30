module AutomatonGraph_tests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Automata.Data as D
import Automata.Verification as D
import List.Extra as List
import Automata.DFA
import Automata.DFA exposing (mkAutomatonGraph)
import Automata.DFA exposing (fromAutomatonGraph)
import Automata.DFA exposing (mkDFA)
import Automata.DFA exposing (toAutomatonGraph)
import Parser exposing (Parser, (|.), (|=))
import Automata.Data exposing (AutomatonGraph)
import Automata.DFA exposing (splitTerminalAndNonTerminal)
import Automata.DFA exposing (renumberAutomatonGraph)
import Graph
import Set


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

ag : String -> AutomatonGraph ()
ag = mkAutomatonGraph << mkAG_input

parseTransitions_suite : Test
parseTransitions_suite =
  describe "String to transitions parsing"
    [ test "parses simple transitions correctly" <|
      \_ ->
        Expect.equal
          [(0, "!av", 1), (0, "b!vk!z", 2), (2, "p", 0)]
          (mkAG_input "0-!av-1 0-b!vk!z-2 2-p-0")
    , test "parses single transition" <|
      \_ ->
        Expect.equal
          [(1, "abc", 2)]
          (mkAG_input "1-abc-2")
    , test "handles empty string" <|
      \_ ->
        Expect.equal [] (mkAG_input "")
    ]


-- toAG_suite : Test
-- toAG_suite =
--   describe "AutomatonGraph→DFA conversion"
--     [ describe "involves subset construction"
--       [ test "with one node" <|
--           \_ ->
--             let
--               g = mkAutomatonGraph [(0, "a", 1), (0, "a", 2), (1, "b", 2), (1, "k", 1), (1, "k", 0), (2, "!c", 3)]
--               dfa = fromAutomatonGraph g
--               expected = mkDFA [(0, 'a', 5), (2, 'c', 3), (4, 'a', 5), (4, 'b', 2), (4, 'k', 4), (5, 'b', 2), (5, 'c', 3), (5, 'k', 4)] [3]
--             in
--               Expect.equal expected dfa
--       ]
--     , describe "splits terminal and non-terminal nodes correctly"
--         [ test "basic recursive loop" <|
--             \_ ->
--               let
--                 g = mkAutomatonGraph [(0, "!a", 1), (1, "b", 1)]
--                 dfa = fromAutomatonGraph g
--                 expected = mkDFA [(0, 'a', 1), (1, 'b', 2), (2, 'b', 2)] [1]
--               in
--                 Expect.equal expected dfa
--         , test "basic connection with terminal & non-terminal" <|
--             \_ ->
--               let
--                 g = mkAutomatonGraph [(0, "!a", 1), (0, "b", 1)]
--                 dfa = fromAutomatonGraph g
--                 expected = mkDFA [(0, 'a', 1), (0, 'b', 2)] [1]
--               in
--                 Expect.equal expected dfa
--         ]
--     ]

ag_equals : AutomatonGraph a -> AutomatonGraph a -> Expect.Expectation
ag_equals g1 g2 =
  let
    normalise =
      renumberAutomatonGraph
      >> \g ->
        ( Graph.nodeIds g.graph |> Set.fromList
        , Graph.edges g.graph
          |> List.map (\e -> (e.from, e.to, Set.toList e.label))
          |> Set.fromList
        , g.root
        )
    (a1, b1, c1) = normalise g1
    (a2, b2, c2) = normalise g2
  in
    if a1 /= a2 then
      Expect.equalSets a1 a2
    else if b1 /= b2 then
      Expect.equalSets b1 b2
    else
      Expect.equal c1 c2

fromAG_suite : Test
fromAG_suite =
  describe "AutomatonGraph→DFA conversion"
    {- this involves several stages.

    1. Splitting into "terminal" and "non-terminal" nodes, such that
       "terminal" nodes receive only terminal connections, and "non-terminal"
       nodes receive only non-terminal connections.

       We do this because in the traditional DFA formalism, finality
       information is carried by the state and not the transition.  So, in
       preparation, the graph will reflect it.

    2. Conversion of NFA-graph to DFA-graph, using (fairly standard) subset
       construction.  This has several phases to it.

    3. Conversion of graph, which is now in DFA-friendly form, into DFA data
       structure.
    -}
    [ describe "splitting terminal & non-terminal works"
      [ describe "simple cases; should not split"
        [ test "two-node link, terminal only" <|
          \_ ->
            ag_equals
              (ag "0-!a-1")
              (splitTerminalAndNonTerminal <| ag "0-!a-1")
        , test "two-node link, non-terminal only" <|
          \_ ->
            ag_equals
              (ag "0-a-1")
              (splitTerminalAndNonTerminal <| ag "0-a-1")
        , test "two-node link, recursive on 0" <|
          \_ ->
            ag_equals
              (ag "0-a-1 0-b-0")
              (splitTerminalAndNonTerminal <| ag "0-a-1 0-b-0")
        , test "two-node link, recursive on 1" <|
          \_ ->
            ag_equals
              (ag "0-a-1 1-b-1")
              (splitTerminalAndNonTerminal <| ag "0-a-1 1-b-1")
        , test "one node, recursive" <|
          \_ ->
            ag_equals
              (ag "0-a-0")
              (splitTerminalAndNonTerminal <| ag "0-a-0")
        ]
      , describe "simple splitting cases"
        [ test "two nodes basic" <|
          \_ ->
            ag_equals
              (ag "0-!a-1 0-b-2")
              (splitTerminalAndNonTerminal <| ag "0-!ab-1")
        , test "two nodes, >1 non-terminal" <|
          \_ ->
            ag_equals
              (ag "0-!a-1 0-bc-2")
              (splitTerminalAndNonTerminal <| ag "0-!abc-1")
        , test "two nodes, >1 terminal" <|
          \_ ->
            ag_equals
              (ag "0-!a!b-1 0-c-2")
              (splitTerminalAndNonTerminal <| ag "0-!a!bc-1")
        , test "one node, recursive" <|
          \_ ->
            ag_equals
              (ag "0-!a-0 0-b-1 1-b-1 1-!a-0")
              (splitTerminalAndNonTerminal <| ag "0-!ab-0")
        , test "two nodes, first is recursive" <|
          \_ ->
            ag_equals
              (ag "0-!a-0 0-b-1 1-b-1 0-c-2 1-c-2 1-!a-0")
              (splitTerminalAndNonTerminal <| ag "0-!ab-0 0-c-1")
        , test "separate links heading to one node" <|
          \_ ->
            ag_equals
              (ag "0-a-1 0-b-2 1-c-3 2-!d-4")
              (splitTerminalAndNonTerminal <| ag "0-a-1 0-b-2 1-c-3 2-!d-3")
        ]

      ]
    , describe "handles edge cases"
      [ test "with one node" <|
          \_ ->
            let
              g = mkAutomatonGraph [(0, "a", 1), (0, "a", 2), (1, "b", 2), (1, "k", 1), (1, "k", 0), (2, "!c", 3)]
              expected = mkDFA [(0, 'a', 5), (2, 'c', 3), (4, 'a', 5), (4, 'b', 2), (4, 'k', 4), (5, 'b', 2), (5, 'c', 3), (5, 'k', 4)] [3]
            in
              Expect.equal expected (fromAutomatonGraph g)
      ]
    , describe "splits terminal and non-terminal nodes correctly"
        [ test "basic recursive loop" <|
            \_ ->
              let
                g = mkAutomatonGraph [(0, "!a", 1), (1, "b", 1)]
                expected = mkDFA [(0, 'a', 1), (1, 'b', 2), (2, 'b', 2)] [1]
              in
                Expect.equal expected (fromAutomatonGraph g)
        , test "basic connection with terminal & non-terminal" <|
            \_ ->
              let
                g = mkAutomatonGraph [(0, "!a", 1), (0, "b", 1)]
                expected = mkDFA [(0, 'a', 1), (0, 'b', 2)] [1]
              in
                Expect.equal expected (fromAutomatonGraph g)
        ]
    ]