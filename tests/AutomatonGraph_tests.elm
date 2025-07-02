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
import Http exposing (Expect)
import Automata.DFA exposing (nfaToDFA)
import Automata.Data exposing (transitionsToString)
import Automata.DFA exposing (fromAutomatonGraphHelper)
import Automata.DFA exposing (DFARecord)
import IntDict
import Dict
import Automata.DFA exposing (minimiseNodesByCombiningTransitions)


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

ag : String -> AutomatonGraph ()
ag = mkAutomatonGraph << mkAG_input

dfa : String -> List Int -> Automata.DFA.DFARecord {} ()
dfa s f = mkDFA_input s |> \xs -> mkDFA xs f

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


toAG_suite : Test
toAG_suite =
  describe "DFA→AutomatonGraph conversion"
    [ let
        mk xs s =
          minimiseNodesByCombiningTransitions (Set.fromList xs) (ag s)
      in
      describe "node minimisation"
      [ test "non-terminal node is extended by terminal node" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-!b-1")
            (mk [2] "0-a-1 1-!b-2 2-!b-2")
      , test "terminal node is extended by non-terminal node" <|
        \_ ->
          ag_equals
            (ag "0-!a-1 1-b-1")
            (mk [2] "0-!a-1 1-b-2 2-b-2")
      , test "joining split mid-nodes" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-d!b-2 2-c-3")
            (mk [2] "0-a-1 1-!b-2 2-c-3 1-d-4 4-c-3")
      , test "joining split end-nodes" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-!bc-2")
            (mk [2] "0-a-1 1-c-3 1-!b-2")
      , test "terminal node extended by non-terminal node, with tail" <|
        \_ ->
          ag_equals
            (ag "0-!a-1 1-b-1 1-c-2")
            (mk [1] "0-!a-1 1-b-2 2-b-2 2-c-3 1-c-3")
      , test "non-terminal node extended by terminal node, with tail" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-!b-1 1-c-2")
            (mk [1] "0-a-1 1-!b-2 2-!b-2 2-c-3 1-c-3")
      , test "combined recursion and splitting" <|
        \_ ->
          ag_equals
            (ag "0-c!a-1 1-b-1 1-d-2")
            (mk [1] "0-!a-1 0-c-2 1-b-2 2-b-2 1-d-3 2-d-3")
      -- , describe "edge-cases and bug-fixes"
      --   [ test "Case Ⅰ" <|
      --     \_ ->
      --       ag_equals
      --         (ag "0-z-1 1-pq-2")
      --         (mk [] "0-z-1 0-z-2 1-p-3 2-q-4")
      --   ]
      ]
    ]

ag_equals : AutomatonGraph a -> AutomatonGraph a -> Expect.Expectation
ag_equals g1 g2 =
  let
    normalise =
      renumberAutomatonGraph
      >> \g ->
        ( Graph.nodeIds g.graph |> Set.fromList
        , Graph.edges g.graph
          |> List.map (\e -> (e.from, Set.toList e.label |> transitionsToString, e.to))
          |> Set.fromList
        , g.root
        )
    (a1, b1, c1) = normalise g1
    (a2, b2, c2) = normalise g2
  in
    if b1 /= b2 then
      Expect.equalSets b1 b2
    else if a1 /= a2 then
      Expect.equalSets a1 a2
    else
      Expect.equal c1 c2

dfa_equals : DFARecord a b -> DFARecord a b -> Expect.Expectation
dfa_equals dfa1 dfa2 =
  let
    states1 = IntDict.keys dfa1.states |> Set.fromList
    states2 = IntDict.keys dfa2.states |> Set.fromList
    transitions1 =
      IntDict.toList dfa1.transition_function
      |> List.map (\(a, d) -> (a, Dict.toList d))
      |> Set.fromList
    transitions2 =
      IntDict.toList dfa2.transition_function
      |> List.map (\(a, d) -> (a, Dict.toList d))
      |> Set.fromList
  in
    if transitions1 /= transitions2 then
      Expect.equalSets transitions1 transitions2
    else if states1 /= states2 then
      Expect.equalSets states1 states2
    else if dfa1.finals /= dfa2.finals then
      Expect.equalSets dfa1.finals dfa2.finals
    else
      Expect.equal dfa1.start dfa2.start

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

       At this stage, we don't actually care about the _characters_ of the
       transitions at all.  It's not important at all.  All we care about is
       terminality of transitions.

    2. Conversion of NFA-graph to DFA-graph, using (fairly standard) subset
       construction.  At this stage, we care about the characters of the
       transitions.  And we are also GUARANTEED, because of the previous
       phase, that no node will have BOTH terminal & non-terminal incoming
       transitions.

       The phases of this are:
       1. Getting the inital set of rows.  We can find these from the NFA.
          If we run into a node with the same character leading to >1
          transition, we remember it as one that we need to look at later on.

       2. We recursively build the new subset construction table, based on
          what going through, looking at the "combination" states we haven't
          yet handled, handling them, and then repeating until we don't have
          any new states generated.

          (At this point, subset construction is actually, technically, DONE!)

       3. Optimisation: we merge cells with identical states.  This is actually
          interesting, because by changing our condition for what is
          "identical", we can merge more thoroughly or more loosely… anyway!

       4. Get rid of unreachable states.  We do this by following from the root
          and seeing what we DON'T pass through.

       5. Rename the final states.

       6. Use the table to generate the DFA.

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
        , test "separate links heading to one node, with an outgoing tail" <|
          \_ ->
            ag_equals
              (ag "0-a-1 0-b-2 1-c-3 2-!d-4 3-e-5 4-e-5")
              (splitTerminalAndNonTerminal <| ag "0-a-1 0-b-2 1-c-3 2-!d-3 3-e-4")
        , test "separate links heading to one recursive node, outgoing tail" <|
          \_ ->
            ag_equals
              (ag "0-a-1 0-b-2 1-c-3 2-!d-4 3-e-3 4-e-3 3-f-5 4-f-5")
              (splitTerminalAndNonTerminal <| ag "0-a-1 0-b-2 1-c-3 2-!d-3 3-e-3 3-f-4")
        , test "separate links for a more complex composite" <|
          \_ ->
            ag_equals
              (ag "0-!a-1 0-c-2 1-b-2 2-b-2 1-d-3 2-d-3")
              (splitTerminalAndNonTerminal <| ag "0-c!a-1 1-b-1 1-d-2")
        ]
      ]
    , describe "NFA→DFA conversion"
      [ test "three nodes, one identical backlink" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-b-2 2-a-1")
            (nfaToDFA <| ag "0-a-1 1-b-2 1-b-0")
      -- these next two are a bit counter-intuitive!
      -- BUT: think of where each transition lands!
      , test "three nodes, terminal tail, one non-terminal backlink" <|
        \_ ->
          ag_equals
            -- after register-and-replace, this would be minimised to
            -- "0-a-1 1-!b-0"
            (ag "0-a-1 1-!b-2 2-a-1")
            (nfaToDFA <| ag "0-a-1 1-!b-2 1-b-0")
      , test "three nodes, non-terminal tail, one terminal backlink" <|
        \_ ->
          ag_equals
            -- after register-and-replace, this would be minimised to
            -- "0-a-1 1-!b-0"
            (ag "0-a-1 1-!b-2 2-a-1")
            (nfaToDFA <| ag "0-a-1 1-b-2 1-!b-0")
      , test "two nodes, recursive on first" <|
        \_ ->
          ag_equals
            -- after register-and-replace, this would be more minimised.
            (ag "0-k-1 1-k-1")
            (nfaToDFA <| ag "0-k-1 0-k-0")
      , test "four nodes, different transitions to same endpoint" <|
        \_ ->
          ag_equals
            (ag "0-z-1 1-pq-2")
            (nfaToDFA <| ag "0-z-1 0-z-2 1-p-3 2-q-3")
      , test "four nodes, different transitions to different endpoints" <|
        \_ ->
          ag_equals
            (ag "0-z-1 1-!p-2 1-q-3")
            (nfaToDFA <| ag "0-z-1 0-z-2 1-!p-3 2-q-4")
      ]
    , describe "Crafting step (post terminality-splitting, post NFA→DFA)"
      [ test "different transitions to same endpoint" <|
        \_ ->
          dfa_equals 
            (dfa "0-z-1 1-p-2 1-q-2" [])
            (fromAutomatonGraphHelper <| ag "0-z-1 1-pq-2")
      ]
    , describe "Full conversion"
      [ describe "edge cases"
        [ test "with one node" <|
          \_ ->
            Expect.equal
              (dfa "0-a-5 2-c-3 4-a-5 4-b-2 4-k-4 5-b-2 5-c-3 5-k-4" [3])
              (fromAutomatonGraph <| ag "0-a-1 0-a-2 1-b-2 1-k-1 1-k-0 2-!c-3")
        ]
      ]
    ]