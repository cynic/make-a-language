module Utility exposing (ag, dfa, mkAG_input, mkDFA_input, ag_equals, dfa_equals)
import Parser exposing (Parser, (|=), (|.))
import Test exposing (..)
import Automata.Data exposing (AutomatonGraph, transitionsToString)
import Automata.DFA exposing (mkAutomatonGraph, DFARecord, mkDFA, renumberAutomatonGraph)
import Expect
import Set
import Graph
import IntDict
import List
import Dict

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
