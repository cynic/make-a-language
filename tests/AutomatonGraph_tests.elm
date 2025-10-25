module AutomatonGraph_tests exposing (..)
import Test exposing (..)
import Automata.DFA exposing
  ( fromAutomatonGraph, splitTerminalAndNonTerminal, nfaToDFA
  , fromAutomatonGraphHelper, minimiseNodesByCombiningTransitions
  , expand
  )
import Utility exposing (ag, dfa, ag_equals, dfa_equals)
import ForceDirectedGraph exposing (applyChangesToGraph)
import AutoDict exposing (Dict)
import Uuid

toAG_suite : Test
toAG_suite =
  describe "DFA→AutomatonGraph"
    [ let
        mk s =
          minimiseNodesByCombiningTransitions (ag s)
          -- |> finaliseEndNodes -- this will only be called at the end of all user-changes.
      in
      describe "node minimisation"
      [ test "non-terminal node is extended by terminal node" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-!b-1")
            (mk "0-a-1 1-!b-2 2-!b-2")
      , test "terminal node is extended by non-terminal node" <|
        \_ ->
          ag_equals
            (ag "0-!a-1 1-b-1")
            (mk "0-!a-1 1-b-2 2-b-2")
      , test "joining split mid-nodes" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-d!b-2 2-c-3")
            (mk "0-a-1 1-!b-2 2-c-3 1-d-4 4-c-3")
      , test "joining split end-nodes" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-!bc-2")
            (mk "0-a-1 1-c-3 1-!b-2")
      , test "terminal node extended by non-terminal node, with tail" <|
        \_ ->
          ag_equals
            (ag "0-!a-1 1-b-1 1-c-2")
            (mk "0-!a-1 1-b-2 2-b-2 2-c-3 1-c-3")
      , test "non-terminal node extended by terminal node, with tail" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-!b-1 1-c-2")
            (mk "0-a-1 1-!b-2 2-!b-2 2-c-3 1-c-3")
      , test "combined recursion and splitting" <|
        \_ ->
          ag_equals
            (ag "0-c!a-1 1-b-1 1-d-2")
            (mk "0-!a-1 0-c-2 1-b-2 2-b-2 1-d-3 2-d-3")
      -- , describe "edge-cases and bug-fixes"
      --   [ test "Case Ⅰ" <|
      --     \_ ->
      --       ag_equals
      --         (ag "0-a-1 1-i-2 2-p-3")
      --         (mk "0-a-1 1-i-2 2-p-3 2-p-6 3-!q-4 4-z-2 4-v-3 4-p-6 6-q-7 6-!o-9 7-!v-6 7-!z-8")
      --   ]
      ]
    ]

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
              (ag "2-!a-0 2-b-1 0-!a-0 0-b-1 1-b-1 1-!a-0")
              (splitTerminalAndNonTerminal <| ag "0-!ab-0")
        , test "two nodes, first is recursive" <|
          \_ ->
            ag_equals
              (ag "3-!a-0 3-b-2 3-c-1 2-b-2 2-c-1 2-!a-0 0-b-2 0-c-1 0-!a-0")
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
        , test "separate links for a more complex composite [case Ⅰ]" <|
          \_ ->
            ag_equals
              (ag "0-!a-1 0-c-2 1-b-2 2-b-2 1-d-3 2-d-3")
              (splitTerminalAndNonTerminal <| ag "0-c!a-1 1-b-1 1-d-2 [case Ⅱ]")
        , test "separate links for a more complex composite [case Ⅱ]" <|
          \_ ->
            ag_equals
              (ag "2-!a-0 0-k-2 0-kz-3 1-k-2 1-kz-3 2-b-1")
              (splitTerminalAndNonTerminal <| ag "0-!ab-1 1-kz-2 1-k-0")
        , test "Root must always be regarded as non-final" <|
          \_ ->
            let
              ag = Utility.mkAutomatonGraph [ (0, "!x", 0) ]
              conv = fromAutomatonGraph ag
            in
              Utility.dfa_equals
                (Utility.mkDFA [ (0, 'x', 1), (1, 'x', 1) ] [1])
                conv
        ]
      ]
    , describe "NFA→DFA conversion"
      [ test "three nodes, one identical backlink" <|
        \_ ->
          ag_equals
            (ag "0-a-1 1-b-0")
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
            (ag "0-a-1 1-!b-0")
            (nfaToDFA <| ag "0-a-1 1-b-2 1-!b-0")
      , test "two nodes, recursive on first" <|
        \_ ->
          ag_equals
            (ag "0-k-0")
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
      , test "three nodes, one backlink" <|
        \_ ->
          ag_equals
            (ag "0-!ab-1 1-k-0 1-z-2")
            (nfaToDFA <| ag "0-!ab-1 1-kz-2 1-k-0")
      , test "do not merge equivalent rows unless they have the same terminality" <|
        \_ ->
          ag_equals
            (ag "0-b-8 0-!a-1 1-k-0 8-k-0 8-z-2 1-z-2")
            (nfaToDFA <| ag "0-b-8 0-!a-1 1-k-0 8-k-0 8-kz-2 1-kz-2")
      , describe "Crafting step (post terminality-splitting, post NFA→DFA)"
        [ test "different transitions to same endpoint" <|
          \_ ->
            dfa_equals 
              (dfa "0-z-1 1-p-2 1-q-2" [])
              (fromAutomatonGraphHelper <| ag "0-z-1 1-pq-2")
        ]
      , describe "Full conversion"
        [ describe "edge cases & bug-fixes"
          [ test "with one node" <|
            \_ ->
              dfa_equals
                (dfa "0-a-5 2-c-3 4-a-5 4-b-2 4-k-4 5-b-2 5-c-3 5-k-4" [3])
                (fromAutomatonGraph (ag "0-a-1 0-a-2 1-b-2 1-k-1 1-k-0 2-!c-3"))
          , test "Case Ⅰ" <|
            \_ ->
              ag_equals
                (ag "0-a-1 1-i-12 6-q-7 6-!o-8 7-!v-6 7-!z-8 12-p-13 13-!o-8 13-!q-14 14-p-6 14-!z-12 14-!v-13")
                (applyChangesToGraph <| ag "0-a-1 1-i-2 2-p-3 2-p-6 3-!q-4 4-z-2 4-v-3 4-p-6 6-q-7 6-!o-9 7-!v-6 7-!z-8")
          , test "Case Ⅱ" <|
            \_ ->
              ag_equals
                (ag "0-p-6 6-!q-8 8-!v-6 8-p-5 5-q-4 4-!v-5")
                (applyChangesToGraph <| ag "0-p-1 1-!q-2 2-v-1 2-p-3 0-p-3 3-q-4 4-!v-3")
          ]
        ]
      ]
    ]

node_expansion : Test
node_expansion =
  describe "Node expansion"
  [ test "No expansion needed (no terminals)" <|
    \_ ->
      ag_equals
        (ag "0-a-1")
        (expand (ag "0-a-1") (AutoDict.empty Uuid.toString) 0)
  , test "No expansion needed (one terminal)" <|
    \_ ->
      ag_equals
        (ag "0-!a-1")
        (expand (ag "0-!a-1") (AutoDict.empty Uuid.toString) 0)
  ]