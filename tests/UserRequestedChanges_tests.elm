module UserRequestedChanges_tests exposing (..)
import Expect
import Test exposing (..)
import Automata.DFA exposing (splitTerminalAndNonTerminal)
import Utility exposing (dfa, ag_equals, dfa_equals, mkAG_input)
import ForceDirectedGraph exposing (applyChangesToGraph)
import ForceDirectedGraph exposing (RequestedGraphChanges)
import ForceDirectedGraph exposing (create_removal_userchange)
import Graph exposing (NodeId)
import Automata.Data exposing (AutomatonGraph)
import Automata.DFA exposing (mkAutomatonGraphWithValues)
import ForceDirectedGraph exposing (Entity)

ag : String -> AutomatonGraph Entity
ag =
  mkAG_input
  >> mkAutomatonGraphWithValues (\id -> { x = 0, y = 0, vx = 0, vy = 0, id = id, value = {} })

check_remove : NodeId -> NodeId -> String -> String -> Expect.Expectation
check_remove src dest s_ag s_expected =
  case create_removal_userchange src dest (ag s_ag) of
    Nothing ->
      Expect.fail <| "Removal userchange couldn't be created for " ++ s_ag
    Just change ->
      ag_equals
        (ag s_expected)
        (applyChangesToGraph [change] (ag s_ag))

suite : Test
suite =
  describe "Application of UserRequestedChanges"
    [ describe "Removal" <|
      [ test "Removing a link between two nodes leaves me with one node" <|
        \_ ->
        check_remove
          0 1 "0-a-1" -- removal and graph to remove from
          "" -- expected
      ]
    ]