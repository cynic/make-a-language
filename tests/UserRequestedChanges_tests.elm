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
import Automata.Data exposing (Transition)
import ForceDirectedGraph exposing (create_update_userchange)
import Set
import ForceDirectedGraph exposing (create_union_userchange)

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

check_update : NodeId -> NodeId -> List Transition -> String -> String -> Expect.Expectation
check_update src dest connList s_ag s_expected =
  case create_update_userchange src dest (Set.fromList connList) (ag s_ag) of
    Nothing ->
      Expect.fail <| "Update userchange couldn't be created for " ++ s_ag
    Just change ->
      ag_equals
        (ag s_expected)
        (applyChangesToGraph [change] (ag s_ag))

check_creating : NodeId -> String -> String -> String -> Expect.Expectation
check_creating new_node s_new_graph s_original_graph s_expected =
  let
    change = create_union_userchange new_node (ag s_new_graph)
  in
    ag_equals
      (ag s_expected)
      (applyChangesToGraph [change] (ag s_original_graph))
  

suite : Test
suite =
  describe "Application of UserRequestedChanges"
    [ describe "Removal" <|
      [ test "Removing a link between two nodes leaves me with one node" <|
        \_ ->
          check_remove
            0 1 "0-a-1" -- removal and graph to remove from
            "" -- expected
      , test "Removing the middle link in a 4-link chain leaves me with 2 nodes" <|
        \_ ->
          check_remove
            1 2 "0-a-1 1-b-2 2-c-3"
            "0-a-1"
      ]
    , describe "Updating" <|
      [ describe "Changing an existing link"
        [ test "Changing a link completely works" <|
          \_ ->
            check_update
              0 1 [('b', 0)] "0-a-1"
              "0-b-1"
        , test "Adding transition to a link works" <|
          \_ ->
            check_update
              0 1 [('b', 0), ('a', 0)] "0-a-1"
              "0-ab-1"
        , test "Removing transition from a link works" <|
          \_ ->
            check_update
              0 1 [('a', 0)] "0-ab-1"
              "0-a-1"
        , test "Adding and removing from a link works" <|
          \_ ->
            check_update
              0 1 [('a', 0),('c', 0)] "0-ab-1"
              "0-ac-1"
        , test "Changing terminality of a link works" <|
          \_ ->
            check_update
              0 1 [('a', 1)] "0-a-1"
              "0-!a-1"
        , test "Partially changing terminality of a link works" <|
          \_ ->
            check_update
              0 1 [('a', 0), ('b', 1)] "0-ab-1"
              "0-a!b-1"
        ]
      , describe "Creating a new link"
        [ test "Can create a link between two existing nodes" <|
          \_ ->
            check_update
              1 3 [('k', 0)] "0-a-1 1-b-2 2-c-3"
              "0-a-1 1-b-2 2-c-3 1-k-3"
        , test "Can create a link to source" <|
          \_ ->
            check_update
              2 0 [('k', 0)] "0-a-1 1-b-2"
              "0-a-1 1-b-2 2-k-0"
        , test "Can create a link from source" <|
          \_ ->
            check_update
              0 2 [('k', 0)] "0-a-1 1-b-2"
              "0-a-1 1-b-2 0-k-2"
        ]
      ]
    , describe "Creating" <|
      [ test "Can create a new node from the source" <|
        \_ ->
          check_creating
            1 "0-a-1" ""
            "0-a-1"
      , test "Can create a new node that isn't from the source" <|
        \_ ->
          check_creating
            2 "0-a-1 1-b-2" "0-a-1"
            "0-a-1 1-b-2"
      , describe "Cases where merging happens"
        [ test "Replicating a link causes a merge" <|
          \_ ->
            check_creating
              2 "0-a-1 0-a-2" "0-a-1"
              "0-a-1"
        , test "Two same-terminality nodes going to the same end are merged [case Ⅰ]" <|
          \_ ->
            check_creating
              2 "0-a-1 0-b-2" "0-a-1"
              "0-ab-1"
        , test "Two same-terminality nodes going to the same end are merged [case Ⅱ]" <|
          \_ ->
            check_creating
              2 "0-!a-1 0-!b-2" "0-!a-1"
              "0-!a!b-1"
        , test "Two different-terminality nodes going to the same end are merged [case Ⅰ]" <|
          \_ ->
            check_creating
              2 "0-a-1 0-!b-2" "0-a-1"
              "0-a!b-1"
        , test "Two different-terminality nodes going to the same end are merged [case Ⅱ]" <|
          \_ ->
            check_creating
              2 "0-!a-1 0-b-2" "0-!a-1"
              "0-!ab-1"
        ]
      ]
    ]