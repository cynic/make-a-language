module UserRequestedChanges_tests exposing (..)
import Expect
import Test exposing (..)
import Utility exposing (ag_equals, dummy_uuid)
import ForceDirectedGraph exposing (applyChangesToGraph)
import Graph exposing (NodeId)
import Automata.Data exposing (AutomatonGraph, Entity, mkAG_input)
import Set
import ForceDirectedGraph exposing (Msg(..))
import Automata.Data exposing (Connection, NodeEffect(..))
import Automata.Debugging exposing (debugAutomatonGraph)
import ForceDirectedGraph exposing (updateLink_graphchange)
import ForceDirectedGraph exposing (removeLink_graphchange)
import Automata.DFA exposing (renumberAutomatonGraph)
import Dict
import AutoSet
import List.Extra as List

{- REGARDING THE ag FUNCTION

So after struggling with the types for a while—they look 100% correct
to me and I'm pretyt sure they ARE correct!—I discovered that I'd run
into a compiler bug https://github.com/elm/compiler/issues/1839 .

  elm: You ran into a compiler bug. Here are some details for the developers:

      a [rank = 2]

  Please create an <http://sscce.org/> and then report it
  at <https://github.com/elm/compiler/issues>


  CallStack (from HasCallStack):
    error, called at compiler/src/Type/Solve.hs:206:15 in main:Type.Solve

    … and this has been an open bug since 2018 😱!

So, I'm basically going to try to work around that by expanding
things inline until I get something that works.
-}
mkConn = Utility.mkConn dummy_uuid

ag : String -> AutomatonGraph
ag s_ =
  let
    ts = mkAG_input s_
    edges =
      List.foldl
        (\(src, s, dest) acc ->
          Dict.update (src, dest)
            (\item ->
              case item of
                Nothing ->
                  Just (mkConn s)
                Just existing ->
                  Just <| AutoSet.union existing (mkConn s)
            )
            acc
        )
        Dict.empty
        ts
      |> Dict.toList
      |> List.map (\((src, dest), conn) -> Graph.Edge src dest conn)
    nodes : List (Graph.Node Entity)
    nodes =
      List.foldl
        (\(src, _, dest) acc -> Set.insert src acc |> Set.insert dest)
        Set.empty
        ts
      |> Set.toList
      |> List.map (\id -> { id = id, label = { id = id, x = 0, y = 0, vx = 0, vy = 0, effect = NoEffect } })
  in
    case nodes of
      [] ->
        { graph = Graph.fromNodesAndEdges [{ id = 0, label = { id = 0, x = 0, y = 0, vx = 0, vy = 0, effect = NoEffect } }] []
        , graphIdentifier = dummy_uuid
        , root = 0
        , maxId = 0
        }
      _ ->
        { graph =
            Graph.fromNodesAndEdges nodes edges
        , graphIdentifier = dummy_uuid
        , root =
            case ts of
              (src, _, _)::_ -> src
              _ -> 0
        , maxId = List.maximumBy .id nodes |> Maybe.map .id |> Maybe.withDefault 0
        }

-- very thin convenience wrapper to get rid of the "x" and "y" components
newnode_change : NodeId -> Connection -> AutomatonGraph -> AutomatonGraph
newnode_change src conn g =
  ForceDirectedGraph.newnode_graphchange src 0 0 conn g

check_remove : NodeId -> NodeId -> String -> String -> Expect.Expectation
check_remove src dest s_ag s_expected =
  ag_equals
    (ag s_expected)
    (applyChangesToGraph <| removeLink_graphchange src dest (ag s_ag))

check_update : NodeId -> NodeId -> String -> String -> String -> Expect.Expectation
check_update src dest s_conn s_ag s_expected =
  ag_equals
    (ag s_expected)
    (applyChangesToGraph <| updateLink_graphchange src dest (mkConn s_conn) (ag s_ag))

check_creating : String -> NodeId -> String -> String -> Expect.Expectation
check_creating s_original src s_conn s_expected =
  ag_equals
    (ag s_expected)
    (applyChangesToGraph <| newnode_change src (mkConn s_conn) (ag s_original))

{-
This works, using node-id values, because all the user-changes are only submitted to the
backend for processing at the **END** of the whole process (i.e. during applyChangesToGraph).
Before that, we are working exclusively and only on the user-graph, where any changes
that we make are simply accepted verbatim and no node-ids are changed at any point.
-}
check_multi : String -> List (AutomatonGraph -> AutomatonGraph) -> String -> Expect.Expectation
check_multi s_start changes s_expected =
  let
    start = ag s_start
    expected = ag s_expected
    check_multi_helper : List (AutomatonGraph -> AutomatonGraph) -> Int -> AutomatonGraph -> Expect.Expectation
    check_multi_helper remaining idx acc =
      case remaining of
        [] -> Expect.fail "No changes were specified in the test."
        f::rest ->
          let
            new = f acc
          in
            case rest of
              [] ->
                debugAutomatonGraph ("◉◉◉ User-modified graph, post-#" ++ String.fromInt idx) new |> \_ ->
                ag_equals
                  expected
                  (applyChangesToGraph new |> renumberAutomatonGraph)
              _ ->
                check_multi_helper
                  rest
                  (idx + 1)
                  (new |> debugAutomatonGraph ("◉◉◉ User-modified graph, post-#" ++ String.fromInt idx))
  in
    check_multi_helper changes 0 start

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
      , test "Removing a recursive link works" <|
        \_ ->
          check_remove
            1 1 "0-a-1 1-b-1"
            "0-a-1"
      , test "Removing a recursive link, identical to an input link, works" <|
        \_ ->
          check_remove
            1 1 "0-a-1 1-a-1"
            "0-a-1"
      ]
    , describe "Updating" <|
      [ describe "Changing an existing link"
        [ test "Changing a link completely works" <|
          \_ ->
            check_update
              0 1 "b" "0-a-1"
              "0-b-1"
        , test "Adding transition to a link works" <|
          \_ ->
            check_update
              0 1 "ab" "0-a-1"
              "0-ab-1"
        , test "Removing transition from a link works" <|
          \_ ->
            check_update
              0 1 "a" "0-ab-1"
              "0-a-1"
        , test "Adding and removing from a link works" <|
          \_ ->
            check_update
              0 1 "ac" "0-ab-1"
              "0-ac-1"
        , test "Changing terminality of a link works" <|
          \_ ->
            check_update
              0 1 "!a" "0-a-1"
              "0-!a-1"
        , test "Partially changing terminality of a link works" <|
          \_ ->
            check_update
              0 1 "a!b" "0-ab-1"
              "0-a!b-1"
        ]
      , describe "Creating a new link"
        [ test "Can create a link between two existing nodes" <|
          \_ ->
            check_update
              1 3 "k" "0-a-1 1-b-2 2-c-3"
              "0-a-1 1-b-2 2-c-3 1-k-3"
        , test "Can create a link to source" <|
          \_ ->
            check_update
              2 0 "k" "0-a-1 1-b-2"
              "0-a-1 1-b-2 2-k-0"
        , test "Can create a link from source" <|
          \_ ->
            check_update
              0 2 "k" "0-a-1 1-b-2"
              "0-a-1 1-b-2 0-k-2"
        ]
      ]
    , describe "Creating" <|
      [ test "Can create a new node from the source" <|
        \_ ->
          check_creating
            "" 0 "a"
            "0-a-1"
      , test "Can create a new node that isn't from the source" <|
        \_ ->
          check_creating
            "0-a-1 1-b-2" 1 "b"
            "0-a-1 1-b-2"
      , describe "Cases where merging happens"
        [ test "Replicating a link causes a merge" <|
          \_ ->
            check_creating
              "0-a-1" 0 "a"
              "0-a-1"
        , test "Two same-terminality nodes going to the same end are merged [case Ⅰ]" <|
          \_ ->
            check_creating
              "0-a-1" 0 "b"
              "0-ab-1"
        , test "Two same-terminality nodes going to the same end are merged [case Ⅱ]" <|
          \_ ->
            check_creating
              "0-!a-1" 0 "!b"
              "0-!a!b-1"
        , test "Two different-terminality nodes going to the same end are merged [case Ⅰ]" <|
          \_ ->
            check_creating
              "0-a-1" 0 "!b"
              "0-a!b-1"
        , test "Two different-terminality nodes going to the same end are merged [case Ⅱ]" <|
          \_ ->
            check_creating
              "0-!a-1" 0 "b"
              "0-!ab-1"
        ]
      ]
    , describe "Multiple changes" <|
      [ test "Can create two links, one after another" <|
        \_ ->
          check_multi
            ""
            [ newnode_change 0 (mkConn "a") -- new node: 1
            , newnode_change 1 (mkConn "b") -- new node: 2
            ]
            "0-a-1 1-b-2"
      , test "Can create three links, the middle one is terminal" <|
        \_ ->
          check_multi
            ""
            [ newnode_change 0 (mkConn "a") -- new node: 1
            , newnode_change 1 (mkConn "!b") -- new node: 2
            , newnode_change 2 (mkConn "c") -- new node: 3
            ]
            "0-a-1 1-!b-2 2-c-3"
      , test "Joining of end-nodes does not happen prematurely [case Ⅰ]" <|
        \_ ->
          check_multi
            ""
            [ newnode_change 0 (mkConn "c") -- new node: 1
            , newnode_change 1 (mkConn "d") -- new node: 2
            , updateLink_graphchange 0 1 (mkConn "!ab")
            , newnode_change 1 (mkConn "!e") -- new node: 3
            , removeLink_graphchange 1 2
            ]
            "0-!ab-1 1-!e-2"
      , test "Joining of end-nodes does not happen prematurely [case Ⅱ]" <|
        \_ ->
          check_multi
            "0-a-1 1-b-2 2-c-3"
            [ newnode_change 2 (mkConn "e") -- new node: 4
            , updateLink_graphchange 4 1 (mkConn "l")
            ]
            "0-a-1 1-b-2 2-c-3 2-e-4 4-l-1"
      , test "Joining of end-nodes happens at the end [case Ⅰ]" <|
        \_ ->
          check_multi
            "0-a-1 1-b-2 2-c-3"
            [ newnode_change 2 (mkConn "e") -- new node: 4
            , updateLink_graphchange 4 1 (mkConn "l")
            , newnode_change 4 (mkConn "k") -- new node: 5
            ]
            "0-a-1 1-b-2 2-c-3 2-e-4 4-l-1 4-k-3"
      , describe "Disconnection / Reconnection scenarios"
        [ test "Disconnecting nodes and reconnecting them afterwards" <|
          \_ ->
            check_multi
              "0-a-1 1-b-2 2-c-3"
              [ removeLink_graphchange 1 2
              , updateLink_graphchange 1 2 (mkConn "d")
              ]
              "0-a-1 1-d-2 2-c-3"
        , test "Disconnect from one branch and reconnect to another" <|
          \_ ->
            check_multi
              "0-a-1 1-b-2 2-c-3 2-d-4 3-e-5 4-f-6"
              [ removeLink_graphchange 2 3
              , removeLink_graphchange 2 4
              , updateLink_graphchange 2 4 (mkConn "z")
              ]
              "0-a-1 1-b-2 2-z-4 4-f-6"
        , test "Disconnect from one branch and reconnect from another" <|
          \_ ->
            check_multi
              "0-a-1 1-b-2 2-c-3 2-d-4 3-e-5 4-f-6"
              [ removeLink_graphchange 2 3
              , removeLink_graphchange 2 4
              , updateLink_graphchange 1 4 (mkConn "z")
              ]
              "0-a-1 1-b-2 1-z-3 3-f-2"
        , test "Disconnect from branches and reconnect both differently" <|
          \_ ->
            check_multi
              "0-a-1 1-b-2 2-c-3 2-d-4 3-e-5 4-f-6 5-g-7 6-h-8"
              [ removeLink_graphchange 3 5
              , removeLink_graphchange 4 6
              , updateLink_graphchange 3 6 (mkConn "p")
              , updateLink_graphchange 4 5 (mkConn "q")
              ]
              "0-a-1 1-b-2 2-c-3 2-d-4 3-p-5 4-q-6 5-h-7 6-g-7"
        , test "Disconnect from branches and reconnect them afterwards [case Ⅰ]" <|
          \_ ->
            check_multi
              "0-a-1 1-b-2 2-c-3 2-d-4 3-e-5 4-f-6"
              [ removeLink_graphchange 2 3
              , removeLink_graphchange 2 4
              , updateLink_graphchange 2 3 (mkConn "r")
              , updateLink_graphchange 2 4 (mkConn "s")
              ]
              "0-a-1 1-b-2 2-r-3 2-s-4 3-e-5 4-f-5"
        , test "Disconnect from branches and reconnect them afterwards [case Ⅱ]" <|
          \_ ->
            check_multi
              "0-a-1 1-b-2 2-c-3 2-d-4 3-e-5 4-f-6"
              [ removeLink_graphchange 2 3
              , removeLink_graphchange 2 4
              , updateLink_graphchange 2 4 (mkConn "r")
              , updateLink_graphchange 2 3 (mkConn "s")
              ]
              "0-a-1 1-b-2 2-r-4 2-s-3 3-e-5 4-f-5"
        ]
      ]
    ]