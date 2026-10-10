module SplittingRoundTrip_tests exposing (suite)
{-| The node-split round-trip, using the digit-graph reported by the user:
    splitting node 2's incoming into 1234 (left → new node) and 5678
    (right → original node), with the full self-loop duplicated to both
    halves.

    Pins:
    - the single-Confirm shape — the original keeps the *right* side, the
      new node takes the *left*, both get the full self-loop, and the new
      node's outgoing must NOT contain an un-mirrored edge back to the
      original;
    - split → Confirm → Confirm restores the original graph when the
      halves are behaviourally identical (end-to-end through the
      optimiser; the engine-level properties of the merge rule live in
      `AutomatonGraph_tests.identicalOutgoing_suite`).
-}
import AutoSet
import Data exposing
  ( GraphViewMsg(..)
  , Model
  , Msg(..)
  , transitionToString
  )
import Expect
import Graph
import Graph exposing (NodeId)
import IntDict
import GoldenGeometry
import Test exposing (..)
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


{-| The user's graph: 0 -!0→ 1, 0 -!1..!9→ 2, 2 -!0..!9→ 2 (self-loop).
-}
digitGraph : Data.AutomatonGraph
digitGraph =
  GoldenGeometry.mkGraph
    [ ( 0, 7.0710678118654755, 0 )
    , ( 1, 72.74551657969945, -1.108949416342405 )
    , ( 2, 39.56924677327628, -44.436285518663745 )
    ]
    [ ( 0, 1, "!0" )
    , ( 0, 2, "!1!2!3!4!5!6!7!8!9" )
    , ( 2, 2, "!0!1!2!3!4!5!6!7!8!9" )
    ]


digitModel : Model
digitModel =
  U.defaultModel
  |> U.withGraph digitGraph


uid : Uuid
uid =
  digitModel.mainGraphView


{-| Open the split on node 2 and move 1,2,3,4 to the left side.
-}
splitInto1234And5678 : List Msg
splitInto1234And5678 =
  [ GraphViewMsg uid (StartSplit 2)
  , QuickInput "1"
  , QuickInput "2"
  , QuickInput "3"
  , QuickInput "4"
  ]


connString : Data.Connection -> List String
connString conn =
  AutoSet.toList conn
  |> List.map transitionToString
  |> List.sort


{-| The committed connection `src → dest` in the main view's graph.
-}
edgeConn : Model -> NodeId -> NodeId -> List String
edgeConn m src dest =
  U.mainView m
  |> Maybe.andThen (.computation >> .graph >> Graph.get dest)
  |> Maybe.map
      (\ctx ->
        ctx.incoming
        |> IntDict.get src
        |> Maybe.withDefault (AutoSet.empty transitionToString)
        |> connString
      )
  |> Maybe.withDefault [ "NO EDGE" ]


nodeCount : Model -> Int
nodeCount m =
  U.mainView m
  |> Maybe.map (.computation >> .graph >> Graph.nodes >> List.length)
  |> Maybe.withDefault -1


edgeCount : Model -> Int
edgeCount m =
  U.mainView m
  |> Maybe.map (.computation >> .graph >> Graph.edges >> List.length)
  |> Maybe.withDefault -1


suite : Test
suite =
  describe "Splitting a node: the round-trip"
    [ describe "one Confirm divides the link and keeps both halves whole"
        [ test "1234 goes to the new node, 5678 stays with the original; self-loops are full; no half-edges" <|
            \_ ->
              let
                result = U.step (splitInto1234And5678 ++ [ Confirm ]) digitModel
              in
                Expect.all
                  [ \_ -> edgeConn result 0 1 |> Expect.equal (connString (Utility.mkConn "!0"))
                  , \_ ->
                      -- the ORIGINAL node keeps the RIGHT side…
                      edgeConn result 0 2
                      |> Expect.equal (connString (Utility.mkConn "!5!6!7!8!9"))
                  , \_ ->
                      -- …and the NEW node takes the LEFT side
                      edgeConn result 0 3
                      |> Expect.equal (connString (Utility.mkConn "!1!2!3!4"))
                  , \_ ->
                      -- both halves carry the full self-loop
                      edgeConn result 2 2
                      |> Expect.equal (connString (Utility.mkConn "!0!1!2!3!4!5!6!7!8!9"))
                  , \_ ->
                      edgeConn result 3 3
                      |> Expect.equal (connString (Utility.mkConn "!0!1!2!3!4!5!6!7!8!9"))
                  , \_ ->
                      -- bug #12: the new node must NOT have an un-mirrored
                      -- edge back to the original node
                      edgeConn result 3 2 |> Expect.equal []
                  , \_ ->
                      -- …and the latent form of the same defect: the new
                      -- node's OUTGOING must not point back at the original
                      -- (invisible to Graph.edges, but execution tracing
                      -- reads outgoing directly)
                      U.mainView result
                      |> Maybe.andThen (.computation >> .graph >> Graph.get 3)
                      |> Maybe.map (\ctx -> IntDict.keys ctx.outgoing |> List.sort)
                      |> Expect.equal (Just [ 3 ])
                  , \_ -> edgeCount result |> Expect.equal 5
                  ]
                    ()
        ]
    , describe "split → Confirm → Confirm restores the original graph"
        [ test "the optimiser merges the equivalent halves back into one" <|
            \_ ->
              -- the two halves are Myhill–Nerode equivalent — same
              -- self-loop set, no other behaviour — so the optimiser
              -- must merge them and restore the original graph.
              let
                result = U.step (splitInto1234And5678 ++ [ Confirm, Confirm ]) digitModel
              in
                Expect.all
                  [ \_ ->
                      U.mainView result
                      |> Maybe.map .computation
                      |> Maybe.map (Utility.ag_equals digitGraph)
                      |> Maybe.withDefault (Expect.fail "no main view")
                  , \_ -> nodeCount result |> Expect.equal 3
                  , \_ ->
                      -- the surviving node keeps the original node-2 position
                      U.mainView result
                      |> Maybe.map (.computation >> .graph >> Graph.nodes)
                      |> Maybe.withDefault []
                      |> List.any
                          (\n ->
                            abs (n.label.x - 39.56924677327628) < 1e-9
                              && abs (n.label.y - -44.436285518663745) < 1e-9
                          )
                      |> Expect.equal True
                  ]
                    ()
        ]
    ]
