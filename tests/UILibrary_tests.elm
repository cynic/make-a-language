module UILibrary_tests exposing (suite)
{-| Smoke tests for the UI harness itself (Batch 1): they verify that the
    harness's deterministic starting model is what the subsequent batches
    will assume it to be.
-}
import AutoDict
import Data
import Expect
import Main
import Test exposing (..)
import UILibrary as U


suite : Test
suite =
  describe "UILibrary harness"
    [ describe "defaultModel"
        [ test "creates the main editor view plus one explorer view per package" <|
            \_ ->
              -- init creates the main editor graph view, and
              -- refreshComputationsList adds one view per package
              AutoDict.size U.defaultModel.graph_views
              |> Expect.equal (1 + AutoDict.size U.defaultModel.packages)
        , test "lists one view in the computations explorer" <|
            \_ ->
              List.length U.defaultModel.computationsExplorer
              |> Expect.equal 1
        , test "marks that graph view as the main view" <|
            \_ ->
              U.mainView U.defaultModel
              |> Maybe.map .id
              |> Expect.equal (Just U.defaultModel.mainGraphView)
        , test "creates one package" <|
            \_ ->
              AutoDict.size U.defaultModel.packages
              |> Expect.equal 1
        , test "leaves the interaction dictionary empty" <|
            \_ ->
              AutoDict.size U.defaultModel.interactionsDict
              |> Expect.equal 0
        , test "is deterministic: a second init yields the same main view uuid" <|
            \_ ->
              (Main.init U.flags |> Tuple.first).mainGraphView
              |> Expect.equal U.defaultModel.mainGraphView
        ]
    , describe "idle-state capabilities"
        [ test "main view permits node/connection selection, splitting and dragging" <|
            \_ ->
              U.mainView U.defaultModel
              |> Maybe.map .can
              |> Expect.all
                  [ Maybe.map .selectNodes >> Expect.equal (Just True)
                  , Maybe.map .selectConnections >> Expect.equal (Just True)
                  , Maybe.map .splitNodes >> Expect.equal (Just True)
                  , Maybe.map .dragNodes >> Expect.equal (Just True)
                  ]
        , test "main view permits panning" <|
            \_ ->
              U.mainView U.defaultModel
              |> Maybe.map .can
              |> Maybe.map .pan
              |> Expect.equal (Just True)
        , test "splitter can be dragged in the idle state" <|
            \_ ->
              U.defaultModel.can.dragSplitter
              |> Expect.equal True
        , test "escape is not permitted in the idle state" <|
            \_ ->
              U.defaultModel.can.escape
              |> Expect.equal False
        ]
    , describe "withGraph"
        [ test "keeps the main view uuid" <|
            \_ ->
              let
                model =
                  U.withGraph (U.defaultModel |> U.mainView |> Maybe.map .computation |> Maybe.withDefault Data.empty) U.defaultModel
              in
                model.mainGraphView
                |> Expect.equal U.defaultModel.mainGraphView
        ]
    ]
