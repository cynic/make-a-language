module UndoRedo_tests exposing (suite)
{-| Batch 10: undo/redo and confirmation, at the update level.

    Undo/Redo swap the main view's computation between the undo and redo
    buffers (and refuse to operate while an interaction is in flight).
    `commit_change` pushes the pre-commit computation onto the undo buffer
    and clears the redo buffer; a Confirm with nothing on the interaction
    stack clears both buffers and optimises the graph (DFA round-trip),
    which removes emptied links, disconnected nodes, and merges equivalent
    states.
-}
import AutoDict
import AutoSet
import Data exposing
  ( GraphViewMsg(..)
  , Model
  , Msg(..)
  , NodeEffect(..)
  , entity
  )
import Dict
import Expect
import Graph
import Graph exposing (NodeId)
import Test exposing (..)
import UILibrary as U
import UILogic as UI
import Uuid exposing (Uuid)
import Utility


{-| A graph whose nodes 1 and 2 are equivalent end-of-computation sinks
    (`0-!a-1 0-!b-2`): optimisation merges them into one.
-}
mergeGraph : Data.AutomatonGraph
mergeGraph =
  let
    at id x y =
      (entity id NoEffect) |> (\e -> { e | x = x, y = y })
  in
    { graph =
        Graph.fromNodesAndEdges
          [ Graph.Node 0 (at 0 0 0)
          , Graph.Node 1 (at 1 300 0)
          , Graph.Node 2 (at 2 600 0)
          ]
          [ Graph.Edge 0 1 (Utility.mkConn "!a")
          , Graph.Edge 0 2 (Utility.mkConn "!b")
          ]
    , description = Nothing
    , root = 0
    }


baseModel : Model
baseModel =
  U.defaultModel
  |> U.withGraph (Utility.ag "0-a-1")


uid : Uuid
uid =
  baseModel.mainGraphView


openEditor : Data.Connection -> Msg
openEditor conn =
  EditConnection { x = 50, y = 50 } uid 0 1 conn


graphOf : Model -> Maybe Data.AutomatonGraph
graphOf m =
  U.mainView m |> Maybe.map .computation


expectGraph : String -> Model -> Expect.Expectation
expectGraph expected m =
  graphOf m
  |> Maybe.map (Utility.ag_equals (Utility.ag expected))
  |> Maybe.withDefault (Expect.fail "no main view")


suite : Test
suite =
  describe "Undo/Redo and confirmation"
    [ describe "undo and redo"
        [ test "undo restores the pre-edit graph; a new edit clears the redo buffer" <|
            \_ ->
              let
                result =
                  U.step
                    [ openEditor (Utility.mkConn "a")
                    , QuickInput "a" -- a becomes final
                    , Confirm
                    , Undo
                    , openEditor (Utility.mkConn "a")
                    , QuickInput "b" -- new change while undone
                    , Confirm
                    ]
                    baseModel
              in
                Expect.all
                  [ \_ -> expectGraph "0-ab-1" result
                  , \_ ->
                      U.mainView result
                      |> Maybe.map (.redoBuffer >> List.isEmpty)
                      |> Expect.equal (Just True) -- "we're not storing a tree"
                  , \_ ->
                      U.mainView result
                      |> Maybe.map (.undoBuffer >> List.length)
                      |> Expect.equal (Just 1) -- the redo consumed the old entry
                  ]
                    ()
        , test "can undo-redo all the way from end to start and back" <|
            \_ ->
              let
                chain =
                  [ openEditor (Utility.mkConn "a")
                  , QuickInput "a" -- a: non-final → final
                  , Confirm
                  , openEditor (Utility.mkConn "!a")
                  , QuickInput "b" -- add b
                  , Confirm
                  ]
                endState = U.step chain baseModel
                u1 = U.step1 Undo endState
                u2 = U.step [ Undo, Undo ] endState
                u3 = U.step [ Undo, Undo, Undo ] endState -- third undo: no-op
                r1 = U.step [ Undo, Undo, Undo, Redo ] endState
                r2 = U.step [ Undo, Undo, Undo, Redo, Redo ] endState
                r3 = U.step [ Undo, Undo, Undo, Redo, Redo, Redo ] endState -- third redo: no-op
              in
                Expect.all
                  [ \_ -> expectGraph "0-!a-1 0-b-1" endState
                  , \_ -> expectGraph "0-!a-1" u1
                  , \_ -> expectGraph "0-a-1" u2
                  , \_ -> expectGraph "0-a-1" u3 -- cannot undo past the buffer
                  , \_ -> expectGraph "0-!a-1" r1
                  , \_ -> expectGraph "0-!a-1 0-b-1" r2
                  , \_ -> expectGraph "0-!a-1 0-b-1" r3 -- cannot redo past the end
                  ]
                    ()
        , test "refits and redraws the graph on undo" <|
            \_ ->
              -- undoing a connection edit must also refresh the link
              -- drawing: after undo, the drawn connection is the restored
              -- (non-final) one, not the committed (final) one.
              let
                chain =
                  [ openEditor (Utility.mkConn "a")
                  , QuickInput "a" -- a becomes final
                  , Confirm
                  ]
                result = U.step (chain ++ [ Undo ]) baseModel
                redone = U.step (chain ++ [ Undo, Redo ]) baseModel
                drawnConnection m =
                  U.mainView m
                  |> Maybe.map
                      (.drawingData
                        >> .link_drawing
                        >> Dict.get ( 0, 1 )
                        >> Maybe.map
                            (.connection
                              >> AutoSet.toList
                              >> List.map Data.transitionToString
                              >> List.sort
                            )
                      )
              in
                Expect.all
                  [ \_ -> expectGraph "0-a-1" result
                  , \_ ->
                      -- after undo, the drawn connection is the restored
                      -- (non-final) one, not the committed (final) one
                      drawnConnection result
                      |> Expect.equal (Just (Just [ "a" ]))
                  , \_ -> expectGraph "0-!a-1" redone
                  , \_ ->
                      -- and after redo, the committed one again
                      drawnConnection redone
                      |> Expect.equal
                          (Just
                            (Just
                              (Utility.mkConn "!a"
                                |> AutoSet.toList
                                |> List.map Data.transitionToString
                                |> List.sort
                              )
                            )
                          )
                  ]
                    ()
        ]
    , describe "confirmation"
        [ test "confirm with an empty buffer is a no-op" <|
            \_ ->
              let
                result = U.step1 Confirm U.defaultModel
              in
                Expect.all
                  [ \_ -> result.mainGraphView |> Expect.equal U.defaultModel.mainGraphView
                  , \_ ->
                      AutoDict.size result.graph_views
                      |> Expect.equal (AutoDict.size U.defaultModel.graph_views)
                  ]
                    ()
        , test "optimises the graph: equivalent states are merged" <|
            \_ ->
              let
                model = U.withGraph mergeGraph U.defaultModel
                result =
                  U.step
                    [ GraphViewMsg model.mainGraphView (SelectNode 0 { x = 95, y = 95 })
                    , GraphViewMsg model.mainGraphView (MoveNode { x = 50, y = 400 })
                    , GraphViewMsg model.mainGraphView SelectSpace
                    , Confirm -- commits the phantom node (empty connection)
                    , Confirm -- optimises
                    ]
                    model
              in
                -- nodes 1 and 2 were equivalent sinks; after optimisation
                -- there is a single sink.  (Ids are renumbered, so ag_equals.)
                expectGraph "0-!a-1 0-!b-1" result
        , test "centers the optimised result properly" <|
            \_ ->
              let
                model = U.withGraph mergeGraph U.defaultModel
                result =
                  U.step
                    [ GraphViewMsg model.mainGraphView (SelectNode 0 { x = 95, y = 95 })
                    , GraphViewMsg model.mainGraphView (MoveNode { x = 50, y = 400 })
                    , GraphViewMsg model.mainGraphView SelectSpace
                    , Confirm
                    , Confirm
                    ]
                    model
                gv = U.mainViewOrFail result
              in
                gv.guest
                |> Expect.equal
                    (UI.calculateGuestDimensionsForHost gv.host False gv.computation.graph)
        , test "removes the undo and redo buffers" <|
            \_ ->
              let
                model = U.withGraph mergeGraph U.defaultModel
                result =
                  U.step
                    [ GraphViewMsg model.mainGraphView (SelectNode 0 { x = 95, y = 95 })
                    , GraphViewMsg model.mainGraphView (MoveNode { x = 50, y = 400 })
                    , GraphViewMsg model.mainGraphView SelectSpace
                    , Confirm
                    , Confirm
                    ]
                    model
              in
                U.mainView result
                |> Maybe.map (\v -> ( List.isEmpty v.undoBuffer, List.isEmpty v.redoBuffer ))
                |> Expect.equal ( Just ( True, True ) )
        ]
    ]
