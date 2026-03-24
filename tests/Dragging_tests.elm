module Dragging_tests exposing (suite)
{-| Batch 3: dragging a node, at the update level.

    The real event wiring lives in `Main.subscriptions` (client coordinates
    are translated to SVG coordinates there, using the host coordinates
    stored on the interaction).  These tests simulate that wiring by calling
    `UI.translateHostCoordinates` exactly as the subscription does, then
    feeding the resulting `MoveNode` message to `update`.
-}
import Data exposing
  ( Coordinate
  , GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  )
import Dict
import Expect
import Graph exposing (NodeId)
import Queries as Q
import Test exposing (..)
import UILibrary as U
import UILogic as UI


{-| A model whose main view contains a single node at SVG (0,0), with a
    190×190 host and a matching 190×190 guest at (−95,−95) — so one host
    pixel equals one SVG unit, and the maths below stays exact.
-}
dragModel : Model
dragModel =
  U.defaultModel
  |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ) ])
  |> U.mapMainView
      (\gv ->
        { gv
          | host = { w = 190, h = 190 }
          , guest = { x = -95, y = -95, w = 190, h = 190 }
          , pan = ( 0, 0 )
        }
      )


startDrag : Coordinate -> Msg
startDrag hostClick =
  GraphViewMsg dragModel.mainGraphView (StartDraggingNode 0 hostClick)


nodeCoord : Model -> NodeId -> Maybe Coordinate
nodeCoord model id =
  U.mainView model
  |> Maybe.andThen (\gv -> Graph.get id gv.computation.graph)
  |> Maybe.map (\ctx -> { x = ctx.node.label.x, y = ctx.node.label.y })


nodeDrawingCoord : Model -> NodeId -> Maybe Coordinate
nodeDrawingCoord model id =
  U.mainView model
  |> Maybe.map .drawingData
  |> Maybe.andThen (\dd -> Dict.get id dd.node_drawing)
  |> Maybe.map .coordinates


suite : Test
suite =
  describe "Dragging a node"
    [ describe "StartDraggingNode"
        [ test "pushes a DraggingNode interaction carrying the SVG element's host coordinates" <|
            \_ ->
              U.step1 (startDrag { x = 50, y = 25 }) dragModel
              |> \m -> Q.peekInteraction (Just m.mainGraphView) m.interactionsDict
              |> Expect.equal (Just (DraggingNode 0 { x = -45, y = -70 }))
        , test "for an unknown node, starts nothing" <|
            \_ ->
              U.step1 (GraphViewMsg dragModel.mainGraphView (StartDraggingNode 99 { x = 50, y = 25 })) dragModel
              |> .interactionsDict
              |> Q.peekInteraction (Just dragModel.mainGraphView)
              |> Expect.equal Nothing
        , test "capabilities become drag-only: main view may pan but not select" <|
            \_ ->
              U.step1 (startDrag { x = 50, y = 25 }) dragModel
              |> U.mainViewOrFail
              |> .can
              |> \can -> ( can.pan, can.selectNodes, can.dragNodes )
              |> Expect.equal ( True, False, False )
        , test "capabilities become drag-only: only escape remains globally" <|
            \_ ->
              U.step1 (startDrag { x = 50, y = 25 }) dragModel
              |> .can
              |> \can -> ( can.escape, can.dragSplitter, can.createNewPackage )
              |> Expect.equal ( True, False, False )
        ]
    , describe "MoveNode"
        [ test "moves the node in the graph and in the drawing data" <|
            \_ ->
              U.step [ startDrag { x = 50, y = 25 }, GraphViewMsg dragModel.mainGraphView (MoveNode { x = 123, y = 45 }) ] dragModel
              |> \m -> ( nodeCoord m 0, nodeDrawingCoord m 0 )
              |> Expect.equal
                  ( Just { x = 123, y = 45 }
                  , Just { x = 123, y = 45 }
                  )
        , test "without a drag in progress, does nothing" <|
            \_ ->
              U.step1 (GraphViewMsg dragModel.mainGraphView (MoveNode { x = 10, y = 20 })) dragModel
              |> \m -> nodeCoord m 0
              |> Expect.equal (Just { x = 0, y = 0 })
        , test "follows the mouse exactly when the graph is panned" <|
            \_ ->
              -- The graph is panned by (7,3) and the node sits at SVG (0,0).
              -- With guest (−95,−95,190,190) the node's client position is
              -- (88,92), and the SVG element's top-left is at client (0,0).
              -- The user shift-clicks the node at (88,92) and drags to
              -- (98,102): a (+10,+10) client movement must land the node at
              -- SVG (10,10) — i.e. exactly under the cursor.
              let
                panned =
                  U.defaultModel
                  |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ) ])
                  |> U.mapMainView
                      (\gv ->
                        { gv
                          | host = { w = 190, h = 190 }
                          , guest = { x = -95, y = -95, w = 190, h = 190 }
                          , pan = ( 7, 3 )
                        }
                      )
                started =
                  U.step1 (GraphViewMsg panned.mainGraphView (StartDraggingNode 0 { x = 88, y = 92 })) panned
                storedHostCoord =
                  Q.peekInteraction (Just panned.mainGraphView) started.interactionsDict
                  |> Maybe.map
                      (\interaction ->
                        case interaction of
                          DraggingNode _ hc ->
                            hc
                          _ ->
                            { x = -1, y = -1 }
                      )
                -- simulate Main.subscriptions: translate the new client position
                -- using the stored host coordinates and the view
                movedTo client =
                  Maybe.map
                    (\hc -> UI.translateHostCoordinates hc client (U.mainViewOrFail started))
                    storedHostCoord
              in
                Expect.all
                  [ \_ -> storedHostCoord |> Expect.equal (Just { x = 0, y = 0 })
                  , \_ -> movedTo { x = 98, y = 102 } |> Expect.equal (Just { x = 10, y = 10 })
                  , \_ ->
                      U.step1 (GraphViewMsg panned.mainGraphView (MoveNode { x = 10, y = 10 })) started
                      |> \m -> nodeCoord m 0
                      |> Expect.equal (Just { x = 10, y = 10 })
                  ]
                    ()
        ]
    , describe "StopDraggingNode"
        [ test "pops the interaction" <|
            \_ ->
              U.step [ startDrag { x = 50, y = 25 }, GraphViewMsg dragModel.mainGraphView StopDraggingNode ] dragModel
              |> .interactionsDict
              |> Q.peekInteraction (Just dragModel.mainGraphView)
              |> Expect.equal Nothing
        , test "restores the idle capabilities" <|
            \_ ->
              U.step [ startDrag { x = 50, y = 25 }, GraphViewMsg dragModel.mainGraphView StopDraggingNode ] dragModel
              |> U.mainViewOrFail
              |> .can
              |> \can -> ( can.pan, can.selectNodes, can.dragNodes )
              |> Expect.equal ( True, True, True )
        , test "the node stays where it was dropped" <|
            \_ ->
              U.step
                [ startDrag { x = 50, y = 25 }
                , GraphViewMsg dragModel.mainGraphView (MoveNode { x = 123, y = 45 })
                , GraphViewMsg dragModel.mainGraphView StopDraggingNode
                ]
                dragModel
              |> \m -> nodeCoord m 0
              |> Expect.equal (Just { x = 123, y = 45 })
        ]
    , describe "drag-and-pan"
        [ test "panning during a drag pans the view and keeps the drag alive" <|
            \_ ->
              let
                dragged =
                  U.step
                    [ startDrag { x = 50, y = 25 }
                    , GraphViewMsg dragModel.mainGraphView (Pan 1 0)
                    ]
                    dragModel
              in
                Expect.all
                  [ \_ ->
                      U.mainViewOrFail dragged
                      |> .pan
                      |> Expect.equal ( 1, 0 )
                  , \_ ->
                      Q.peekInteraction (Just dragged.mainGraphView) dragged.interactionsDict
                      |> Maybe.map
                          (\interaction ->
                            case interaction of
                              DraggingNode id _ ->
                                id
                              _ ->
                                -1
                          )
                      |> Expect.equal (Just 0)
                  , \_ ->
                      -- and the drag can continue after the pan
                      U.step1 (GraphViewMsg dragged.mainGraphView (MoveNode { x = 10, y = 20 })) dragged
                      |> \m -> nodeCoord m 0
                      |> Expect.equal (Just { x = 10, y = 20 })
                  ]
                    ()
        , test "the view permits panning mid-drag (that is what makes drag-and-pan possible)" <|
            \_ ->
              U.step1 (startDrag { x = 50, y = 25 }) dragModel
              |> U.mainViewOrFail
              |> .can
              |> .pan
              |> Expect.equal True
        ]
    ]
