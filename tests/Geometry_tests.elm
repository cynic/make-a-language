module Geometry_tests exposing (suite)
{-| Batch 2: pure geometry functions.

    - `UILogic.translateHostCoordinates` — host (client) coords → guest
      (SVG-viewport) coords, including edge clamping and panning.
    - `UILogic.calculate_host_coord` — the reverse direction, through the
      model: host coords of a clicked node + its SVG coords → host coords of
      the SVG element's top-left corner.
    - `UILogic.calculateGuestDimensionsForHost` / `fitGraphViewToGraph` —
      centering the viewport on the graph.
    - `GraphEditor.panGraphView` — panning, its limits, and the "heading
      toward another node" heuristic that escapes the local-minimum trap.
-}
import Data exposing (GraphView, Model)
import Expect
import Graph exposing (NodeId)
import GraphEditor
import Test exposing (..)
import UILibrary as U
import UILogic as UI
import Utility


{-| A main graph view with known geometry: nodes as given, host 100×100,
    guest 190×190 with its top-left at (−95,−95), pan (0,0).  A single node
    at (0,0) therefore sits exactly at the centre of the guest viewport.
-}
fixedView : List ( NodeId, Float, Float ) -> GraphView
fixedView nodes =
  U.defaultModel
  |> U.withGraph (U.graphWithNodes nodes)
  |> U.mainViewOrFail
  |> \gv ->
      { gv
        | host = { w = 100, h = 100 }
        , guest = { x = -95, y = -95, w = 190, h = 190 }
        , pan = ( 0, 0 )
      }


{-| A view with a 2:1 guest:host ratio, used for translation maths.
-}
ratioView : GraphView
ratioView =
  fixedView [ ( 0, 0, 0 ) ]
  |> \gv ->
      { gv
        | host = { w = 100, h = 50 }
        , guest = { x = -10, y = -20, w = 200, h = 100 }
      }


{-| Apply the same pan step `n` times.
-}
panTimes : Int -> Int -> Int -> GraphView -> GraphView
panTimes n dx dy gv =
  List.foldl (\_ acc -> GraphEditor.panGraphView dx dy acc) gv (List.range 1 n)


{-| A model whose main view contains a single node at SVG (5,5), with known
    host/guest geometry, for `calculate_host_coord` tests.
-}
hostCoordModel : Model
hostCoordModel =
  U.defaultModel
  |> U.withGraph (U.graphWithNodes [ ( 0, 5, 5 ) ])
  |> U.mapMainView
      (\gv ->
        { gv
          | host = { w = 100, h = 50 }
          , guest = { x = -10, y = -20, w = 200, h = 100 }
          , pan = ( 0, 0 )
        }
      )


suite : Test
suite =
  describe "Pure geometry functions"
    [ describe "translateHostCoordinates"
        [ test "translates an interior point (2:1 ratio, no pan)" <|
            \_ ->
              UI.translateHostCoordinates { x = 0, y = 0 } { x = 50, y = 25 } ratioView
              |> Expect.equal { x = 90, y = 30 }
        , test "clamps to the left/top edge" <|
            \_ ->
              UI.translateHostCoordinates { x = 0, y = 0 } { x = -5, y = -3 } ratioView
              |> Expect.equal { x = -10, y = -20 }
        , test "clamps to the right/bottom edge" <|
            \_ ->
              UI.translateHostCoordinates { x = 0, y = 0 } { x = 150, y = 80 } ratioView
              |> Expect.equal { x = 190, y = 80 }
        , test "adds the pan offset" <|
            \_ ->
              UI.translateHostCoordinates { x = 0, y = 0 } { x = 50, y = 25 }
                { ratioView | pan = ( 7, 3 ) }
              |> Expect.equal { x = 97, y = 33 }
        , test "measures relative to a panned host origin" <|
            \_ ->
              UI.translateHostCoordinates { x = 200, y = 100 } { x = 250, y = 125 } ratioView
              |> Expect.equal { x = 90, y = 30 }
        ]
    , describe "calculate_host_coord"
        [ test "recovers the SVG element's host-space top-left from a clicked node" <|
            \_ ->
              UI.calculate_host_coord hostCoordModel.mainGraphView hostCoordModel { x = 50, y = 25 } 0
              |> Expect.equal (Just { x = 42.5, y = 12.5 })
        , test "returns Nothing for an unknown node" <|
            \_ ->
              UI.calculate_host_coord hostCoordModel.mainGraphView hostCoordModel { x = 50, y = 25 } 99
              |> Expect.equal Nothing
        , test "returns Nothing for an unknown view" <|
            \_ ->
              UI.calculate_host_coord (Utility.uuid_from_int 99) hostCoordModel { x = 50, y = 25 } 0
              |> Expect.equal Nothing
        ]
    , describe "calculateGuestDimensionsForHost / fitGraphViewToGraph"
        [ test "a lone node at the origin is centred (fitClosely)" <|
            \_ ->
              UI.calculateGuestDimensionsForHost { w = 100, h = 100 } True
                (U.graphWithNodes [ ( 0, 0, 0 ) ]).graph
              |> \guest ->
                  Expect.all
                    [ \g -> ( g.x + g.w / 2, g.y + g.h / 2 ) |> Expect.equal ( 0, 0 )
                    , \g -> ( g.w, g.h ) |> Expect.equal ( 120, 120 )
                    ]
                    guest
        , test "a node dragged elsewhere is centred at its new position" <|
            \_ ->
              UI.calculateGuestDimensionsForHost { w = 100, h = 100 } True
                (U.graphWithNodes [ ( 0, 500, 300 ) ]).graph
              |> \guest ->
                  ( guest.x + guest.w / 2, guest.y + guest.h / 2 )
                  |> Expect.equal ( 500, 300 )
        , test "without fitClosely, extra padding surrounds the graph" <|
            \_ ->
              UI.calculateGuestDimensionsForHost { w = 100, h = 100 } False
                (U.graphWithNodes [ ( 0, 0, 0 ) ]).graph
              |> \guest ->
                  Expect.all
                    [ \g -> ( g.x + g.w / 2, g.y + g.h / 2 ) |> Expect.equal ( 0, 0 )
                    , \g -> ( g.w, g.h ) |> Expect.equal ( 190, 190 )
                    ]
                    guest
        , test "fitGraphViewToGraph restores a centred guest, whatever the guest was" <|
            \_ ->
              fixedView [ ( 0, 0, 0 ) ]
              |> \gv -> { gv | guest = { x = 123, y = 456, w = 10, h = 10 } }
              |> UI.fitGraphViewToGraph
              |> .guest
              |> Expect.equal
                  { x = -95, y = -95, w = 190, h = 190 }
        ]
    , describe "panGraphView"
        [ test "pans by one step when a node is in view" <|
            \_ ->
              GraphEditor.panGraphView 1 0 (fixedView [ ( 0, 0, 0 ) ])
              |> \gv -> ( gv.pan, gv.activePanDirection )
              |> Expect.equal ( ( 1, 0 ), Just Data.ToRight )
        , test "cannot pan beyond the graph: stops when the last node leaves view" <|
            \_ ->
              panTimes 200 1 0 (fixedView [ ( 0, 0, 0 ) ])
              |> .pan
              |> Expect.equal ( 94, 0 )
        , test "scales the step size with the viewport" <|
            \_ ->
              fixedView [ ( 0, 0, 0 ) ]
              |> \gv -> { gv | guest = { x = -500, y = -500, w = 1000, h = 1000 } }
              |> GraphEditor.panGraphView 1 0
              |> .pan
              |> Expect.equal ( 4, 0 )
        , test "panning is refused when panning is not permitted" <|
            \_ ->
              let
                noPan =
                  fixedView [ ( 0, 0, 0 ) ]
                  |> \v -> { v | can = v.can |> \c -> { c | pan = False } }
              in
                GraphEditor.panGraphView 1 0 noPan
                |> \r -> ( r.pan, r.activePanDirection )
                |> Expect.equal ( ( 0, 0 ), Nothing )
        , test "when no node is in view, panning toward a distant node is still allowed" <|
            \_ ->
              -- nodes at 0 and 1000; the viewport (190 wide) shows neither at
              -- pan (285,0), but panning right heads toward the node at 1000.
              fixedView [ ( 0, 0, 0 ), ( 1, 1000, 0 ) ]
              |> \gv -> { gv | pan = ( 285, 0 ) }
              |> GraphEditor.panGraphView 1 0
              |> .pan
              |> Expect.equal ( 286, 0 )
        , test "when no node is in view, panning away from every node is refused" <|
            \_ ->
              -- nodes at 0 and 500; at pan (600,0) both nodes are behind us
              let
                pastEverything =
                  fixedView [ ( 0, 0, 0 ), ( 1, 500, 0 ) ]
                  |> \v -> { v | pan = ( 600, 0 ) }
              in
                GraphEditor.panGraphView 1 0 pastEverything
                |> \r -> ( r.pan, r.activePanDirection )
                |> Expect.equal ( ( 600, 0 ), Nothing )
        , test "records the pan direction (diagonal)" <|
            \_ ->
              GraphEditor.panGraphView -1 -1 (fixedView [ ( 0, 0, 0 ) ])
              |> .activePanDirection
              |> Expect.equal (Just Data.ToTopLeft)
        , test "records the pan direction (down)" <|
            \_ ->
              GraphEditor.panGraphView 0 1 (fixedView [ ( 0, 0, 0 ) ])
              |> .activePanDirection
              |> Expect.equal (Just Data.ToBottom)
        ]
    ]
