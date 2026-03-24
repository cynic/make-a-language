module Tier1Geometry_tests exposing (suite)
{-| Batch 11: Tier-1 geometry assertions — model-level, refactor-stable.

    - Cardinality of one-way, two-way and self-loop links.
    - The connection points of straight/curved links: the source sits
      exactly on the node circle (radius 7); the target sits 18 units out
      — deliberately *not* on the boundary, to leave room for the
      arrowhead (radius_to*2 + 8).
    - The recursive loop attaches symmetrically on the node circle.
    - Node drawing flags (the model-level proxy for the CSS classes
      `start`, `terminal`, `disconnected`, `splittable`).
-}
import AutoDict
import Data exposing
  ( AutomatonGraph
  , Cardinality(..)
  , NodeEffect(..)
  , entity
  )
import Dict
import Expect
import Graph
import Graph exposing (NodeId)
import GraphEditor
import Test exposing (..)
import Uuid exposing (Uuid)
import Utility


{-| The drawing data computed for a graph, with no packages involved.
-}
drawnLinks : AutomatonGraph -> Dict.Dict ( NodeId, NodeId ) Data.LinkDrawingData
drawnLinks ag =
  GraphEditor.linkDrawingForPackage ag (AutoDict.empty Uuid.toString)


{-| A graph with the given nodes (id, x, y) and edges
    (src, dest, connection-string).
-}
edgeGraph : List ( NodeId, Float, Float ) -> List ( NodeId, NodeId, String ) -> AutomatonGraph
edgeGraph nodes edges =
  let
    at id x y =
      (entity id NoEffect) |> (\e -> { e | x = x, y = y })
  in
    { graph =
        Graph.fromNodesAndEdges
          (List.map (\( id, x, y ) -> Graph.Node id (at id x y)) nodes)
          (List.map (\( a, b, c ) -> Graph.Edge a b (Utility.mkConn c)) edges)
    , description = Nothing
    , root = 0
    }


dist : ( Float, Float ) -> ( Float, Float ) -> Float
dist ( x1, y1 ) ( x2, y2 ) =
  sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)


cardinalityOf : AutomatonGraph -> ( NodeId, NodeId ) -> Maybe Cardinality
cardinalityOf ag key =
  drawnLinks ag
  |> Dict.get key
  |> Maybe.map .cardinality


suite : Test
suite =
  describe "Tier-1 geometry"
    [ describe "cardinality"
        [ test "a one-way link is Unidirectional" <|
            \_ ->
              cardinalityOf (edgeGraph [ ( 0, 0, 0 ), ( 1, 300, 0 ) ] [ ( 0, 1, "a" ) ]) ( 0, 1 )
              |> Expect.equal (Just Unidirectional)
        , test "a two-way link is Bidirectional (both directions)" <|
            \_ ->
              cardinalityOf
                (edgeGraph [ ( 0, 0, 0 ), ( 1, 300, 0 ) ] [ ( 0, 1, "a" ), ( 1, 0, "b" ) ])
              |> \f -> ( f ( 0, 1 ), f ( 1, 0 ) )
              |> Expect.equal ( Just Bidirectional, Just Bidirectional )
        , test "a self-loop is Recursive" <|
            \_ ->
              cardinalityOf
                (edgeGraph [ ( 0, 100, 100 ) ] [ ( 0, 0, "a" ) ])
                ( 0, 0 )
              |> Expect.equal (Just Recursive)
        ]
    , describe "connection points lie on the node boundaries"
        [ test "straight (unidirectional) link" <|
            \_ ->
              let
                ag = edgeGraph [ ( 0, 0, 0 ), ( 1, 300, 0 ) ] [ ( 0, 1, "a" ) ]
                pb =
                  drawnLinks ag
                  |> Dict.get ( 0, 1 )
                  |> Maybe.map .pathBetween
              in
                case pb of
                  Just p ->
                    Expect.all
                      [ \_ ->
                          -- source: exactly on the circle of radius 7
                          Expect.within (Expect.Absolute 1e-9)
                            (dist ( p.source_connection_point.x, p.source_connection_point.y ) ( 0, 0 ))
                            7
                      , \_ ->
                          -- target: 18 units out (radius_to*2 + 8) to make
                          -- room for the arrowhead — by design, not on the
                          -- boundary itself
                          Expect.within (Expect.Absolute 1e-9)
                            (dist ( p.target_connection_point.x, p.target_connection_point.y ) ( 300, 0 ))
                            18
                      ]
                        ()

                  _ ->
                    Expect.fail "no link drawing for (0,1)"
        , test "curved (bidirectional) link keeps the same distances" <|
            \_ ->
              let
                ag =
                  edgeGraph [ ( 0, 0, 0 ), ( 1, 300, 0 ) ] [ ( 0, 1, "a" ), ( 1, 0, "b" ) ]
                pb =
                  drawnLinks ag
                  |> Dict.get ( 0, 1 )
                  |> Maybe.map .pathBetween
              in
                case pb of
                  Just p ->
                    -- the curve bows away, but the ends stay on their circles
                    Expect.all
                      [ \_ ->
                          Expect.within (Expect.Absolute 1e-9)
                            (dist ( p.source_connection_point.x, p.source_connection_point.y ) ( 0, 0 ))
                            7
                      , \_ ->
                          Expect.within (Expect.Absolute 1e-9)
                            (dist ( p.target_connection_point.x, p.target_connection_point.y ) ( 300, 0 ))
                            18
                      , \_ ->
                          -- and it really is curved: the control point is off the line
                          (p.control_point.y /= 0)
                            |> Expect.equal True
                      ]
                        ()

                  _ ->
                    Expect.fail "no link drawing for (0,1)"
        , test "recursive loop attaches symmetrically on the node circle" <|
            \_ ->
              let
                ag = edgeGraph [ ( 0, 100, 100 ) ] [ ( 0, 0, "a" ) ]
                pb =
                  drawnLinks ag
                  |> Dict.get ( 0, 0 )
                  |> Maybe.map .pathBetween
              in
                case pb of
                  Just p ->
                    -- the loop path starts at source_connection_point and
                    -- ends 14 to its right (pathString "… 14,0"); the
                    -- midpoint of the loop's base is the node's circle
                    -- point, exactly 7 units from the centre
                    Expect.within (Expect.Absolute 1e-9)
                      (dist
                        ( p.source_connection_point.x + 7, p.source_connection_point.y )
                        ( 100, 100 )
                      )
                      7

                  _ ->
                    Expect.fail "no link drawing for (0,0)"
        ]
    , describe "node flags (the model behind the CSS classes)"
        [ test "reflect root, terminal, splittable and disconnected state" <|
            \_ ->
              let
                -- isTerminal = has a *final* incoming transition
                -- canSplit  = >1 non-recursive sources, or >1 transitions in
                ag =
                  edgeGraph
                    [ ( 0, 0, 0 ), ( 1, 300, 0 ), ( 2, 600, 0 ), ( 3, 900, 0 ) ]
                    [ ( 0, 1, "!ac" ) -- final a + plain c: node 1 is terminal and splittable
                    , ( 0, 2, "b" )
                      -- node 3 has no edges at all: disconnected
                    ]
                flags id =
                  GraphEditor.nodeDrawingForPackage ag (Utility.uuid_from_int 1)
                  |> Dict.get id
                  |> Maybe.map (\d -> ( ( d.isRoot, d.isTerminal ), ( d.canSplit, d.isDisconnected ) ))
              in
                Expect.all
                  [ \_ -> flags 0 |> Expect.equal ( Just ( ( True, False ), ( False, False ) ) )
                  , \_ -> flags 1 |> Expect.equal ( Just ( ( False, True ), ( True, False ) ) )
                  , \_ -> flags 2 |> Expect.equal ( Just ( ( False, False ), ( False, False ) ) )
                  , \_ -> flags 3 |> Expect.equal ( Just ( ( False, False ), ( False, True ) ) )
                  ]
                    ()
        ]
    ]
