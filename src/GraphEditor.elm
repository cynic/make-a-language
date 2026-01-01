module GraphEditor exposing
  ( applyChangesToGraph
  , cancelNewNodeCreation
  , computeGraphFully
  , coordinateForces
  , createNewGraphNode
  , dragNode
  , linkDrawingForPackage
  , movePhantomNode
  , newnode_graphchange
  , nodeDrawingForPackage
  , panGraphView
  , recalculateLinksAndNodes
  , removeLink_graphchange
  , removePhantomLink
  , selectSourceNode
  , spreadOutForces
  , updateLink_graphchange
  , viewGraph
  , viewGraphReference
  )
import AutoDict
import Data exposing (..)
import Debugging exposing (..)
import Automata.DFA as DFA
import AutoSet
import Changes as C
import Css
import Dict exposing (Dict)
import Force
import Graph exposing (Graph, NodeContext, NodeId)
import Html.Styled exposing (div, Html)
import Html.Styled.Attributes as HA
import IntDict
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Math.Vector2 exposing (distance)
import Maybe.Extra as Maybe
import Queries as Q
import Random.Pcg.Extended as Random
import Set
import TypedSvg exposing (circle, defs, g, marker, path, rect, svg, text_, title, tspan)
import TypedSvg.Attributes exposing (class, d, dy, fill, id, orient, refX, refY, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, markerHeight, markerWidth, rx, ry, width, x, y)
import TypedSvg.Core exposing (text, Svg)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (..)
import Uuid exposing (Uuid)
import VirtualDom

  -- to add: Execute, Step, Stop
  -- also: when I make a change to the graph, set .execution to Nothing!

-- For zooming, I take the approach set out at https://www.petercollingridge.co.uk/tutorials/svg/interactive/pan-and-zoom/


{-| The buffer from the edges within which panning occurs -}
-- panBufferAmount : (Float, Float) -> Float
-- panBufferAmount (w, h) =
--   let
--     minBuffer = 25 -- minimum comfortable target: 20-40px (WCAG). Mouse precision @ 15-25px.
--     -- Fitts' Law says edges are infinite, but there's no guarantee that the edge of the
--     -- window will be a *real* edge.
--     maxBuffer = 60
--     bufferRatio = 0.025 -- 2.5%.  Perhaps ≈3% would be even better; let's see.
--     -- but also, a more consistent buffer-size might be simpler and better for users to
--     -- build muscle-memory.  So let's see.
--   in
--     clamp
--       minBuffer
--       maxBuffer
--       (bufferRatio * min w h)

-- makeLinkForces : Graph Entity Connection -> Force.Force NodeId
-- makeLinkForces graph =
--   Graph.fold
--     (\ctx acc ->
--       -- for all outgoing links that AREN'T recursive, create a Force.
--       let
--         outs = ctx.outgoing |> IntDict.filter (\k _ -> k /= ctx.node.id)
--       in
--         IntDict.toList outs
--         |> List.foldl
--           (\(k, v) forces_ ->
--             { source = ctx.node.id
--             , target = k
--             , distance = 80 -- 35-40 seems like a good distance
--             , strength = Just 1.0 -- * (toFloat <| Set.size v)
--             } :: forces_
--           )
--           acc
--     )
--     []
--     graph
--   |> Force.customLinks 3

spreadOutForces : AutomatonGraph -> List (Force.Force NodeId)
spreadOutForces g =
  let
    root = g.root
    graph = g.graph
    seed0 = Random.initialSeed 0xdead [0xbeef, 0xbabe, 0xcafe, 0x10ad, 0xface, 0xfeed, 0x50fa, 0xab1e]
    num_nodes = Graph.size graph
    x_target : Float
    x_target = toFloat <| num_nodes * (200 + 18)
  in
    -- links are the "springs"
    -- manyBody is the "repulsion"
    Graph.fold
      (\ctx (seedy, {toX, toY, link, manyBody}) ->
        let
        -- for all outgoing links that AREN'T recursive, create a link-Force.
          (seed1, linkForce) =
            IntDict.toList ctx.outgoing
            |> List.filter (Tuple.first >> (/=) ctx.node.id)
            |> List.foldl
                (\(k, conn) (seed, forces_) ->
                  let
                    (n, seed_) =
                      Random.step (Random.int 1 150) seed
                    (s, seed__) =
                      Random.step (Random.float 1 3) seed_
                  in
                  ( seed__
                  , { source = ctx.node.id
                    , target = k
                    , distance = 50 + toFloat n + linkLabelWidth conn -- 35-40 seems like a good distance
                    , strength = Just s -- * (toFloat <| Set.size v)
                    } :: forces_
                  )
                )
                (seedy, link)
          (_, seed2) = -- first element is "go_right"
            Random.step (Random.bool) seed1
        in
          if ctx.node.id == root then
            ( seed2
            , { toX = toX
                  --{ node = ctx.node.id , strength = 10.0 , target = 0 } :: toX
              , toY = toY
                  --{ node = ctx.node.id , strength = 10.0 , target = 0 } :: toY
              , link = linkForce
              , manyBody = ctx.node.id :: manyBody
              }
            )
          else
            ( seed2
            , { toX =
                  -- toX
                  { node = ctx.node.id , strength = 0.04
                  -- , target = if go_right then x_target else -x_target
                  , target = x_target
                  } :: toX
              , toY =
                  { node = ctx.node.id , strength = 0.6 , target = 0 } :: toY
              , link = linkForce
              , manyBody = ctx.node.id :: manyBody
              }
            )
      )
      (seed0, { toX = [], toY = [], link = [], manyBody = [] })
      graph
    |>  (\(_, {toX, toY, link, manyBody}) ->
          if num_nodes > 1 then
            [ Force.towardsX toX
            , Force.towardsY toY
            , Force.customLinks 2 link
            , Force.manyBodyStrength -850.0 manyBody
            , Force.center 0 0
            ]
          else
            []
        )

coordinateForces : AutomatonGraph -> List (Force.Force NodeId)
coordinateForces g =
  let
    root = g.root
    graph = g.graph
    num_nodes = Graph.size graph
    x_target : Float
    x_target = toFloat <| num_nodes * (200 + 18)
  in
    -- links are the "springs"
    -- manyBody is the "repulsion"
    Graph.fold
      (\ctx {toX, toY, link, manyBody} ->
        let
        -- for all outgoing links that AREN'T recursive, create a link-Force.
          linkForce =
            IntDict.toList ctx.outgoing
            |> List.filter (Tuple.first >> (/=) ctx.node.id)
            |> List.foldl
                (\(k, conn) forces_ ->
                  { source = ctx.node.id
                  , target = k
                  , distance = 50 + linkLabelWidth conn -- 35-40 seems like a good distance
                  , strength = Just 1.5 -- * (toFloat <| Set.size v)
                  } :: forces_
                )
                link
        in
          if ctx.node.id == root then
            { toX =
                { node = ctx.node.id , strength = 10.0 , target = 0 } :: toX
            , toY =
                { node = ctx.node.id , strength = 10.0 , target = 0 } :: toY
            , link = linkForce
            , manyBody = ctx.node.id :: manyBody
            }
          else
            { toX =
                { node = ctx.node.id , strength = 0.02 , target = x_target } :: toX
            , toY =
                { node = ctx.node.id , strength = 0.01 , target = 0 } :: toY
            , link = linkForce
            , manyBody = ctx.node.id :: manyBody
            }
      )
      { toX = [], toY = [], link = [], manyBody = [] }
      graph
    |>  (\{toX, toY, link, manyBody} ->
          if num_nodes > 1 then
            [ Force.towardsX toX
            , Force.towardsY toY
            , Force.customLinks 3 link
            , Force.manyBodyStrength -350.0 manyBody
            ]
          else
            []
        )

toForceGraph : AutomatonGraph -> AutomatonGraph
toForceGraph g =
  let
    initializeNode : Node -> NodeContext Entity Connection
    initializeNode ctx =
      { node =
        { label =
            entity ctx.node.id NoEffect
        , id = ctx.node.id
        }
      , incoming = ctx.incoming
      , outgoing = ctx.outgoing
      }
  in
    { graph = Graph.mapContexts initializeNode g.graph
    , description = Nothing
    , root = g.root
    }

type alias ComputeGraphResult =
  { simulation : Force.State NodeId
  , solvedGraph : AutomatonGraph
  , forces : List (Force.Force NodeId)
  }

computeGraphFully : (AutomatonGraph -> List (Force.Force NodeId)) -> AutomatonGraph -> ComputeGraphResult
computeGraphFully computer g =
  let
    forceGraph =
      toForceGraph (g {- |> Debugging.debugAutomatonGraph "Graph as received" -})
      -- |> Debugging.debugAutomatonGraph "After toForceGraph"
    forces_ = computer forceGraph
    nodes = Graph.nodes forceGraph.graph
    simulation =
      Force.simulation forces_
    shiftedNodes =
      Force.computeSimulation
        simulation
        (List.map .label nodes)
    resultingGraph =
      -- no changes to node-ids are made; only the spatial positions change.
      { forceGraph | graph = updateGraphWithList forceGraph.graph shiftedNodes }
      -- |> Debugging.debugAutomatonGraph "After sim"
  in
    -- no changes to node-ids are made; only the spatial positions change.
    { solvedGraph = resultingGraph
    , simulation = simulation
    , forces = forces_
    }
    -- |> Debugging.debugAutomatonGraph "After sim"

-- updateNode : ( Float, Float ) -> ( Float, Float ) -> NodeContext Entity Connection -> NodeContext Entity Connection
-- updateNode (offsetX, offsetY) (x, y) nodeCtx =
--   updateContextWithValue
--     nodeCtx
--     ( setXY (x + offsetX) (y + offsetY) nodeCtx.node.label )

nodeDrawingForPackage : AutomatonGraph -> Uuid -> Dict NodeId NodeDrawingData
nodeDrawingForPackage ag graphView_uuid =
  let
    disconnectedNodes =
      DFA.identifyDisconnectedNodes ag
    isSplittable : Graph.NodeContext Entity Connection -> Bool
    isSplittable graphNode =
      let
        nonRecursive =
          IntDict.filter (\k _ -> k /= graphNode.node.id) graphNode.incoming
      in
        IntDict.size nonRecursive > 1 ||
        ( IntDict.findMin nonRecursive
          |> Maybe.map (\(_, conn) -> AutoSet.size conn > 1)
          |> Maybe.withDefault False
        )
  in
    Graph.nodes ag.graph
    |> List.map
      (\node ->
        let
          nodeContext =
            Graph.get node.id ag.graph
        in
          ( node.id
          , { isDisconnected = Set.member node.id disconnectedNodes
            , isTerminal =
                Maybe.map isTerminalNode nodeContext
                |> Maybe.withDefault False
            , coordinates = Coordinate node.label.x node.label.y
            , isRoot = node.id == ag.root
            , canSplit =
                Maybe.map isSplittable nodeContext
                |> Maybe.withDefault False
            , view_uuid = graphView_uuid
            , isSelected = False
            }
          )
      )
    |> Dict.fromList

identifyCardinalityViaContext : NodeId -> Graph.NodeContext Entity Connection -> Cardinality
identifyCardinalityViaContext from to =
  if to.node.id == from then
    Recursive
  else if Q.linkExistsInGraph to from then -- i.e. in opposite direction
    Bidirectional
  else
    Unidirectional

path_between : CoordinateLike a -> CoordinateLike b -> Cardinality -> PathBetweenReturn
path_between sourceXY_orig destXY_orig cardinality =
  {- we're doing a curved line, using a quadratic path.
      So, let's make a triangle. The two points at the "base" are the
      start and end of the connection ("source" and "target").  Now,
      take two lines at an angle Θ from both sides, and where they
      meet is our control point.  We can then adjust the angle "up" and
      "down" until we are satisfied with the look.
      
      Of course, because the angles are equal, the length of the lines is
      also equal.  So another equivalent way of going about it is by getting
      the midpoint and then pushing a line "up", orthogonal to that midpoint,
      and saying that this is the control point.  As the length of the line
      increases, the angle increases too.
  --}
  let
    nodeRadius = 7
    radius_from = 7
    radius_to = 5
    sourceXY =
      case cardinality of
        Recursive ->
          { x = sourceXY_orig.x - nodeRadius
          , y = sourceXY_orig.y
          }
        _ ->
          { x = sourceXY_orig.x, y = sourceXY_orig.y }
    destXY =
      case cardinality of
        Recursive ->
          { x = sourceXY_orig.x + nodeRadius
          , y = sourceXY_orig.y
          }
        _ ->
          { x = destXY_orig.x, y = destXY_orig.y }
    d_y = sourceXY.y - destXY.y
    d_x = sourceXY.x - destXY.x
    orig_line_len = sqrt (d_x * d_x + d_y * d_y)
    curvature =
      case cardinality of
        Bidirectional ->
          1/e -- ± to ± length, and therefore curvature.  Sensible range is 0-1.
        Unidirectional ->
          0
        Recursive ->
          e
    orthogonal_len =
      curvature * orig_line_len
    orthogonal_vector =
      -- normalised
      { x = (destXY.y - sourceXY.y) / orig_line_len
      , y = (destXY.x - sourceXY.x) / orig_line_len
      }
    parametric_direction_vector =
      -- normalised
      { y = (destXY.y - sourceXY.y) / orig_line_len
      , x = (destXY.x - sourceXY.x) / orig_line_len
      }
    half_len = orig_line_len / 2
    midPoint =
      -- I can do this early because the midpoint should remain the midpoint,
      -- no matter how I place the actual targets
      { x = destXY.x - half_len * parametric_direction_vector.x
      , y = destXY.y - half_len * parametric_direction_vector.y
      }
    -- with the midpoint, I can work out the control points.
    control_point =
      { x = midPoint.x + orthogonal_len * orthogonal_vector.x
      , y = midPoint.y - orthogonal_len * orthogonal_vector.y
      }
    hypotenuse_len =
      sqrt (half_len * half_len + orthogonal_len * orthogonal_len)
    -- now, with the control point, I can make the source & target hypotenuse vectors
    source_hypotenuse_vector =
      -- normalised
      { x = ( control_point.x - sourceXY.x ) / hypotenuse_len
      , y = ( control_point.y - sourceXY.y ) / hypotenuse_len
      }
    dest_hypotenuse_vector =
      { x = -( control_point.x - destXY.x ) / hypotenuse_len
      , y = -( control_point.y - destXY.y ) / hypotenuse_len
      }
    shorten_source =
      { x = sourceXY.x + source_hypotenuse_vector.x * radius_from
      , y = sourceXY.y + source_hypotenuse_vector.y * radius_from
      }
    shorten_target =
      -- the extra addition is for the stroke-width (which is 3px)
      { x = destXY.x - dest_hypotenuse_vector.x * (radius_to * 2 + 8) --- parametric_direction_vector.x * 10
      , y = destXY.y - dest_hypotenuse_vector.y * (radius_to * 2 + 8) --- parametric_direction_vector.y * 10
      }
    line_len =
      let
        dx = shorten_target.x - shorten_source.x
        dy = shorten_target.y - shorten_source.y
      in
        sqrt (dx * dx + dy * dy)
    -- control_point =
    --   { x = sourceXY.x + hypotenuse_len * radius_offset_x
    --   , y = sourceXY.y - hypotenuse_len * radius_offset_y
    --   }
    linePath =
      case cardinality of
        Recursive ->
          "M " ++ String.fromFloat (shorten_source.x)
          ++ " " ++ String.fromFloat (shorten_source.y)
          ++ " c -14,-14 28,-14 " ++ String.fromFloat (nodeRadius * 2) ++ ",0"
          --      ^    ^  ^   ^
          --      a    b  c   d
          -- to "raise" it, increase the numerical values of b and d (e.g. to -25 and -25).
          -- to "widen" it, increase the numerical values of a and c (e.g. to -21 and 35).
          -- increase/decrease numbers by the same amount to maintain symmetry.
          -- the last two numbers give the offset for the destination.
        _ ->
          "M " ++ String.fromFloat (shorten_source.x)
          ++ " " ++ String.fromFloat (shorten_source.y)
          ++ " Q " ++ String.fromFloat control_point.x
          ++ " " ++ String.fromFloat control_point.y
          ++ " " ++ String.fromFloat (shorten_target.x)
          ++ " " ++ String.fromFloat (shorten_target.y)
    transition_coordinates =
      { x = midPoint.x + (orthogonal_len / 2) * orthogonal_vector.x --+ control_vector.x / 2
      , y = midPoint.y - (orthogonal_len / 2) * orthogonal_vector.y --+ control_vector.y / 2
      }
  in
    { pathString = linePath
    , transition_coordinates = transition_coordinates
    , length = line_len
    , control_point = control_point
    , source_connection_point = shorten_source
    , target_connection_point = shorten_target
    }

linkDrawingForEdge : Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Connection -> PackageDict -> ( (NodeId, NodeId), LinkDrawingData )
linkDrawingForEdge sourceNode destNode connection packages =
  let
    cardinality : Cardinality
    cardinality =
      identifyCardinalityViaContext sourceNode.node.id destNode
  in
    ( (sourceNode.node.id, destNode.node.id)
    , { cardinality = cardinality
      , executionData = Nothing
      , graphReferenceDescriptions =
          Q.descriptionsForConnection connection packages
      , connection = connection
      , pathBetween =
          path_between
            sourceNode.node.label
            destNode.node.label
            cardinality
      }
    )

linkDrawingForPackage : AutomatonGraph -> PackageDict -> Dict (NodeId, NodeId) LinkDrawingData
linkDrawingForPackage ag packages =
  let
    edgeContexts =
      Graph.edges ag.graph
      |> List.filterMap
          (\edge ->
            Maybe.andThen2
              (\f t ->
                -- this will happen when there is a "removed" edge,
                -- but it hasn't been confirmed yet.
                -- The "invisible" link will anchor with forces, rather
                -- than having the disconnected nodes fly off into the
                -- wild blue yonder…
                if AutoSet.isEmpty edge.label then
                  Nothing
                else
                  Just { sourceNode = f, destNode = t, label = edge.label })
              (Graph.get edge.from ag.graph)
              (Graph.get edge.to ag.graph)
          )
  in
    edgeContexts
    |> List.map
        (\{sourceNode, destNode, label} ->
          linkDrawingForEdge sourceNode destNode label packages
        )
    |> Dict.fromList

recalculateLinksAndNodes : PackageDict -> GraphView -> GraphView
recalculateLinksAndNodes packages graph_view =  
  { graph_view
    | drawingData =
        let dd = graph_view.drawingData in
        { dd
          | link_drawing = linkDrawingForPackage graph_view.computation packages
          , node_drawing = nodeDrawingForPackage graph_view.computation graph_view.id
        }
    , disconnectedNodes = DFA.identifyDisconnectedNodes graph_view.computation
  }


updateContextWithValue : NodeContext Entity Connection -> Entity -> NodeContext Entity Connection
updateContextWithValue ({node} as nodeCtx) value =
  { nodeCtx | node = { node | label = value } }

updateGraphWithList : Graph Entity Connection -> List Entity -> Graph Entity Connection
updateGraphWithList =
  let
    graphUpdater value =
      Maybe.map (\ctx -> updateContextWithValue ctx value)
  in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)

-- need to expose this; it is used from testing.
applyChangesToGraph : AutomatonGraph -> AutomatonGraph
applyChangesToGraph g =
  -- debugAutomatonGraph "Initial from user" g |> \_ ->
  DFA.removeDisconnectedNodes g
  |> debugAutomatonGraph "After removing disconnected"
  |> (DFA.fromAutomatonGraph >> DFA.toAutomatonGraph)
  |> (\g_ -> { g_ | description = g.description }) -- restore description.
  |> debugAutomatonGraph "After conversion round-trip"

newnode_graphchange : NodeId -> Float -> Float -> Connection -> AutomatonGraph -> AutomatonGraph
newnode_graphchange src x y conn g =
  { g
    | graph =
        let
          id = maxId g + 1

          initial = entity id NoEffect
        in
          Graph.insert
            { node =
              { label = initial |> setXY x y
              , id = id
              }
            , incoming = IntDict.singleton src conn
            , outgoing = IntDict.empty
            }
            g.graph
  }
  -- |> debugAutomatonGraph "[create_union_graphchange] updated graph"

updateLink_graphchange : NodeId -> NodeId -> Connection -> AutomatonGraph -> AutomatonGraph
updateLink_graphchange src dest conn g =
  { g
    | graph =
        Graph.update dest
          (Maybe.map (\node ->
            { node | incoming = IntDict.insert src conn node.incoming }
          ))
          g.graph
      -- if it's recursive, I must add it to both so that it reflects in the graph.
      -- in other cases, this should do no harm.
      |> Graph.update src
        (Maybe.map (\node ->
          { node | outgoing = IntDict.insert dest conn node.outgoing }
        ))
  }
  -- |> debugAutomatonGraph "[create_update_graphchange] updated graph"

removeLink_graphchange : NodeId -> NodeId -> AutomatonGraph -> AutomatonGraph
removeLink_graphchange src dest g =
  -- here, we "update" and set the connection to empty.
  -- why not remove it?  That WOULD be more correct!
  -- Because if we remove it, then the _forces_ link is gone, and
  -- if the simulation runs, then the disconnected portion will fly off
  -- somewhere into the ether.  That's not what we want to happen.
  -- So, we fake it here, and figure it out and remove it later on.
  { g
    | graph =
        Graph.update dest
          (Maybe.map
            (\node ->
              { node
                | incoming =
                    IntDict.update src
                      (Maybe.map (\_ -> AutoSet.empty transitionToString))
                      node.incoming
                , outgoing =
                    if src == dest then
                      IntDict.update dest
                        (Maybe.map (\_ -> AutoSet.empty transitionToString))
                        node.outgoing
                    else
                      node.outgoing
              })
          )
          g.graph
  }
  -- |> debugAutomatonGraph "[create_removal_graphchange] updated graph"

panGraphView : Int -> Int -> GraphView -> GraphView
panGraphView xMovement yMovement ({guest, pan, computation, properties} as graph_view) =
  let
    ( pan_x, pan_y ) = pan
    -- this is a rectangle that is inset from the maximum bounds.
    movement_amount_x =
      -- if the space to traverse is large, then move by a larger amount.
      max 1 (ceiling (guest.w / 250)) |> toFloat
    movement_amount_y =
      -- if the space to traverse is large, then move by a larger amount.
      max 1 (ceiling (guest.h / 250)) |> toFloat
    ( new_pan_x, new_pan_y ) =
      ( pan_x + toFloat xMovement * movement_amount_x
      , pan_y + toFloat yMovement * movement_amount_y
      )
    ((sx, sy), (ex, ey)) = -- this is after the proposed pan.
      ( ((guest.x + new_pan_x), (guest.y + new_pan_y))
      , ((guest.x + new_pan_x + guest.w), (guest.y + new_pan_y + guest.h))
      )
    graph_nodes =
      Graph.nodes computation.graph
      -- |> debugLog_ "[panGraphView] nodes to check" (List.map .id)
    is_okay =
      List.any
        (\node ->
          -- we want to check whether any node is within the screen bounds.
          -- debugLog_ "[panGraphView] Checking against " (\v -> (v.id, v.label.x ,v.label.y)) node |> \_ ->
          node.label.x > sx &&
          node.label.y > sy &&
          node.label.x < ex &&
          node.label.y < ey
        )
        graph_nodes
      -- |> Debug.log "[panGraphView] is okay?"
    is_really_okay =
      is_okay ||
        -- here's where it gets … tricky.
        -- see, there can be a node on the screen.  But due to the aspect ratio,
        -- it may be that there is only a few nodes on the screen—and the remaining
        -- nodes are off the screen, but separated by a distance that is larger than the
        -- screen width/height.  In such a case, we won't be able to pan to them with the
        -- `is_okay` check, because we will be prevented from panning that drives all of
        -- the existing nodes off the screen.  So we are, effectively, trapped in a local
        -- minimum!
        --
        -- So, how do we address this?
        --
        -- Well, first of all, this is an edge-case.  So if the cheap check passes, then
        -- we don't actually want to do any other checks.  But if the cheap check doesn't
        -- pass, we've got more work to do.  And this is that work right here…
        -- We must check whether we are HEADING TO another node, and if so, THEN we can
        -- permit the pan.
        ( let
            orig_center = Math.Vector2.vec2 (guest.x + pan_x + guest.w / 2) (guest.y + pan_y + guest.h / 2)
            new_center = Math.Vector2.vec2 ((sx + ex) / 2) ((sy + ey) / 2)
          in
            -- am I heading towards any graph node?
            List.any
              (\node ->
                let
                  node_center = Math.Vector2.vec2 node.label.x node.label.y
                in
                  distance node_center new_center < distance node_center orig_center
              )
              graph_nodes
        )
    activePanDirection =
      -- working around a compiler bug, which is why I have this +1 nonsense.
      case xMovement+1 of
        0 ->
          case yMovement+1 of
            0 -> Just ToTopLeft
            2 -> Just ToBottomLeft
            1 -> Just ToLeft
            _ -> Nothing
        1 ->
          case yMovement+1 of
            0 -> Just ToTop
            2 -> Just ToBottom
            _ -> Nothing
        2 ->
          case yMovement+1 of
            0 -> Just ToTopRight
            2 -> Just ToBottomRight
            1 -> Just ToRight
            _ -> Nothing
        _ -> Nothing
  in
    if properties.canPan && is_really_okay then
      { graph_view
        | pan = ( new_pan_x , new_pan_y )
        , activePanDirection = activePanDirection
      }
    else
      { graph_view | activePanDirection = Nothing }


{-| (2+7n)×(2+4n)-pixel UUID-representing badges.
    
    Sensible sizes are:
    - 2 = 16x10px badge
    - 4 = 30x18px badge
-}
viewGraphReference : Uuid -> Int -> Float -> Float -> Svg a
viewGraphReference uuid scale x_ y_ =
  let
    pixels = getPalette uuid
    pixelSize = toFloat scale -- 4
  in
    g
      [ class [ "graph-badge" ] ]
      ( rect
          [ x <| x_
          , y <| y_
          , width <| (7 * pixelSize) + 2
          , height <| (4 * pixelSize) + 2
          , rx 2
          , ry 2
          -- , fill <| Paint <| Color.black
          , class [ "backing" ]
          ]
          []
      -- each one is a 4px square
      :: Tuple.second
          (List.foldl
            (\color (i, acc) ->
              ( i + 1
              , rect
                  [ x <| x_ + 1 + pixelSize * (modBy 7 i |> toFloat)
                  , y <| y_ + 1 + pixelSize * (modBy 4 i |> toFloat)
                  , width pixelSize
                  , height pixelSize
                  , fill <| Paint color
                  -- , stroke <| Paint <| Color.black
                  -- , strokeWidth 1
                  , rx (pixelSize * 0.15)
                  , ry (pixelSize * 0.15)
                  ]
                  []
                :: acc
              )
            )
            (0, [])
            pixels
          )
      )

char_width : Float
char_width = 16 / 1.85 -- FCC is probably anywhere from ≈2.2-2.55 for sans serif. Can be adjusted.
badge_width : Float
badge_width = 16 -- for a scale of "2".
gap : Float
gap = 2

linkLabelWidth : Connection -> Float
linkLabelWidth connection =
  let
    sum_width =
      AutoSet.foldl
        (\{via} acc ->
          case via of
            ViaCharacter _ -> acc + char_width
            ViaGraphReference _ -> acc + badge_width
        )
        0
        connection
    sum_gap =  gap * ( toFloat <| AutoSet.size connection - 1 )
  in
    -- this is an estimate.
    sum_width + sum_gap

viewLinkLabel : LinkDrawingData -> Svg Msg
viewLinkLabel drawing_data =
  let
    x_ = drawing_data.pathBetween.transition_coordinates.x
    y_ = drawing_data.pathBetween.transition_coordinates.y
    est_width = linkLabelWidth drawing_data.connection
  in
  AutoSet.foldl
    (\{via, isFinal} (cur_x, cur_y, elems) ->
      case via of
        ViaCharacter ch ->
          ( cur_x + char_width + gap
          , cur_y
          , ( g
              [ class [ "transition", "text", if isFinal then "final" else "nonfinal" ] ]
              [ text_
                  [ x cur_x
                  , y cur_y
                  ]
                  [ text <| String.fromChar ch ]
              ]
            ) :: elems
          )
        ViaGraphReference uuid ->
          ( cur_x + badge_width + gap
          , cur_y
          , ( g
                [ class [ "transition", "graph-reference", if isFinal then "final" else "nonfinal" ]
                ]
                [ viewGraphReference uuid 2 cur_x (cur_y - 5)
                , title
                    []
                    [ ( ( AutoDict.get uuid drawing_data.graphReferenceDescriptions
                          |> Maybe.withDefault "(no description)"
                        )
                        ++ "\n\n" ++ truncate_uuid uuid
                      )
                      |> text
                    ]
                ]
            ) :: elems
          )
    )
    ( x_ - 0.5 * est_width, y_, [] )
    drawing_data.connection
  |> (\(_, _, elems) -> g [] elems)

viewLink : GraphView -> (NodeId, NodeId) -> DrawingData -> LinkDrawingData -> Svg Msg
viewLink {id, properties} (from, to) {highlighted_links, tentative_link, lowlighted_links} drawing_data =
      let
        x_ = drawing_data.pathBetween.transition_coordinates.x
        y_ = drawing_data.pathBetween.transition_coordinates.y
        transitionLabel : Svg Msg
        transitionLabel =
          case drawing_data.executionData of
            Just _ ->
              g
                -- … will maybe figure out recency later on, in sha Allah…
                [ class [ "executed" ] ]
                [ viewLinkLabel drawing_data ]
              -- connectionToSvgTextHighlightingChars
              --   drawing_data.label
              --   (\via ->
              --     AutoDict.get via chosen
              --     |> Maybe.map (\recency ->
              --       [ "executed", "recent-" ++ String.fromInt recency ]
              --     )
              --     |> Maybe.withDefault []
              --   )
            Nothing ->
              viewLinkLabel drawing_data
        linkClass =
          conditionalList
            [ ( "link", True )
            , ( "phantom", tentative_link == Just (from, to))
            , ( "highlight", Set.member (from, to) highlighted_links )
            , ( "lowlight", Set.member (to, from) lowlighted_links )
            ]
      in
        g
          [ class [ "link-group" ] ]
          [
            path
              [ d drawing_data.pathBetween.pathString
              , class ( "background" :: linkClass )
              ]
              [ {- title [] [ text <| Data.connectionToString edge.label ] -} ]
          , path
              [ d drawing_data.pathBetween.pathString
              , class ( "foreground" :: linkClass )
              ]
              [ {- title [] [ text <| Data.connectionToString edge.label ] -} ]
          , g
              [ class linkClass
              , properties.canSelectConnections
                |> thenPermitSvgInteraction
                    (onClick (EditConnection {x = x_, y = y_} id from to drawing_data.connection))
              ]
              [ transitionLabel
              , if properties.canSelectConnections then
                  title [] [ text "Click to modify" ]
                else
                  g [] []
              ]
          -- , rect
          --     [ x <| positioning.transition_coordinates.x
          --     , y <| positioning.transition_coordinates.y - 70
          --     , width 28
          --     , height 16
          --     , rx 2
          --     , ry 2
          --     , fill <| Paint <| Color.black
          --     ]
          --     []
          -- , rect
          --     [ x <| positioning.transition_coordinates.x + 4
          --     , y <| positioning.transition_coordinates.y - 70 + 4
          --     , width 4
          --     , height 4
          --     , fill <| Paint <| Color.white
          --     ]
          --     []

          -- for debugging the paths.
          -- , circle
          --     [ cx <| positioning.control_point.x
          --     , cy <| positioning.control_point.y
          --     , r 3
          --     , fill <| Paint <| Color.red
          --     ]
          --     []
          -- , circle
          --     [ cx <| positioning.source_connection_point.x
          --     , cy <| positioning.source_connection_point.y
          --     , r 3
          --     , fill <| Paint <| Color.black
          --     ]
          --     []
          -- , circle
          --     [ cx <| positioning.target_connection_point.x
          --     , cy <| positioning.target_connection_point.y
          --     , r 3
          --     , fill <| Paint <| Color.yellow
          --     ]
          --     []
          ]

viewNode : GraphViewProperties -> NodeId -> DrawingData -> NodeDrawingData -> Svg Msg
viewNode properties id {tentative_link} data =
  let
    nodeClass =
      conditionalList
        [ ("graph-node", True)
        , ("selected"
          , Maybe.map (Tuple.first >> (==) id) tentative_link
            |> Maybe.withDefault False
          )
        -- , ("current", data.exclusiveAttributes == Just DrawCurrentExecutionNode)
        , ( "phantom"
          , Maybe.map (Tuple.second >> (==) id) tentative_link
            |> Maybe.withDefault False
          )
        , ("disconnected", data.isDisconnected)
        , ("start", data.isRoot)
        , ("terminal", data.isTerminal)
        ]

    fewDP n = toFloat (round (n * 10)) / 10

    titleText =
      (if data.isTerminal && data.isRoot then
        "Start AND end of computation\n"
      else if data.isTerminal then
        "End of computation\n"
      else if data.isRoot then
        "Start of computation\n"
      else
        "") ++
      (if properties.canDragNodes then
          "Drag to reposition\n"
        else
          ""
      ) ++
      (if data.canSplit && properties.canSplitNodes then
            "\nCtrl-click to split transitions"
          else
            ""
      ) ++
      (if properties.canSelectNodes then
        "\nClick to create or link a new transition"
      else
        ""
      )
      ++ "\n#" ++ String.fromInt id ++ "; " ++ String.fromFloat (fewDP data.coordinates.x) ++ ", " ++ String.fromFloat (fewDP data.coordinates.y) -- DEBUGGING          
  in
    g
      [ class nodeClass
      , if properties.canSelectNodes || (properties.canSplitNodes && data.canSplit) then
          TypedSvg.Events.on "click"
            (VirtualDom.Normal
              ( D.field "ctrlKey" D.bool
                |> D.andThen (\ctrlPressed ->
                  if ctrlPressed then
                    if data.canSplit && properties.canSplitNodes then
                      D.succeed (GraphViewMsg data.view_uuid <| StartSplit id)
                    else
                        D.fail "Unwanted event"
                  else if properties.canSelectNodes then
                    D.map2
                      (\x y -> GraphViewMsg data.view_uuid <| SelectNode id { x = x, y = y })
                      (D.field "clientX" D.float)
                      (D.field "clientY" D.float)
                  else
                    D.fail "Unwanted event"
                )
              )
            )
        else
          TypedSvg.Events.on "dummy" (VirtualDom.Normal <| D.fail "dummy event")
      -- , (properties.canSplitNodes && data.canSplit)
      --   |> thenPermitSvgInteraction (onClick <| StartSplit data.view_uuid id)
      , if properties.canDragNodes then
          TypedSvg.Events.on "mousedown"
            (VirtualDom.Normal
              ( D.field "shiftKey" D.bool
                |> D.andThen (\shiftPressed ->
                  if shiftPressed then
                    D.map2
                      (\x y -> GraphViewMsg data.view_uuid <| StartDraggingNode id {x = x, y = y})
                      (D.field "clientX" D.float)
                      (D.field "clientY" D.float)
                  else
                    D.fail "Unwanted event"
                )
              )
            )
        else
          TypedSvg.Events.on "dummy" (VirtualDom.Normal <| D.fail "dummy event")
      ]
      [ circle
          [ cx data.coordinates.x
          , cy data.coordinates.y
          , class nodeClass
          ]
          []
      , title [] [text titleText]
      ]


arrowheadDefs : List (Svg msg)
arrowheadDefs =
  let
    mkArrowhead : String -> Svg msg
    mkArrowhead id_ =
      marker
        [ id id_
        , viewBox 0 0 10 10
        , refX "0"
        , refY "5"
        , orient "auto-start-reverse"
        , markerWidth 5
        , markerHeight 5
        ]
        [ path
          [ d "M 0 0 L 10 5 L 0 10 z" ]
          []
        ]
  in
    [ mkArrowhead "arrowhead"
    , mkArrowhead "phantom-arrowhead"
    , mkArrowhead "highlight-arrowhead"
      --strokeWidth 1.5
      -- , strokeDasharray "1.5 2"
      -- , strokeLinecap StrokeLinecapRound
    , mkArrowhead "hover-arrowhead"
    ]

-- transition_buttonSize : Float
-- transition_buttonSize = 55

-- transition_spacing : Float
-- transition_spacing = 15


viewUndoRedoVisualisation : GraphView -> Svg a
viewUndoRedoVisualisation { undoBuffer, redoBuffer, guest } =
  let
    bottom_y = guest.y + guest.h
    rect_width = 30
    rect_height = 10
    rect_spacing = 3
    num_undo = List.length undoBuffer
    idxToY idx =
      bottom_y - (toFloat (15 + idx * (rect_height + 1) + idx * (rect_spacing - 1)))
  in
    g
      []
      (
        ( List.indexedMap
            (\idx _ ->
              rect
                [ TypedSvg.Attributes.InPx.width rect_width
                , TypedSvg.Attributes.InPx.height rect_height
                , fill <| Paint (List.getAt idx colorScale.svg.best_to_worst |> Maybe.withDefault colorScale.svg.worst)
                , ry 2
                , x (guest.x + 5)
                , y <| idxToY idx
                , class ["undo", if num_undo - 1 == idx then "current" else ""]
                ]
                []
            )
            undoBuffer
        )
        ++
        ( List.indexedMap
            (\idx _ ->
              rect
                [ TypedSvg.Attributes.InPx.width rect_width
                , TypedSvg.Attributes.InPx.height rect_height
                , ry 2
                , x (guest.x + 5)
                , y <| idxToY (idx + num_undo)
                , class ["redo"]
                ]
                []
            )
            redoBuffer
        )
      )

panToString : (Float, Float) -> String
panToString pan =
  case Tuple.mapBoth round round pan of
    ( 0, 0 ) -> "centered"
    ( x, y) ->
      let
        xString = if x > 0 then "+" ++ String.fromInt x else String.fromInt x
        yString = if y > 0 then "+" ++ String.fromInt y else String.fromInt y
      in
      ("(" ++ xString ++ ", " ++ yString ++ ")")

matrixFromZoom : (Float, Float) -> Float -> {- ( Float, Float ) -> -} Transform
matrixFromZoom (panX, panY) factor {- (pointerX, pointerY) -} =
{- https://www.petercollingridge.co.uk/tutorials/svg/interactive/pan-and-zoom/

  The matrix is the "usual":

  ⎾ a c e ⏋ ⎾ x ⏋
  | b d f | | y |
  ⎿ 0 0 1 ⏌ ⎿ 1 ⏌

  - the new x-coordinate of each element is a・x + c・y + e
  - the new y-coordinate of each element is b・x + d・y + f
-}

  Matrix
    factor
    0
    0
    factor
    ( (1 - factor) {- * pointerX -} - panX )
    ( (1 - factor) {- * pointerY -} - panY )
  -- |> Debug.log "matrix transform applied"

viewMainSvgContent : GraphView -> Svg Msg
viewMainSvgContent graph_view =
  g -- this is the "main" interactive frame, which will be zoomed, panned, etc.
    [ transform [ matrixFromZoom graph_view.pan 1.0 ]
    ]
    [ defs [] arrowheadDefs
      -- this part is for debugging panning. It shows the "original" viewport,
      -- sans panning.
    -- , rect
    --     [ stroke <| Paint Color.lightRed
    --     , fill <| PaintNone
    --     , Px.strokeWidth 2
    --     , Px.x <| Tuple.first graph_view.guest_coordinates
    --     , Px.y <| Tuple.second graph_view.guest_coordinates
    --     , Px.width <| Tuple.first graph_view.guest_dimensions
    --     , Px.height <| Tuple.second graph_view.guest_dimensions
    --     ]
    --     []
    , Dict.toList graph_view.drawingData.link_drawing
      -- draw any phantom link last, because it should be displayed on top of everything else.
      |> List.sortBy (\(edge, _) -> if Set.member edge graph_view.drawingData.highlighted_links then 1 else 0)
      |> List.map (\((from, to), data) -> viewLink graph_view (from, to) graph_view.drawingData data)
      |> g [ class [ "edges" ] ]
    , Dict.toList graph_view.drawingData.node_drawing
      |> List.map (\(nodeId, data) -> viewNode graph_view.properties nodeId graph_view.drawingData data)
      |> g [ class [ "nodes" ] ]
    ]

-- {-| Takes a guess at where a string should be broken into words.

--     Characters-per-line = line-in-px / average-character-width
--     Avg-character-width = font-size / font-character-constant

--     CPL = LIP / ACW = LIP / (FS / FCC)

--     That "font-character-constant" is the key metric, and it is
--     typeface-specific. All other parts of this can be known.
--     Some values for FCC:
--     - Trebuchet: 2.2
--     - Arial: 2.26
--     - Times New Roman: 2.48
--     - Baskerville: 2.51

--     (see https://pearsonified.com/characters-per-line/)

--     - `fcc`: font-character-constant
--     - `px`: font-size in pixels
--     - `width`: this is the width of a line, in px.

--     We return a list of strings.
-- -}
-- breakStringBadly : Float -> Int -> Float -> String -> List String
-- breakStringBadly fcc px width text =
--   let
--     acw = toFloat px / fcc
--     cpl = width / acw |> floor -- |> Debug.log "CPL"
--     -- now break up the text along whitespace
--     nextWordSize charList count word =
--       case charList of
--         ' '::((' '::_) as t) ->
--           nextWordSize t count word -- we'll get it next time 'round
--         ' '::rest ->
--           (count, String.fromList <| List.reverse word, rest)
--         c::rest ->
--           nextWordSize rest (count + 1) (c::word)
--         [] ->
--           (count, String.fromList <| List.reverse word, [])
--     break charList n current acc =
--       -- `n` is the number of words on the current line
--       let
--         (next_word_size, next_word, remaining) =
--           nextWordSize charList 0 []
--       in
--         if List.isEmpty charList then
--           -- base case
--           case current of
--             [] ->
--               List.reverse acc
--             _ ->
--               List.reverse (String.join " " (List.reverse current) :: acc)
--         else
--           -- still some work to do.
--           if n == 0 then
--             if n + next_word_size > cpl then
--               -- We are at the start of the line; there is nothing to do but
--               -- put this word in anyway, and let it exceed bounds. It's too large.
--               -- So, put it in, move to the next line, and carry on.
--               break remaining 0 [] (next_word :: acc)
--             else -- we're still under the limit
--               -- n+1 because we have to account for the space to be inserted after.
--               break remaining (n + 1 + next_word_size) (next_word :: current) acc
--           else -- words to the left of me, ?? to the right.
--             if n + next_word_size > cpl then
--               -- skip to the next line.
--               break remaining (next_word_size + 1) [next_word] (String.join " " (List.reverse current) :: acc)
--             else
--               -- keep going
--               break remaining (n + 1 + next_word_size) (next_word :: current) acc
--   in
--     break (String.toList text) 0 [] []

viewGraph : GraphView -> Html Msg
viewGraph ({guest, host, properties, id, pan, activePanDirection} as graphView) =
  let
    mkArrow : String -> String -> Html Msg
    mkArrow direction pathString =
      -- this will be laid out in CSS via grid
      div
        [ HA.class <| "pan-region " ++ direction
        ]
        [ svg
            [ class [ "pan-arrow" ]
            , viewBox 0 0 20 20
            ]
            [ path [ d pathString ] [] ]
          |> Html.Styled.fromUnstyled
        ]
  in
    div
      [ HA.class "graph-container"
      , HA.property "uiType" (E.string "graph-view")
      , HA.css
          [ Css.width (Css.px host.w)
          , Css.height (Css.px host.h)
          ]
      , HA.attribute "graph-id" (Uuid.toString id)
      -- , HE.onMouseOver (GraphViewMsg gv RequestCoordinates)
      ]
      [ svg
          ([ viewBox guest.x guest.y guest.w guest.h
          , TypedSvg.Attributes.InPx.width host.w
          , TypedSvg.Attributes.InPx.height host.h
          , class
              ( conditionalList
                  [ ("graph", True)
                  , ("can-select-nodes", properties.canSelectNodes)
                  , ("can-select-connections", properties.canSelectConnections)
                  , ("can-split-nodes", properties.canSplitNodes)
                  , ("can-select-space", properties.canSelectEmptySpace)
                  ]
              )
          , properties.canSelectEmptySpace
            |> thenPermitSvgInteraction (onClick (GraphViewMsg id SelectSpace))
          ] {- ++ interactivity -})
          [ -- this stuff is in the background.
            viewUndoRedoVisualisation graphView
            -- this part is for debugging panning. If I uncomment it, I should also
            -- uncomment the corresponding code in viewMainSvgContent.
          , viewMainSvgContent graphView -- this is the "main" interactive frame, which will be zoomed, panned, etc.
          , if not properties.canPan then
              g [] []
            else
              g
                [ ]
                [ -- text_
                    -- [ x <| (guest_x + guest_width) - 80
                    -- , y <| (guest_y + guest_height) - 10
                    -- , class [ "status-line", "zoom" ]
                    -- ]
                    -- [ text (" 🔍 " ++ String.fromInt (round <| graphView.zoom * 100) ++ "%") ]
                  text_
                    [ x <| (guest.x + guest.w) - 5
                    , y <| (guest.y + guest.h) - 10
                    , class [ "status-line", "pan" ]
                    ]
                    [ tspan
                        ( if pan == (0, 0) then
                            []
                          else
                            [ class [ "pan-reset" ]
                            , properties.canPan
                              |> thenPermitSvgInteraction (onClick (GraphViewMsg id ResetPan))
                            ]
                        )
                        [ text "🧭"
                        , title [] [ text "Re-center" ]
                        ]
                    , tspan
                        []
                        [ text <| " " ++ panToString pan
                        ]
                    ]
                -- , case graphView.interactionsDict of
                --     Just (ModifyingGraph _ { dest }) ->
                --       case dest of
                --         NoDestination ->
                --           bottomMsg "Press «Esc» to cancel link creation"
                --         ExistingNode _ ->
                --           bottomMsg "Choose transitions to connect these nodes. Press «Esc» to cancel."
                --         NewNode _ ->
                --           bottomMsg "Choose transitions for this link. Press «Esc» to cancel."
                --     Just (AlteringConnection _ _) ->
                --       bottomMsg "Choose transitions for this link. Press «Esc» to cancel."
                --     Just (Splitting _) ->
                --       g [] []
                --     Just (Executing _ _) ->
                --       bottomMsg "Executing computation."
                --     Just (Dragging _) ->
                --       g [] []
                --     Nothing ->
                --       case graphView.package.undoBuffer of
                --         [] ->
                --           g [] []
                --         _ ->
                --           bottomMsg "Press «Enter» to apply these changes; press «Ctrl-Z» / «Ctrl-Y» to undo / redo"
                ]
          ]
          |> Html.Styled.fromUnstyled
      , if properties.canPan then
          case activePanDirection of
            Just ToTop ->
              mkArrow "top"           "M10 2 L2 18 L18 18 Z"
            Just ToBottom ->
              mkArrow "bottom"        "M10 18 L18 2 L2 2 Z"
            Just ToLeft ->
              mkArrow "left"          "M2 10 L18 2 L18 18 Z"
            Just ToTopRight ->
              mkArrow "top-right"     "M16 4 L4 4 L16 16 Z"
            Just ToBottomRight ->
              mkArrow "bottom-right"  "M4 16 L16 16 L16 4 Z"
            Just ToTopLeft ->
              mkArrow "top-left"      "M4 4 L16 4 L4 16 Z"
            Just ToRight ->
              mkArrow "right"         "M18 10 L2 2 L2 18 Z"
            Just ToBottomLeft ->
              mkArrow "bottom-left"   "M16 16 L4 16 L4 4 Z"
            Nothing ->
              Html.Styled.text ""
        else
          Html.Styled.text ""
      ]

selectSourceNode : Model -> Uuid -> Coordinate -> NodeId -> Model
selectSourceNode model view_uuid host_coord node_id =
  -- initially, you must click on a node to select it.
  -- therefore, we are initially always looking at a recursive
  -- connection to the same node!
  AutoDict.get view_uuid model.graph_views
  |> Maybe.andThen (.computation >> .graph >> Graph.get node_id)
  |> Maybe.map
    (\nodeContext ->
      let
        -- by default, the connection is recursive
        connection =
          IntDict.get node_id nodeContext.incoming
          |> Maybe.withDefault (AutoSet.empty transitionToString)
        linkDrawingData : LinkDrawingData
        linkDrawingData =
          { cardinality = Recursive
          , graphReferenceDescriptions = AutoDict.empty Uuid.toString
          , pathBetween =
              path_between
                nodeContext.node.label
                nodeContext.node.label
                Recursive
          , executionData = Nothing
          , connection = AutoSet.empty transitionToString
          }
      in
        C.pushInteractionForStack (Just view_uuid)
          ( ChoosingDestinationFor node_id
              ( ExistingNode node_id connection )
              host_coord
          )
          model
          -- and now modify the drawing-data for that view
        |> C.updateDrawingData view_uuid
            (\drawingData ->
              { drawingData
                | tentative_link = Just (node_id, node_id)
                , link_drawing =
                    Dict.insert (node_id, node_id) linkDrawingData drawingData.link_drawing
              }
            )
    )
  |> Maybe.withDefault model

switchFromExistingToPhantom : Uuid -> Bool -> NodeId -> GraphView -> Coordinate -> Coordinate -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromExistingToPhantom view_uuid old_conn_is_empty existing_id graph_view svg_coords host_coords sourceNodeContext model =
  let
    phantom_nodeid =
      Q.unusedNodeId graph_view.computation
    nodeData =
      { isTerminal = True
      , isDisconnected = False
      , coordinates = svg_coords
      , isRoot = False
      , canSplit = False
      , view_uuid = view_uuid
      , isSelected = False
      }
    linkData = -- this is the new calculated link-path
      { cardinality = Unidirectional
      , graphReferenceDescriptions = AutoDict.empty Uuid.toString
      , pathBetween = -- calculate the link path
          path_between sourceNodeContext.node.label svg_coords Unidirectional
      , executionData = Nothing
      , connection = AutoSet.empty transitionToString
      }
  in
    C.updateDrawingData view_uuid
      (\drawingData ->
        { drawingData
          | node_drawing = Dict.insert phantom_nodeid nodeData drawingData.node_drawing
          , link_drawing =
              ( if old_conn_is_empty then
                  Dict.remove (sourceNodeContext.node.id, existing_id) drawingData.link_drawing
                else
                  drawingData.link_drawing
              )
              -- and add the link path to `some_node`
              |> Dict.insert (sourceNodeContext.node.id, phantom_nodeid) linkData
          , tentative_link = Just (sourceNodeContext.node.id, phantom_nodeid)
        }
      )
      model
    -- we've made the changes, so set the interaction
    |> C.replaceInteraction (Just view_uuid)
        ( ChoosingDestinationFor sourceNodeContext.node.id
            ( NewNode phantom_nodeid svg_coords )
            host_coords
        )

phantomLinkDrawingForExisting : Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Model -> LinkDrawingData
phantomLinkDrawingForExisting sourceNodeContext existingNodeContext model =
  let
    cardinality =
      identifyCardinalityViaContext sourceNodeContext.node.id existingNodeContext
    connection =
      IntDict.get sourceNodeContext.node.id existingNodeContext.incoming
      |> Maybe.withDefaultLazy (\() -> AutoSet.empty transitionToString)
  in
    { cardinality = cardinality
    , graphReferenceDescriptions =
        Q.descriptionsForConnection connection model.packages
    , pathBetween =
        path_between -- calculate the link path
          sourceNodeContext.node.label
          existingNodeContext.node.label
          cardinality
    , executionData = Nothing
    , connection = connection
    }

switchFromPhantomToExisting : Uuid -> NodeId -> Coordinate -> Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromPhantomToExisting view_uuid phantom_id host_coords sourceNodeContext existingNodeContext model =
  let
    linkData = -- this is the new calculated link-path
      phantomLinkDrawingForExisting sourceNodeContext existingNodeContext model
  in
    C.updateDrawingData view_uuid
      (\drawingData ->
        { drawingData
          | node_drawing =
              Dict.remove phantom_id drawingData.node_drawing
          , link_drawing =
              -- get rid of the old phantom link
              Dict.remove (sourceNodeContext.node.id, phantom_id) drawingData.link_drawing
              -- and add the link path to `some_node`
              |> Dict.insert (sourceNodeContext.node.id, existingNodeContext.node.id) linkData
          , tentative_link = Just (sourceNodeContext.node.id, existingNodeContext.node.id)
        }
      )
      model
    -- we've made the changes, so set the interaction
    |> C.replaceInteraction (Just view_uuid)
        ( ChoosingDestinationFor sourceNodeContext.node.id
            ( ExistingNode existingNodeContext.node.id linkData.connection )
            host_coords
        )

switchFromExistingToExisting : Uuid -> Bool -> NodeId -> Coordinate -> Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromExistingToExisting view_uuid old_conn_is_empty old_existing host_coords sourceNodeContext nearbyNodeContext model =
  let
    linkData = -- this is the new calculated link-path
      phantomLinkDrawingForExisting sourceNodeContext nearbyNodeContext model
  in
    C.updateDrawingData view_uuid
      (\drawingData ->
        { drawingData
          | link_drawing =
              -- get rid of the old phantom link, if necessary
              ( if old_conn_is_empty then
                  Dict.remove (sourceNodeContext.node.id, old_existing) drawingData.link_drawing
                else
                  drawingData.link_drawing
              )
              -- and add the link path to `some_node`
              |> Dict.insert (sourceNodeContext.node.id, nearbyNodeContext.node.id) linkData
          , tentative_link = Just (sourceNodeContext.node.id, nearbyNodeContext.node.id)
        }
      )
      model
    -- we've made the changes, so set the interaction
    |> C.replaceInteraction (Just view_uuid)
        ( ChoosingDestinationFor sourceNodeContext.node.id
            ( ExistingNode nearbyNodeContext.node.id linkData.connection )
            host_coords
        )

updatePhantomMovement : Uuid -> NodeId -> Coordinate -> Coordinate -> Graph.NodeContext Entity Connection -> Model -> Model
updatePhantomMovement view_uuid phantom_id svg_coords host_coords sourceNodeContext model =
  C.updateDrawingData view_uuid
    (\drawingData ->
        { drawingData
          | node_drawing =
              Dict.update phantom_id
                (Maybe.map (\nodeData ->
                  { nodeData | coordinates = svg_coords }
                ))
                drawingData.node_drawing
          , link_drawing =
              Dict.update (sourceNodeContext.node.id, phantom_id)
                (Maybe.map (\linkData ->
                  { linkData
                    | pathBetween =
                        path_between
                          sourceNodeContext.node.label
                          svg_coords
                          Unidirectional
                  }
                ))
                drawingData.link_drawing
        }
    )
    model
  |> C.replaceInteraction (Just view_uuid)
    ( ChoosingDestinationFor sourceNodeContext.node.id (NewNode phantom_id svg_coords) host_coords )

movePhantomNodeInView : Uuid -> GraphView -> Coordinate -> Model -> Model
movePhantomNodeInView view_uuid graph_view svg_coord model =
  -- in the event, the `x_` and `y_` have already been translated
  -- into guest-viewport coordinates, accounting for pan information.
  let
    -- what is the destination of this link?
    -- Sounds like a simple question: it's the phantom node, of course.  But when the
    -- phantom node gets too close to a REAL node, then it should switch to that node;
    -- and when it is far enough away, then it should switch back.
    nearby_node_lockOnDistance : Float
    nearby_node_lockOnDistance = 36 -- min distance before arrow inverts…

    nearby_node_func : ((Graph.Node Entity -> Bool) -> List (Graph.Node Entity) -> b) -> Float -> Coordinate -> GraphView -> b
    nearby_node_func f distance mouse { computation } =
      -- a good distance value is nodeRadius + 9 = 7 + 9 = 16, for "locking on".
      let
        square_dist = distance * distance
      in
        f
          (\node ->
            let
              dx = node.label.x - mouse.x
              dy = node.label.y - mouse.y
            in
              -- Debug.log ("Checking (" ++ String.fromFloat node.label.x ++ ", " ++ String.fromFloat node.label.y ++ ") against (" ++ String.fromFloat mouse_x ++ ", " ++ String.fromFloat mouse_y ++ ")") () |> \_ ->
              dx * dx + dy * dy <= square_dist -- 7 + 9 = 16
          )
          (Graph.nodes computation.graph)

    nearby_node : Float -> Coordinate -> GraphView -> Maybe (Graph.Node Entity)
    nearby_node =
      nearby_node_func List.find

    -- nearby_nodes : Float -> (Float, Float) -> GraphView -> List (Graph.Node Entity)
    -- nearby_nodes =
    --   nearby_node_func List.filter

  in
    -- first, let's find this thing.
    case Q.peekInteraction (Just view_uuid) model.interactionsDict of
      Just (ChoosingDestinationFor source (NewNode phantom_id _) host_coords) ->
        -- okay, so there is a phantom node already there and active.
        -- in the easiest case, we just need to move its (x,y) coordinates, and be done.
        -- But if we are close enough to "lock on" to a nearby node, then we need to
        -- change this interaction to reflect that instead.  So, which case do we
        -- have?
        case nearby_node nearby_node_lockOnDistance svg_coord graph_view of
          Just nearbyNode ->
            -- ooh, we're close to a lock-on node. Okay. Let's get rid of the phantom
            -- node; then calculate the link path (might be straight or curved or recursive)
            -- based on the node; and then get rid of the old link path, and put in the
            -- new one.
            -- Lastly, we set the interaction to the correct value.            
            Maybe.map2
              (\sourceNodeContext nearbyNodeContext ->
                  switchFromPhantomToExisting view_uuid phantom_id host_coords sourceNodeContext nearbyNodeContext model
              )
              (Graph.get source graph_view.computation.graph)
              (Graph.get nearbyNode.id graph_view.computation.graph)
            |> Maybe.withDefault model
          Nothing ->
            -- great, there is no nearby node; just update the (x, y).
            Maybe.map
              (\sourceNodeContext ->
                updatePhantomMovement view_uuid phantom_id svg_coord host_coords sourceNodeContext model
              )
              (Graph.get source graph_view.computation.graph)
            |> Maybe.withDefault model
      Just (ChoosingDestinationFor source (ExistingNode existing_node conn) host_coords) ->
        -- here the situation is "reversed":
        -- If I find a neighbour, and it is the existing neighbour, then I need do
        -- nothing.
        -- If I find a neighbour, and it is NOT the existing neighbour, then I need to
        -- switch to it.
        -- If I don't find a neighbour, then I must switch to a phantom node.
        case nearby_node nearby_node_lockOnDistance svg_coord graph_view of
          Just nearbyNode ->
            if existing_node == nearbyNode.id then
              -- no change needed.
              model
            else
              -- switch to the new node.
              Maybe.map2
                (\sourceNodeContext nearbyNodeContext ->
                    switchFromExistingToExisting view_uuid (AutoSet.isEmpty conn) existing_node host_coords sourceNodeContext nearbyNodeContext model
                )
                (Graph.get source graph_view.computation.graph)
                (Graph.get nearbyNode.id graph_view.computation.graph)
              |> Maybe.withDefault model -- no changes made.
          Nothing ->
            -- there is no nearby node; move from the existing node to a phantom node. 
            Maybe.map
              (\sourceNodeContext ->
                switchFromExistingToPhantom view_uuid (AutoSet.isEmpty conn) existing_node graph_view svg_coord host_coords sourceNodeContext model)
              (Graph.get source graph_view.computation.graph)
            |> Maybe.withDefault model
      _ ->
        model

movePhantomNode : Uuid -> Coordinate -> Model -> Model
movePhantomNode view_uuid coord model =
  AutoDict.get view_uuid model.graph_views
  |> Maybe.map (\gv -> movePhantomNodeInView view_uuid gv coord model)
  |> Maybe.withDefault model

dragNode : Uuid -> Coordinate -> NodeId -> Model -> Model
dragNode view_uuid svg_coord nodeId model =
  C.updateGraphView view_uuid
    (\gv ->
      let
        nodeContext = Graph.get nodeId gv.computation.graph
        get_vertices getFan getDrawing =
          Maybe.map
            (\ctx ->
              IntDict.toList (getFan ctx)
              |> List.filterMap
                (\(k, conn) ->
                  Graph.get k gv.computation.graph
                  |> Maybe.map (getDrawing conn ctx)
                )
            )
            nodeContext
          |> Maybe.withDefault []
        vertices_in =
          get_vertices .incoming (\conn otherCtx fanCtx -> linkDrawingForEdge fanCtx otherCtx conn model.packages)
        vertices_out =
          get_vertices .outgoing (\conn otherCtx fanCtx -> linkDrawingForEdge otherCtx fanCtx conn model.packages)
      in
        C.mapGraph
          (Graph.update nodeId
            (Maybe.map (\ctx ->
              { ctx
                | node =
                    { id = nodeId
                    , label =
                        let e = ctx.node.label in
                        { e | x = svg_coord.x, y = svg_coord.y }
                    }
              }
            ))
          )
          gv
        |> C.mapDrawingData
          (\dd ->
            { dd
              | node_drawing =
                  Dict.update nodeId
                    (Maybe.map (\node ->
                      { node | coordinates = svg_coord }
                    ))
                    dd.node_drawing
              , link_drawing =
                  List.foldl
                    (\( (src, dest), data ) ->
                      Dict.insert (src, dest) data
                    )
                    dd.link_drawing
                    (vertices_in ++ vertices_out)
            }
          )
    )
    model

removePhantomLink : Uuid -> NodeId -> NodeId -> Model -> Model
removePhantomLink view_uuid source dest =
  C.updateDrawingData view_uuid
    (\drawingData ->
      { drawingData
        | link_drawing =
            Dict.remove (source, dest) drawingData.link_drawing
        , tentative_link = Nothing
      }
    )

createNewGraphNode : Uuid -> NodeId -> Coordinate -> Model -> Model
createNewGraphNode view_uuid node_id svg_coord =
    C.updateGraphView view_uuid
     (C.mapGraph
        (Graph.insert
            { node =
                { id = node_id
                , label =
                    entity node_id NoEffect
                    |> (\e -> { e | x = svg_coord.x, y = svg_coord.y })
                }
            , incoming = IntDict.empty
            , outgoing = IntDict.empty
            }
        )
     )

cancelNewNodeCreation : Uuid -> Model -> Model
cancelNewNodeCreation view_uuid model =
  let
    kill : NodeId -> NodeId -> Model -> Model
    kill source dest model_ =
      C.updateDrawingData view_uuid
        (\drawingData ->
          { drawingData
            | node_drawing =
                Dict.remove dest drawingData.node_drawing
            , link_drawing =
                Dict.remove (source, dest) drawingData.link_drawing
            , tentative_link = Nothing
          }
        )
        model_
  in
    case C.popInteraction (Just view_uuid) model of
      Just (ChoosingDestinationFor source (NewNode dest _) _, model_) ->
        kill source dest model_
      Just (EditingConnection {source, dest, targetKind} _, model_) ->
        if targetKind == PhantomNodeNewConnection then
          kill source dest model_
        else
          Debugging.println "🚨 ERROR WHMD(MWOEI" -- how am I in this function, if there's no new node??
          model
      _ ->
        Debugging.println "🚨 ERROR $DBMWMGYERCC" -- how am I in this function, if neither of these is cancelled??
        model
