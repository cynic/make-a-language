module GraphEditor exposing (..)
import Color
import Force
import Css
import Graph exposing (Graph, NodeContext, NodeId)
import Html.Styled.Events as HE
import Html.Styled exposing
  (Html, div)
-- import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Styled.Attributes as HA
import Json.Decode as D
import TypedSvg exposing
  (circle, g, svg, title, text_, marker, path, defs, tspan, rect)
import TypedSvg.Attributes exposing
  ( class, fill, viewBox, id, refX, refY, orient, d
  , transform, dy)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Attributes.InPx exposing
  ( cx, cy, r, x, y, height
  , markerWidth, markerHeight, width, rx , ry)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing
  (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..)
  , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..)
  , MeetOrSlice(..), Align(..), Scale(..)
  , percent
  )
import TypedSvg.Attributes.InPx as Px
import Set exposing (Set)
import AutoSet
import IntDict
import Dict
import List.Extra as List
import Automata.DFA as DFA exposing (fromAutomatonGraph, toAutomatonGraph)
import Automata.Data exposing (..)
import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Uuid
import Uuid exposing (Uuid)
import AutoDict
import Automata.Debugging as Debugging
import Automata.Debugging exposing (debugAutomatonGraph, debugAutomatonGraphXY)
import Automata.Debugging exposing (println)
import VirtualDom
import Random.Pcg.Extended as Random

  -- to add: Execute, Step, Stop
  -- also: when I make a change to the graph, set .execution to Nothing!

-- For zooming, I take the approach set out at https://www.petercollingridge.co.uk/tutorials/svg/interactive/pan-and-zoom/


{-| The buffer from the edges within which panning occurs -}
panBufferAmount : (Float, Float) -> Float
panBufferAmount (w, h) =
  let
    minBuffer = 25 -- minimum comfortable target: 20-40px (WCAG). Mouse precision @ 15-25px.
    -- Fitts' Law says edges are infinite, but there's no guarantee that the edge of the
    -- window will be a *real* edge.
    maxBuffer = 60
    bufferRatio = 0.025 -- 2.5%.  Perhaps ≈3% would be even better; let's see.
    -- but also, a more consistent buffer-size might be simpler and better for users to
    -- build muscle-memory.  So let's see.
  in
    clamp
      minBuffer
      maxBuffer
      (bufferRatio * min w h)

makeLinkForces : Graph Entity Connection -> Force.Force NodeId
makeLinkForces graph =
  Graph.fold
    (\ctx acc ->
      -- for all outgoing links that AREN'T recursive, create a Force.
      let
        outs = ctx.outgoing |> IntDict.filter (\k _ -> k /= ctx.node.id)
      in
        IntDict.toList outs
        |> List.foldl
          (\(k, v) forces_ ->
            { source = ctx.node.id
            , target = k
            , distance = 80 -- 35-40 seems like a good distance
            , strength = Just 1.0 -- * (toFloat <| Set.size v)
            } :: forces_
          )
          acc
    )
    []
    graph
  |> Force.customLinks 3

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
      (\ctx {toX, toY, link, manyBody} ->
        let
        -- for all outgoing links that AREN'T recursive, create a link-Force.
          (seed1, linkForce) =
            IntDict.toList ctx.outgoing
            |> List.filter (Tuple.first >> (/=) ctx.node.id)
            |> List.foldl
                (\(k, conn) (seed, forces_) ->
                  let
                    (n, seed_) = Random.step (Random.int 1 500) seed
                  in
                  ( seed_
                  , { source = ctx.node.id
                    , target = k
                    , distance = 50 + toFloat n + linkLabelWidth conn -- 35-40 seems like a good distance
                    , strength = Just 1.0 -- * (toFloat <| Set.size v)
                    } :: forces_
                  )
                )
                (seed0, link)
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
            , Force.manyBodyStrength -5000.0 manyBody
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
    , graphIdentifier = g.graphIdentifier
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
      toForceGraph (g {- |> Automata.Debugging.debugAutomatonGraph "Graph as received" -})
      -- |> Automata.Debugging.debugAutomatonGraph "After toForceGraph"
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
      -- |> Automata.Debugging.debugAutomatonGraph "After sim"
  in
    -- no changes to node-ids are made; only the spatial positions change.
    { solvedGraph = resultingGraph
    , simulation = simulation
    , forces = forces_
    }
    -- |> Automata.Debugging.debugAutomatonGraph "After sim"

-- updateNode : ( Float, Float ) -> ( Float, Float ) -> NodeContext Entity Connection -> NodeContext Entity Connection
-- updateNode (offsetX, offsetY) (x, y) nodeCtx =
--   updateContextWithValue
--     nodeCtx
--     ( setXY (x + offsetX) (y + offsetY) nodeCtx.node.label )

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

identifyDisconnectedNodes : AutomatonGraph -> Set NodeId
identifyDisconnectedNodes g =
  Graph.mapContexts
    (\ctx ->
      { ctx
        | incoming = IntDict.filter (\_ -> not << AutoSet.isEmpty) ctx.incoming
        , outgoing = IntDict.filter (\_ -> not << AutoSet.isEmpty) ctx.outgoing
      }
    )
    g.graph
  |> Graph.guidedDfs
    Graph.alongOutgoingEdges
    (\_ acc -> (acc, identity))
    [g.root]
    ()
  |> Tuple.second
  |> Graph.nodeIds
  |> Set.fromList


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

-- paletteColors : { state : { normal : Color.Color, start : Color.Color, terminal : Color.Color }, edge : Color.Color, transition : { nonFinal : Color.Color, final : Color.Color }, background : Color.Color }
-- paletteColors =
--   { state =
--     { normal = Color.rgb255 0 222 255
--     , start = Color.rgb255 0 35 135
--     , terminal = Color.rgb255 0 123 167
--     }
--   , edge = Color.hsl 1 0 0.35
--   , transition =
--       { nonFinal = Color.hsl 1 0 0.25
--       -- orange is from ≈31°-39° (←red, orange, yellow→).
--       , final = Color.darkOrange
--       }
--   , background = Color.grey -- for painting a surrounding stroke
--   }


-- type LinkType
--   = Confirmed Cardinality -- in the confirmed graph.
--   | Prospective Cardinality -- user is playing around, hasn't clicked yet.
--   | New Cardinality -- user has clicked, but entirety isn't approved yet.
--                     -- When it is approved, we'll see it under Confirmed.

path_between : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Cardinality -> PathBetweenReturn
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

{-| (2+7n)×(2+4n)-pixel UUID-representing badges.
    
    Sensible sizes are:
    - 2 = 16x10px badge
    - 4 = 30x18px badge
-}
viewGraphReference : Uuid.Uuid -> Int -> Float -> Float -> Svg a
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
                    [ AutoDict.get uuid drawing_data.graphReferenceDescriptions
                      |> Maybe.withDefault "(no description)"
                      |> text
                    ]
                ]
            ) :: elems
          )
    )
    ( x_ - 0.5 * est_width, y_, [] )
    drawing_data.connection
  |> (\(_, _, elems) -> g [] elems)

viewLink : GraphView -> (NodeId, NodeId) -> LinkDrawingData -> Svg Msg
viewLink {id, properties} (from, to) drawing_data =
      let
        x_ = drawing_data.pathBetween.transition_coordinates.x
        y_ = drawing_data.pathBetween.transition_coordinates.y
        transitionLabel : Svg Msg
        transitionLabel =
          case drawing_data.executionData of
            Just {chosen} ->
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
          case ( drawing_data.executionData, drawing_data.highlighting ) of
            ( Nothing, Just Phantom ) ->
              [ "link", "phantom" ]
            ( Nothing, Just Highlight ) ->
              [ "link", "highlight" ]
            ( Nothing, Nothing ) ->
              [ "link" ]
            ( Just {smallest_recency}, Just Phantom ) ->
              [ "link", "executed", "phantom", "recent-" ++ String.fromInt smallest_recency ]
            ( Just {smallest_recency}, Just Highlight ) ->
              [ "link", "executed", "highlight", "recent-" ++ String.fromInt smallest_recency ]
            ( Just {smallest_recency}, Nothing ) ->
              [ "link", "executed", "recent-" ++ String.fromInt smallest_recency ]
      in
        g
          [ class [ "link-group" ] ]
          [
            path
              [ d drawing_data.pathBetween.pathString
              , class ( "background" :: linkClass )
              ]
              [ {- title [] [ text <| Automata.Data.connectionToString edge.label ] -} ]
          , path
              [ d drawing_data.pathBetween.pathString
              , class ( "foreground" :: linkClass )
              ]
              [ {- title [] [ text <| Automata.Data.connectionToString edge.label ] -} ]
          , g
              [ class linkClass
              , properties.canSelectConnections
                |> thenPermitSvgInteraction
                    (onClick (EditConnection (x_, y_) id from to drawing_data.connection))
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

nodeRadius : Float
nodeRadius = 7

viewNode : GraphViewProperties -> NodeId -> NodeDrawingData -> Svg Msg
viewNode properties id data =
  let
    nodeClass =
      conditionalList
        [ ("graph-node", True)
        , ("selected", data.exclusiveAttributes == Just DrawSelected)
        , ("current", data.exclusiveAttributes == Just DrawCurrentExecutionNode)
        , ("phantom", data.exclusiveAttributes == Just DrawPhantom)
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
      ++ "\n#" ++ String.fromInt id ++ "; " ++ String.fromFloat (fewDP node_x) ++ ", " ++ String.fromFloat (fewDP node_y) -- DEBUGGING          
    ( node_x, node_y ) =
      data.coordinates
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
                    D.succeed (GraphViewMsg data.view_uuid <| SelectNode id)
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
                    D.succeed (GraphViewMsg data.view_uuid <| StartDraggingNode id)
                  else
                    D.fail "Unwanted event"
                )
              )
            )
        else
          TypedSvg.Events.on "dummy" (VirtualDom.Normal <| D.fail "dummy event")
      ]
      [ circle
          [ cx node_x
          , cy node_y
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
viewUndoRedoVisualisation { package, guest_coordinates, guest_dimensions } =
  let
    x_ = Tuple.first guest_coordinates
    h = Tuple.second guest_coordinates + Tuple.second guest_dimensions
    rect_width = 30
    rect_height = 10
    rect_spacing = 3
    num_undo = List.length package.undoBuffer
    idxToY idx =
      h - (toFloat (15 + idx * (rect_height + 1) + idx * (rect_spacing - 1)))
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
                , x (x_ + 5)
                , y <| idxToY idx
                , class ["undo", if num_undo - 1 == idx then "current" else ""]
                ]
                []
            )
            package.undoBuffer
        )
        ++
        ( List.indexedMap
            (\idx _ ->
              rect
                [ TypedSvg.Attributes.InPx.width rect_width
                , TypedSvg.Attributes.InPx.height rect_height
                , ry 2
                , x (x_ + 5)
                , y <| idxToY (idx + num_undo)
                , class ["redo"]
                ]
                []
            )
            package.redoBuffer
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
      -- this part is for debugging panning. If I uncomment it, I should also
      -- uncomment the corresponding code in viewGraph
    -- , rect
    --     [ stroke <| Paint Color.lightRed
    --     , fill <| PaintNone
    --     , Px.strokeWidth 2
    --     , Px.x guest_x
    --     , Px.y guest_y
    --     , Px.width guest_width
    --     , Px.height guest_height
    --     ]
    --     []
    , Dict.toList graph_view.drawingData.link_drawing
      -- draw any phantom link last, because it should be displayed on top of everything else.
      |> List.sortBy (\(_, data) -> if Maybe.isJust data.highlighting then 1 else 0)
      |> List.map (\((from, to), data) -> viewLink graph_view (from, to) data)
      |> g [ class [ "edges" ] ]
    , Dict.toList graph_view.drawingData.node_drawing
      |> List.map (\(nodeId, data) -> viewNode graph_view.properties nodeId data)
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
viewGraph graphView =
  let
    ( host_x, host_y ) = graphView.host_coordinates
    ( host_width, host_height ) = graphView.host_dimensions
    ( guest_width, guest_height) = graphView.guest_dimensions
    ( guest_x, guest_y ) = graphView.guest_coordinates
    ( pan_dx, pan_dy ) = graphView.pan
    ( guest_inner_x, guest_inner_y ) = graphView.guest_inner_coordinates
    ( guest_inner_width, guest_inner_height ) = graphView.guest_inner_dimensions
    draw_right_buffer =
      guest_x - pan_dx + guest_width > guest_inner_x + guest_inner_width
    draw_left_buffer =
      guest_x - pan_dx < guest_inner_x
    draw_bottom_buffer =
      guest_y - pan_dy + guest_height > guest_inner_y + guest_inner_height
    draw_top_buffer =
      guest_y - pan_dy < guest_inner_y
    rectangles =
      if graphView.properties.canPan then
        conditionalList
          [ ( Rectangle host_x host_y host_width graphView.panBuffer, draw_top_buffer ) -- top
          , ( Rectangle host_x host_y graphView.panBuffer host_height, draw_left_buffer ) -- left
          , ( Rectangle host_x (host_y + host_height - graphView.panBuffer) host_width graphView.panBuffer, draw_bottom_buffer ) -- bottom
          , ( Rectangle (host_x + host_width - graphView.panBuffer) host_y graphView.panBuffer host_height, draw_right_buffer ) -- right
          ]
      else
        []
    panRadius = sqrt (2.0 * graphView.panBuffer * graphView.panBuffer)
    mkArrow : String -> Float -> Float -> String -> Html Msg
    mkArrow direction width height pathString =
      if graphView.properties.canPan then
        div
          [ HA.class <| "pan-region " ++ direction
          , HA.css
            [ Css.width (Css.px width)
            , Css.height (Css.px height)
            ]
          , graphView.properties.canPan
            |> thenPermitInteraction (HE.onMouseOver (GraphViewMsg graphView.id (ConsiderPan rectangles)))
          , graphView.properties.canPan
            |> thenPermitInteraction (HE.onMouseOut (GraphViewMsg graphView.id StopPan))
          ]
          [ svg
              [ class [ "pan-arrow" ] 
              , viewBox 0 0 20 20
              ]
              [ path [ d pathString ] [] ]
            |> Html.Styled.fromUnstyled
          ]
      else
        Html.Styled.text ""
  in
    div
      [ HA.class "graph-container"
      , HA.id <| Uuid.toString graphView.id
      , HA.css
          [ Css.width (Css.px host_width)
          , Css.height (Css.px host_height)
          ]
      , HE.onMouseOver (GraphViewMsg graphView.id RequestCoordinates)
      ]
      [ svg
          ([ viewBox guest_x guest_y guest_width guest_height
          , TypedSvg.Attributes.InPx.width host_width
          , TypedSvg.Attributes.InPx.height host_height
          , class
              ( conditionalList
                  [ ("graph", True)
                  , ("can-select-nodes", graphView.properties.canSelectNodes)
                  , ("can-select-connections", graphView.properties.canSelectConnections)
                  , ("can-split-nodes", graphView.properties.canSplitNodes)
                  , ("can-select-space", graphView.properties.canSelectEmptySpace)
                  ]
              )
          , graphView.properties.canSelectEmptySpace
            |> thenPermitSvgInteraction (onClick (GraphViewMsg graphView.id SelectSpace))
          ] {- ++ interactivity -})
          [ -- this stuff is in the background.
            viewUndoRedoVisualisation graphView
            -- this part is for debugging panning. If I uncomment it, I should also
            -- uncomment the corresponding code in viewMainSvgContent.
          -- , rect
          --     [ fill <| Paint Color.lightYellow
          --     , Px.x guest_inner_x
          --     , Px.y guest_inner_y
          --     , Px.width guest_inner_width
          --     , Px.height guest_inner_height
          --     ]
          --     []
          , viewMainSvgContent graphView -- this is the "main" interactive frame, which will be zoomed, panned, etc.
          , if graphView.isFrozen then
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
                    [ x <| (guest_x + guest_width) - 5
                    , y <| (guest_y + guest_height) - 10
                    , class [ "status-line", "pan" ]
                    ]
                    [ tspan
                        ( if graphView.pan == (0, 0) then
                            []
                          else
                            [ class [ "pan-reset" ]
                            , graphView.properties.canPan
                              |> thenPermitSvgInteraction (onClick (GraphViewMsg graphView.id ResetPan))
                            ]
                        )
                        [ text "🧭" ]
                    , tspan
                        []
                        [ text <| " " ++ panToString graphView.pan
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
      , div
          []
          ( if graphView.properties.canPan then
              conditionalList
                [ ( mkArrow "top" host_width graphView.panBuffer     "M10 2 L2 18 L18 18 Z", draw_top_buffer )
                , ( mkArrow "bottom" host_width graphView.panBuffer  "M10 18 L18 2 L2 2 Z", draw_bottom_buffer )
                , ( mkArrow "left" graphView.panBuffer host_height   "M2 10 L18 2 L18 18 Z", draw_left_buffer )
                , ( mkArrow "top-right" panRadius panRadius          "M16 4 L4 4 L16 16 Z", draw_top_buffer && draw_right_buffer )
                , ( mkArrow "bottom-right" panRadius panRadius       "M4 16 L16 16 L16 4 Z", draw_bottom_buffer && draw_right_buffer )
                , ( mkArrow "top-left" panRadius panRadius           "M4 4 L16 4 L4 16 Z", draw_top_buffer && draw_left_buffer )
                , ( mkArrow "right" graphView.panBuffer host_height  "M18 10 L2 2 L2 18 Z", draw_right_buffer )
                , ( mkArrow "bottom-left" panRadius panRadius        "M16 16 L4 16 L4 4 Z", draw_bottom_buffer && draw_left_buffer )
                ]
            else
              []
          )
      ]