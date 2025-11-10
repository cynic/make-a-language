module GraphEditor exposing (..)
import Browser.Events
import Color
import Force
import Graph exposing (Edge, Graph, NodeContext, NodeId)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Json.Decode as D
import TypedSvg exposing
  (circle, g, svg, title, text_, marker, path, defs, tspan, rect)
import TypedSvg.Attributes exposing
  ( class, fill, stroke, viewBox, fontFamily, fontWeight, alignmentBaseline
  , textAnchor, cursor, id, refX, refY, orient, d, markerEnd, dominantBaseline
  , transform, noFill, strokeDasharray, strokeLinecap
  , markerStart, pointerEvents, dy)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Attributes.InPx exposing
  ( cx, cy, r, strokeWidth, x, y, height, fontSize
  , markerWidth, markerHeight, width, rx , ry)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing
  (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..)
  , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..)
  , MeetOrSlice(..), Align(..), Scale(..)
  )
import TypedSvg.Attributes.InPx as Px
import Html
import Html.Attributes
import Set exposing (Set)
import AutoSet
import IntDict
import Time
import Dict
import List.Extra as List
import Automata.DFA as DFA exposing (fromAutomatonGraph, toAutomatonGraph)
import Automata.Data exposing (..)
import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Automata.Debugging
import Uuid
import Svg
import Uuid exposing (Uuid)
import AutoDict
import Random.Pcg.Extended as Random
import Automata.Debugging as Debugging
import Automata.Debugging exposing (debugAutomatonGraph, debugAutomatonGraphXY)
import Automata.Debugging exposing (println)
import Basics.Extra exposing (minSafeInteger)
import Basics.Extra exposing (maxSafeInteger)

type alias Model = GraphView
type alias Msg = Main_Msg

  -- to add: Execute, Step, Stop
  -- also: when I make a change to the graph, set .execution to Nothing!

-- For zooming, I take the approach set out at https://www.petercollingridge.co.uk/tutorials/svg/interactive/pan-and-zoom/


{-| The buffer from the edges within which panning occurs -}
panBufferAmount : (Float, Float) -> Float
panBufferAmount (w, h) =
  let
    minBuffer = 15 -- minimum comfortable target: 20-40px (WCAG). Mouse precision @ 15-25px.
    -- Fitts' Law says edges are infinite, but there's no guarantee that the edge of the
    -- window will be a *real* edge.
    maxBuffer = 40
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

coordinateForces : NodeId -> Graph Entity Connection -> List (Force.Force NodeId)
coordinateForces root graph =
  let
    x_target : Float
    x_target = toFloat <| (Graph.size graph) * (80 + 18)
  in
    -- links are the "springs"
    -- manyBody is the "repulsion"
    Graph.fold
      (\ctx {toX, toY, link, manyBody} ->
        let
        -- for all outgoing links that AREN'T recursive, create a link-Force.
          linkForce =
            IntDict.keys ctx.outgoing
            |> List.filter ((/=) ctx.node.id)
            |> List.foldl
                (\k forces_ ->
                  { source = ctx.node.id
                  , target = k
                  , distance = 80 -- 35-40 seems like a good distance
                  , strength = Just 1.0 -- * (toFloat <| Set.size v)
                  } :: forces_
                )
                link
        in
          if ctx.node.id == root then
            { toX =
                { node = ctx.node.id , strength = 20.0 , target = 0 } :: toX
            , toY =
                { node = ctx.node.id , strength = 20.0 , target = 0 } :: toY
            , link = linkForce
            , manyBody = ctx.node.id :: manyBody
            }
          else
            { toX =
                { node = ctx.node.id , strength = 0.03 , target = x_target } :: toX
            , toY =
                { node = ctx.node.id , strength = 0.03 , target = 0 } :: toY
            , link = linkForce
            , manyBody = ctx.node.id :: manyBody
            }
      )
      { toX = [], toY = [], link = [], manyBody = [] }
      graph
    |>  (\{toX, toY, link, manyBody} ->
          [ Force.towardsX toX
          , Force.towardsY toY
          , Force.customLinks 3 link
          , Force.manyBodyStrength -20.0 manyBody
          ]
        )

forces : AutomatonGraph -> (Int, Int) -> List (Force.Force NodeId)
forces g (width, height) =
  coordinateForces g.root g.graph

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

computeGraphFully : (Float, Float) -> AutomatonGraph -> ComputeGraphResult
computeGraphFully (w, h) g =
  let
    forceGraph =
      toForceGraph (g {- |> Automata.Debugging.debugAutomatonGraph "Graph as received" -})
      -- |> Automata.Debugging.debugAutomatonGraph "After toForceGraph"
    forces_ = forces forceGraph (round w, round h)
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

-- {-| What is the amount to pan by, given the x-coordinate of the mouse? -}
-- xPanAt : Float -> Float -> Float -> Float
-- xPanAt width panBuffer x =
--   if x >= ( width - panBuffer ) then
--     1
--   else if x <= panBuffer then
--     -1
--   else
--     0

-- yPanAt : Float -> Float -> Float -> Float
-- yPanAt height panBuffer y =
--   if y >= ( height - panBuffer ) then
--     1
--   else if y <= panBuffer then
--     -1
--   else
--     0

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


-- newnode_graphchange : NodeId -> Float -> Float -> Connection -> AutomatonGraph -> AutomatonGraph
-- newnode_graphchange src x y conn g =
--   { g
--     | graph =
--         let
--           id = maxId g + 1

--           initial = entity id NoEffect
--         in
--           Graph.insert
--             { node =
--               { label = initial |> setXY x y
--               , id = id
--               }
--             , incoming = IntDict.singleton src conn
--             , outgoing = IntDict.empty
--             }
--             g.graph
--   }
--   -- |> debugAutomatonGraph "[create_union_graphchange] updated graph"

-- updateLink_graphchange : NodeId -> NodeId -> Connection -> AutomatonGraph -> AutomatonGraph
-- updateLink_graphchange src dest conn g =
--   { g
--     | graph =
--         Graph.update dest
--           (Maybe.map (\node ->
--             { node | incoming = IntDict.insert src conn node.incoming }
--           ))
--           g.graph
--       -- if it's recursive, I must add it to both so that it reflects in the graph.
--       -- in other cases, this should do no harm.
--       |> Graph.update src
--         (Maybe.map (\node ->
--           { node | outgoing = IntDict.insert dest conn node.outgoing }
--         ))
--   }
--   -- |> debugAutomatonGraph "[create_update_graphchange] updated graph"

-- removeLink_graphchange : NodeId -> NodeId -> AutomatonGraph -> AutomatonGraph
-- removeLink_graphchange src dest g =
--   { g
--     | graph =
--         Graph.update dest
--           (Maybe.map
--             (\node ->
--               { node
--                 | incoming =
--                     IntDict.update src
--                       (Maybe.map (\_ -> AutoSet.empty transitionToString))
--                       node.incoming
--                 , outgoing =
--                     if src == dest then
--                       IntDict.update dest
--                         (Maybe.map (\_ -> AutoSet.empty transitionToString))
--                         node.outgoing
--                     else
--                       node.outgoing
--               })
--           )
--           g.graph
--   }
--   -- |> debugAutomatonGraph "[create_removal_graphchange] updated graph"

-- splitNode : NodeContext Entity Connection -> Connection -> Connection -> Model -> AutomatonGraph
-- splitNode node left _ model = -- turns out that "right" isn't needed. Hmmm!!!?
--   let
--     (recursive, nonRecursive) =
--       node.incoming
--       |> IntDict.partition (\k _ -> k == node.node.id)
--     recursive_connection =
--       recursive
--       |> IntDict.values
--       |> List.foldl AutoSet.union (AutoSet.empty transitionToString)
--     ( leftConnections, rightConnections ) =
--       nonRecursive
--       |> IntDict.foldl
--           (\k conn (l, r) ->
--             -- classify each transition into 'left' or 'right'.
--             AutoSet.foldl
--               (\transition (l_, r_) ->
--                 if AutoSet.member transition left then
--                   ( IntDict.update k
--                       ( Maybe.map (AutoSet.insert transition)
--                         >> Maybe.orElseLazy (\() -> Just <| AutoSet.singleton transitionToString transition)
--                       )
--                       l_
--                   , r_
--                   )
--                 else
--                   ( l_
--                   , IntDict.update k
--                     ( Maybe.map (AutoSet.insert transition)
--                       >> Maybe.orElseLazy (\() -> Just <| AutoSet.singleton transitionToString transition)
--                     )
--                     r_
--                   )
--               )
--               (l, r)
--               conn
--           )
--           (IntDict.empty, IntDict.empty)
--     ag = model.package.userGraph
--     id = maxId ag + 1
--     newUserGraph =
--       { ag
--         | graph =
--             ag.graph
--             |> Graph.insert
--               { node =
--                   { label = entity id NoEffect
--                   , id = id
--                   }
--               , incoming =
--                   leftConnections
--                   |> IntDict.insert id recursive_connection
--               , outgoing =
--                   node.outgoing
--                   |> IntDict.insert id recursive_connection
--               }
--             |> Graph.update node.node.id
--               (\_ ->
--                 Just <|
--                   { node
--                     | incoming =
--                         rightConnections
--                         |> IntDict.insert node.node.id recursive_connection
--                   }
--               )
--       }
--   in
--     newUserGraph

textChar : Char -> String
textChar ch =
  case ch of
    ' ' ->
      "└┘"
    _ ->
      String.fromChar ch

transitionToTextSpan : Transition -> (AcceptVia -> List String) -> Svg msg
transitionToTextSpan transition otherClasses =
  case transition.via of
    ViaCharacter ch ->
      tspan
        [ class <|
            (if .isFinal transition then "final" else "nonfinal")
            :: otherClasses transition.via
        ]
        [ text <| textChar ch ]
    ViaGraphReference ref ->
      tspan
        [ class <|
            (if .isFinal transition then "final" else "nonfinal")
            :: otherClasses transition.via
        ]
        [ text "Ω"
        , title [] [ text <| Uuid.toString ref ]
        ]

connectionToSvgText : Connection -> List (Svg msg)
connectionToSvgText =
  AutoSet.toList
  >> List.map (\t -> transitionToTextSpan t (\_ -> []))

connectionToSvgTextHighlightingChars : Connection -> (AcceptVia -> List String) -> List (Svg msg)
connectionToSvgTextHighlightingChars conn highlightFunction =
  AutoSet.toList conn
  |> List.map (\t -> transitionToTextSpan t highlightFunction)

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

path_between : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Cardinality -> Float -> Float -> PathBetweenReturn
path_between sourceXY_orig destXY_orig cardinality radius_from radius_to =
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

executionData : ExecutionResult -> Maybe ExecutionData
executionData r =
  let
    getData s =
      case s of
        Accepted d -> d
        Rejected d -> d
        RequestedNodeDoesNotExist d -> d
        NoPossibleTransition d -> d
  in
  case r of
    InternalError -> Nothing
    EndOfInput s -> Just <| getData s
    EndOfComputation s -> Just <| getData s
    CanContinue s -> Just <| getData s

-- executionResultAutomatonGraph : ExecutionResult -> Maybe AutomatonGraph
-- executionResultAutomatonGraph executionResult =
--   executionData executionResult
--   |> Maybe.map .computation

-- -- from the execution result, obtain the edges which were taken.
-- -- Note that multiple edges may be "taken" between two nodes,
-- -- in the cases of (1) loops and (2) graph-ref intersections.
-- executing_edges : ExecutionResult -> Dict (NodeId, NodeId) (Int, AcceptVia)
-- executing_edges result =
--   let
--     trace : TransitionTakenData -> (NodeId, Int, Dict (NodeId, NodeId) (Int, AcceptVia)) -> (NodeId, Int, Dict (NodeId, NodeId) (Int, AcceptVia))
--     trace {dest, matching} (lastNode, recency, acc) =
--       ( dest
--       , recency - 1
--       , Dict.insert (lastNode, dest) (recency, matching.via) acc
--       )
--   in
--     executionData result
--     |> Maybe.map
--       (\data ->
--           List.foldl
--             trace
--             (data.computation.root, List.length data.transitions, Dict.empty)
--             data.transitions
--           |> (\(_, _, dict) -> dict)
--       )
--     |> Maybe.withDefault Dict.empty

-- viewGraphReference : Uuid.Uuid -> Float -> Float -> Svg a
-- viewGraphReference uuid x_ y_ =
--   let
--     pixels = getPalette uuid
--     pixelSize = 4
--   in
--     g
--       []
--       ( rect
--           [ x <| x_
--           , y <| y_
--           , width <| (7 * pixelSize) + 2
--           , height <| (4 * pixelSize) + 2
--           , rx 2
--           , ry 2
--           , fill <| Paint <| Color.black
--           ]
--           []
--       -- each one is a 4px square
--       :: Tuple.second
--           (List.foldl
--             (\color (i, acc) ->
--               ( i + 1
--               , rect
--                   [ x <| x_ + 1 + pixelSize * (modBy 7 i |> toFloat)
--                   , y <| y_ + 1 + pixelSize * (modBy 4 i |> toFloat)
--                   , width pixelSize
--                   , height pixelSize
--                   , fill <| Paint color
--                   -- , stroke <| Paint <| Color.black
--                   -- , strokeWidth 1
--                   , rx (pixelSize * 0.15)
--                   , ry (pixelSize * 0.15)
--                   ]
--                   []
--                 :: acc
--               )
--             )
--             (0, [])
--             pixels
--           )
--       )

viewLink : (NodeId, NodeId) -> LinkDrawingData -> Svg Msg
viewLink (from, to) drawing_data =
      let
        labelText =
          if drawing_data.executionData.executed then
            connectionToSvgTextHighlightingChars
              drawing_data.label
              (\via ->
                AutoDict.get via drawing_data.executionData.chosen
                |> Maybe.map (\recency ->
                  [ "executed", "recent-" ++ String.fromInt recency ]
                )
                |> Maybe.withDefault []
              )
          else
            connectionToSvgText drawing_data.label
        linkClass =
          if drawing_data.executionData.executed then
            [ "link", "executed", "recent-" ++ String.fromInt drawing_data.executionData.smallest_recency ]
          else if drawing_data.isPhantom then
            [ "link", "phantom" ]
          else
            [ "link" ]
      in
        g
          [ class [ "link-group" ] ]
          [
            path
              [ d drawing_data.pathBetween.pathString
              -- , noFill
              , class ( "background" :: linkClass )
              ]
              [ {- title [] [ text <| Automata.Data.connectionToString edge.label ] -} ]
          , path
              [ d drawing_data.pathBetween.pathString
              , class linkClass
              ]
              [ {- title [] [ text <| Automata.Data.connectionToString edge.label ] -} ]
          , text_
              [ x <| drawing_data.pathBetween.transition_coordinates.x
              , y <| drawing_data.pathBetween.transition_coordinates.y
              -- , Html.Attributes.attribute "paint-order" "stroke fill markers"
              , class ( "text" :: linkClass )
              -- , onClick (EditTransition edge.from edge.to edge.label)
              ]
              ( title [] [ text "Click to modify" ] :: labelText 
              )
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
    -- permit_node_reselection =
    --   Mouse.onWithOptions
    --     "mousedown"
    --     { stopPropagation = True, preventDefault = True }
    --     (\e ->
    --       if e.keys.shift then
    --         NodeDragStart id
    --       else if e.keys.ctrl && splittable then
    --         StartSplit id
    --       else
    --         case interactionsDict of
    --           Just (ModifyingGraph _ _) ->
    --             -- ANY node is fine!  If it's the same node, that's also fine.  Recursive links are okay.
    --             CreateOrUpdateLinkTo id
    --           _ ->
    --             SelectNode id
    --     )

    -- interactivity =
    --   case interactionsDict of
    --     Just (ModifyingGraph _ { dest }) ->
    --       case dest of
    --         NoDestination ->
    --           [ permit_node_reselection ]
    --         _ ->
    --           []
    --     _ ->
    --       if Set.member id disconnectedNodes then
    --         []
    --       else
    --         [ permit_node_reselection ]
  let
    nodeClass =
      classList
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
        "")
      ++ "Shift-drag to reposition" ++
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
      -- ::interactivity
      , properties.canSelectNodes
        |> thenPermitInteraction (onClick <| SelectNode data.view_uuid id (node_x, node_y))
      -- , properties.canSplitNodes
      --   |> thenPermitInteraction (onClick <| StartSplit id)
      ]
      [ circle
          [ --r nodeRadius
          --, strokeWidth 2
            cx node_x
          , cy node_y
          , class nodeClass
          ]
          []
      -- ,  if data.isTerminal && data.isRoot then
      --       text_
      --         [ x <| node_x
      --         , y <| (node_y + 1)
      --         , fontFamily ["sans-serif"]
      --         , fontSize 14
      --         , fontWeight FontWeightNormal
      --         , textAnchor AnchorMiddle
      --         , alignmentBaseline AlignmentBaseline
      --         , dominantBaseline DominantBaselineMiddle
      --         , Html.Attributes.attribute "paint-order" "stroke fill markers"
      --         ]
      --         [ text "💥"
      --         , title [] [text titleText]
      --         ]
      --     else if data.isTerminal then
      --       text_
      --         [ x <| node_x
      --         , y <| (node_y + 1)
      --         , fontFamily ["sans-serif"]
      --         , fontSize 14
      --         , fontWeight FontWeightNormal
      --         , textAnchor AnchorMiddle
      --         , alignmentBaseline AlignmentBaseline
      --         , dominantBaseline DominantBaselineMiddle
      --         , Html.Attributes.attribute "paint-order" "stroke fill markers"
      --         ]
      --         [ text "🎯"
      --         , title [] [text titleText]
      --         ]
      --     else if data.isRoot then
      --       text_
      --         [ x <| node_x
      --         , y <| (node_y + 1)
      --         , fontFamily ["sans-serif"]
      --         , fontSize 12
      --         , fontWeight FontWeightNormal
      --         , textAnchor AnchorMiddle
      --         , alignmentBaseline AlignmentBaseline
      --         , dominantBaseline DominantBaselineMiddle
      --         , Html.Attributes.attribute "paint-order" "stroke fill markers"
      --         , fill <| Paint <| Color.grey
      --         ]
      --         [ text "⭐"
      --         , title [] [text titleText]
      --         ]
      --     else
      --       g [] []
      , title [] [text titleText]
      ]

-- nearby_node_lockOnDistance : Float
-- nearby_node_lockOnDistance = nodeRadius + 9

-- nearby_node_repulsionDistance : Float
-- nearby_node_repulsionDistance =
--   nodeRadius * 12

-- {-| Used by nearby_node and nearby_nodes. Not for other use. -}
-- nearby_node_func : ((Graph.Node Entity -> Bool) -> List (Graph.Node Entity) -> b) -> Float -> (Float, Float) -> Model -> b
-- nearby_node_func f distance mouseCoords { package, pan } =
--   -- a good distance value is nodeRadius + 9 = 7 + 9 = 16, for "locking on".
--   let
--     userGraph = package.userGraph
--     ( xPan, yPan ) = pan
--     ( mouse_x, mouse_y ) =
--       mouseCoords -- |> Debug.log "Mousecoords"
--     adjustment_x = xPan + mouse_x
--     adjustment_y = yPan + mouse_y
--     square_dist = distance * distance
--   in
--     f
--       (\node ->
--         let
--           dx = node.label.x - adjustment_x
--           dy = node.label.y - adjustment_y
--         in
--           -- Debug.log ("Checking (" ++ String.fromFloat node.label.x ++ ", " ++ String.fromFloat node.label.y ++ ") against (" ++ String.fromFloat mouse_x ++ ", " ++ String.fromFloat mouse_y ++ ")") () |> \_ ->
--           dx * dx + dy * dy <= square_dist -- 7 + 9 = 16
--       )
--       (Graph.nodes userGraph.graph)

-- nearby_node : Float -> (Float, Float) -> Model -> Maybe (Graph.Node Entity)
-- nearby_node =
--   nearby_node_func List.find

-- nearby_nodes : Float -> (Float, Float) -> Model -> List (Graph.Node Entity)
-- nearby_nodes =
--   nearby_node_func List.filter

-- viewPhantomSvg : { x : Float, y : Float } -> PathBetweenReturn -> Svg GraphView_Msg
-- viewPhantomSvg target positioning =
--   let
--     radius = 9
--   in
--     g
--       []
--       [ circle
--           [ r radius
--           , cx target.x
--           , cy target.y
--           , noFill
--           , strokeWidth 2
--           , strokeDasharray "1 5.2"
--           , strokeLinecap StrokeLinecapRound
--           , class ["phantom-state-node"]
--           ]
--           []
--       -- now draw the path to it
--       , path
--           [ strokeWidth 5
--           , stroke <| Paint <| paletteColors.background
--           , d positioning.pathString
--           , noFill
--           , class [ "link" ]
--           ]
--           []
--       , path
--           [ strokeWidth 3
--           , d positioning.pathString
--           , if positioning.length > 10 then markerEnd "url(#phantom-arrowhead)" else markerStart "invalid_ref"
--           , noFill
--           , strokeDasharray "1 5"
--           , strokeLinecap StrokeLinecapRound
--           , class [ "phantom-link" ]
--           ]
--           []
--       -- for debugging the paths.
--       -- , circle
--       --     [ cx <| positioning.control_point.x
--       --     , cy <| positioning.control_point.y
--       --     , r 3
--       --     , fill <| Paint <| Color.red
--       --     ]
--       --     []
--       -- , circle
--       --     [ cx <| positioning.source_connection_point.x
--       --     , cy <| positioning.source_connection_point.y
--       --     , r 3
--       --     , fill <| Paint <| Color.black
--       --     ]
--       --     []
--       -- , circle
--       --     [ cx <| positioning.target_connection_point.x
--       --     , cy <| positioning.target_connection_point.y
--       --     , r 3
--       --     , fill <| Paint <| Color.yellow
--       --     ]
--       --     []
--       ]

-- {-| A "phantom move" that the user MIGHT make, or might not.
--     This function is called when the user is still choosing.
--     `viewPhantomChosen` is called when the user has chosen.
-- -}
-- viewPhantomChoosing : Model -> (Float, Float) -> NodeContext Entity Connection -> Svg GraphView_Msg
-- viewPhantomChoosing model mouseCoords sourceNode =
--   let
--     ( xPan, yPan ) =
--       model.pan
--     nearby = nearby_node nearby_node_lockOnDistance mouseCoords model
--     ( center_x, center_y) =
--       -- Now, if the mouse is over an actual node, then we want to "lock" to that node.
--       -- But if the mouse is anywhere else, just use the mouse coordinates.
--       nearby
--       |> Maybe.map (\node -> (node.label.x - xPan, node.label.y - yPan))
--       |> Maybe.withDefault mouseCoords
--     target =
--       { x = center_x + xPan
--       , y = center_y + yPan
--       }
--     cardinality =
--       case nearby of
--         Nothing ->
--           Unidirectional
--         Just some_node ->
--           if some_node.id == sourceNode.node.id then
--             Recursive
--           else
--             case IntDict.get some_node.id sourceNode.incoming of
--               Just _ ->
--                 Bidirectional
--               Nothing ->
--                 Unidirectional
--     positioning =
--       path_between sourceNode.node.label target cardinality 7 9
--   in
--     viewPhantomSvg target positioning

-- {-| A "phantom move" that the user MIGHT make, or might not.
--     This function is called when the user has chosen their
--     phantom move.
-- -}
-- viewPhantomChosen : Model -> NodeContext Entity Connection -> (Float, Float) -> Cardinality -> Svg GraphView_Msg
-- viewPhantomChosen model sourceNode (dest_x, dest_y) cardinality =
--   let
--     ( xPan, yPan ) =
--       model.pan
--     target =
--       { x = dest_x + xPan
--       , y = dest_y + yPan
--       }
--     positioning =
--       path_between sourceNode.node.label target cardinality 7 9
--   in
--     viewPhantomSvg target positioning

-- {-| A "phantom move" that the user MIGHT make, or might not.
-- -}
-- viewPhantom : Model -> (Float, Float) -> NodeContext Entity Connection -> Svg GraphView_Msg
-- viewPhantom model mouseCoords sourceNode =
--   let
--     cardinalityOf nodeId =
--       if nodeId == sourceNode.node.id then
--         Recursive
--       else
--         case IntDict.get nodeId sourceNode.incoming of
--           Just _ ->
--             Bidirectional
--           Nothing ->
--             Unidirectional
--     nodeCoordinates id =
--       Graph.get id model.package.userGraph.graph
--       |> Maybe.map
--           (\ctx -> ( ctx.node.label.x, ctx.node.label.y ))
--       |> Maybe.withDefaultLazy
--           (\() -> ( 0, 0 ) |> Debug.log "Error K<UFOUDFI8") -- should never get here!
--   in
--     case model.interactionsDict of
--       Just (ModifyingGraph _ { dest }) ->
--         case dest of
--           NoDestination ->
--             viewPhantomChoosing model mouseCoords sourceNode
--           ExistingNode id ->
--             viewPhantomChosen model sourceNode
--               (nodeCoordinates id)
--               (cardinalityOf id)
--           NewNode coords ->
--             viewPhantomChosen model sourceNode coords Unidirectional
--       _ ->
--         g [] []

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

-- -- https://ishadeed.com/article/target-size/
-- viewSingleKey : Char -> Connection -> (Int, Int) -> Float -> Svg GraphView_Msg
-- viewSingleKey ch conn (gridX, gridY) y_offset =
--   let
--     buttonX = transition_spacing * toFloat (gridX + 1) + transition_buttonSize * toFloat gridX
--     buttonY = y_offset + transition_spacing * toFloat (gridY + 1) + transition_buttonSize * toFloat gridY
--     tr finality =
--       Transition finality (ViaCharacter ch)
--     isThisNodeTerminal = AutoSet.member (tr True) conn
--     keyClass =
--       if AutoSet.member (tr False) conn then
--         [ "transition-chooser-key", "selected" ]
--       else if isThisNodeTerminal then
--         [ "transition-chooser-key", "selected", "terminal" ]
--       else
--         [ "transition-chooser-key" ]
--   in
--   g
--     []
--     [ rect
--         [ x buttonX
--         , y buttonY
--         , Px.width transition_buttonSize
--         , Px.height transition_buttonSize
--         , Px.rx 5
--         , Px.ry 5
--         , strokeWidth 2
--         , stroke <| Paint <| Color.white
--         , class keyClass
--         , onClick <| ToggleSelectedTransition (ViaCharacter ch)
--         ]
--         ( if isThisNodeTerminal then [ title [] [ text "This is a terminal transition" ] ] else [] )
--     , text_
--         [ x <| buttonX + transition_buttonSize / 2
--         , y <| buttonY + transition_buttonSize / 2
--         , fontSize 20
--         , strokeWidth 0
--         , textAnchor AnchorMiddle
--         , alignmentBaseline AlignmentCentral
--         , dominantBaseline DominantBaselineMiddle
--         , pointerEvents "none"
--         , fontFamily ["sans-serif"]
--         ]
--         [ text <| String.fromChar ch ]
--     ]

-- viewSvgCharacterChooser : AutoSet.Set String Transition -> Float -> Float -> (Svg GraphView_Msg, Float)
-- viewSvgCharacterChooser conn y_offset w =
--   let
--     items_per_row = round ( (w - transition_spacing * 2) / (transition_buttonSize + transition_spacing) )
--     alphabet = String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890~`[{}]-_=\\|;:,.<>/?!@#$%^&*()+ abcdefghijklmnopqrstuvwxyz"
--     numRows = ceiling <| toFloat (List.length alphabet) / toFloat items_per_row
--     my_height = transition_spacing * toFloat (numRows + 2) + toFloat numRows * transition_buttonSize + transition_spacing
--     gridItemsAndCoordinates =
--       List.foldl
--         (\item (acc, ( col, row )) ->
--           if col + 1 >= items_per_row then
--             ((item, col, row) :: acc, (0, row + 1))
--           else
--             ((item, col, row) :: acc, (col + 1, row))
--         )
--         ([], (0, 0))
--         alphabet
--       |> Tuple.first
--     chooser_svg =
--       g
--         []
--         ( List.map
--             (\(item, col, row) ->
--               viewSingleKey
--                 item
--                 conn
--                 (col, row)
--                 y_offset
--             )
--             gridItemsAndCoordinates
--         )
--   in
--     ( chooser_svg, my_height )

-- -- viewSvgGraphRefChooser : Int -> Model -> AutoSet.Set String Transition -> Float -> Float -> (Svg GraphView_Msg, Float)
-- -- viewSvgGraphRefChooser focusedIndex model conn y_offset panelWidth =
-- --   let
-- --     -- this is the width of each thumbnail.
-- --     -- and it is ALSO the center position in the panel.
-- --     packages = model.packages
-- --     panelCenter = panelWidth / 2
-- --     width = panelWidth / 2
-- --     height = thumbnailHeight width
-- --     -- and here, confusingly, I use the meanings in both ways ^_^!
-- --     x_start = panelCenter - (width / 2)
-- --     y_start =
-- --       2 + -- the stroke-width
-- --       5 + -- the padding (top)
-- --       10 + -- margin (top)
-- --       y_offset
-- --     -- now, one of the thumbnails are going to be in-frame, and others will not be.
-- --     -- there will be a half-thumbnail to the left and to the right.
-- --     -- … with a gap between, too, so a bit less then a half.
-- --     gap_between = panelWidth * 0.05 -- the gap is 5% of the width
-- --     -- with extra margin at the bottom
-- --     thumbnailPosition idx =
-- --       x_start + (toFloat idx * (gap_between + width))
-- --     my_height = 4 + 10 + 40 + height + 90
-- --     makeThumbnail pkg =
-- --       viewComputationThumbnail (panelWidth / 2) model pkg
-- --     keys_and_thumbs =
-- --       AutoDict.toList packages
-- --       |> List.map (\(ref, v) -> (ref, makeThumbnail v))
-- --     thumbnail_carousel =
-- --       g
-- --         [ transform
-- --             [ Translate
-- --                 (-1 * (thumbnailPosition focusedIndex - width/2))
-- --                 0
-- --             ]
-- --         , class [ "thumbnail-carousel" ]
-- --         ]
-- --         ( List.indexedMap
-- --             (\i (ref, thumb) ->
-- --               let
-- --                 tr finality = Transition finality (ViaGraphReference ref)
-- --                 isSelected =
-- --                   AutoSet.member (tr True) conn || AutoSet.member (tr False) conn
-- --                 scale = height / 5
-- --               in
-- --                 g
-- --                   [ transform [ Translate (thumbnailPosition i) y_start ]
-- --                   ]
-- --                   [ -- give it a background
-- --                     rect
-- --                       [ x 0
-- --                       , y 0
-- --                       , TypedSvg.Attributes.InPx.width <| width
-- --                       , TypedSvg.Attributes.InPx.height <| height
-- --                       , fill <| Paint <|
-- --                           if isSelected then Color.rgb 0.596 0.984 0.596 else Color.rgb 1 1 0.941
-- --                       , rx 15
-- --                       , ry 15
-- --                       ]
-- --                       []
-- --                     -- there is nothing interactable in the thumbnail, so this is
-- --                     -- really just for getting the types to line up sensibly.
-- --                   , thumb |> Svg.map (\_ -> Tick)
-- --                   , -- cover it with a "top" which is interactable
-- --                     rect
-- --                       [ x -5
-- --                       , y -5
-- --                       , TypedSvg.Attributes.InPx.width <| width + 10
-- --                       , TypedSvg.Attributes.InPx.height <| height + 10
-- --                       , fill <| Paint <| Color.rgba 1 1 1 0.1
-- --                       , rx 15
-- --                       , ry 15
-- --                       , strokeWidth 2
-- --                       , stroke <| Paint <|
-- --                           if isSelected then Color.green else Color.black
-- --                       , cursor CursorPointer
-- --                       , onClick <| ToggleSelectedTransition (ViaGraphReference ref)
-- --                       ]
-- --                       []
-- --                   , -- if the item is selected, then put on a UI for things that can be
-- --                     -- changed about it.
-- --                     -- first up, a big checkbox to say "Yes! You're selected!"
-- --                     if isSelected then
-- --                       -- REMEMBER: I'm already translated! So everything is relative
-- --                       -- to the "frame" of this particular thumbnail.
-- --                       -- …which certainly is convenient in some respects…!
-- --                       g
-- --                         []
-- --                         [ circle
-- --                             [ cx <| width / 2
-- --                             , cy <| height - height / 5
-- --                             , r <| scale / 2
-- --                             , fill <| Paint <| if AutoSet.member (tr False) conn then Color.white else paletteColors.transition.final
-- --                             ]
-- --                             []
-- --                         , text_
-- --                             [ x <| width / 2
-- --                             , y <| height - height / 5 + 5
-- --                             , fill <| Paint <| Color.green
-- --                             , fontSize scale
-- --                             , dominantBaseline DominantBaselineMiddle
-- --                             -- , alignmentBaseline AlignmentCentral
-- --                             , textAnchor AnchorMiddle
-- --                             ]
-- --                             [ text "✔" ]
-- --                         ]
-- --                     else
-- --                       g [][]
-- --                   ]
-- --             )
-- --             keys_and_thumbs
-- --         )
-- --     with_ui =
-- --       g
-- --         []
-- --         [ thumbnail_carousel
-- --           -- the static UI. Must place this after the thumbnails so
-- --           -- that it appears on top; and must place it outside of
-- --           -- the transformed group, so it is relative to the screen.
-- --           -- move right, if such a thing is possible
-- --         , if focusedIndex < AutoDict.size packages - 1 then
-- --             g
-- --               []
-- --               [ rect
-- --                   [ x <| panelCenter + 30
-- --                   , y <| y_offset + height + 30
-- --                   , Px.width 60
-- --                   , Px.height 60
-- --                   , Px.rx 5
-- --                   , Px.ry 5
-- --                   , class [ "transition-chooser-key" ]
-- --                   , strokeWidth 2
-- --                   , stroke <| Paint <| Color.white
-- --                   , onClick SwitchToNextComputation
-- --                   ]
-- --                   []
-- --               , text_
-- --                   [ x <| panelCenter + 60
-- --                   , y <| y_offset + height + 60
-- --                   , fontSize 40
-- --                   , rx 15
-- --                   , ry 15
-- --                   , strokeWidth 0
-- --                   , textAnchor AnchorMiddle
-- --                   , alignmentBaseline AlignmentCentral
-- --                   , dominantBaseline DominantBaselineMiddle
-- --                   , pointerEvents "none"
-- --                   ]
-- --                   [ text <| "→" ]
-- --               , title
-- --                   []
-- --                   [ text "Next computation" ]
-- --               ]
-- --           else
-- --             g [] []
-- --         , if focusedIndex > 0 then
-- --             g
-- --               []
-- --               [ rect
-- --                   [ x <| panelCenter - 60
-- --                   , y <| y_offset + height + 30
-- --                   , Px.width 60
-- --                   , Px.height 60
-- --                   , Px.rx 5
-- --                   , Px.ry 5
-- --                   , class [ "transition-chooser-key" ]
-- --                   , strokeWidth 2
-- --                   , stroke <| Paint <| Color.white
-- --                   , onClick SwitchToPreviousComputation
-- --                   ]
-- --                   []
-- --               , text_
-- --                   [ x <| panelCenter - 30
-- --                   , y <| y_offset + height + 60
-- --                   , fontSize 40
-- --                   , rx 15
-- --                   , ry 15
-- --                   , strokeWidth 0
-- --                   , textAnchor AnchorMiddle
-- --                   , alignmentBaseline AlignmentCentral
-- --                   , dominantBaseline DominantBaselineMiddle
-- --                   , pointerEvents "none"
-- --                   ]
-- --                   [ text <| "←" ]
-- --               , title
-- --                   []
-- --                   [ text "Previous computation" ]
-- --               ]
-- --           else
-- --             g [] []
-- --         ]
        
-- --   in
-- --     ( with_ui, my_height )

-- viewSvgTransitionChooser : AcceptChoice -> Model -> Svg GraphView_Msg
-- viewSvgTransitionChooser via model =
--   let
--     ( w, _ ) = model.dimensions
--     category_chooser_space = 80
--     conn =
--       case model.interactionsDict of
--         Just (ModifyingGraph _ { transitions }) ->
--           transitions
--         Just (AlteringConnection _ { transitions }) ->
--           transitions
--         _ ->
--           (AutoSet.empty transitionToString)
--           |> Debug.log "CXO<DPO(*EU I should not be here!"
--     ( chooser_svg, chooser_height) =
--       case via of
--         ChooseCharacter ->
--           viewSvgCharacterChooser conn category_chooser_space w
--         ChooseGraphReference idx ->
--           g [] []
--           -- viewSvgGraphRefChooser idx model conn category_chooser_space w
--     chooserButtonSize = 30
--   in
--   g
--     []
--     [ -- this is above the "keyboard"
--       text_
--         [ x <| w / 2
--         , y <| 16 + 8
--         , fill <| Paint <| Color.black
--         , textAnchor AnchorMiddle
--         , alignmentBaseline AlignmentCentral
--         , dominantBaseline DominantBaselineMiddle
--         , pointerEvents "none"
--         , fontFamily ["Serif"]
--         , fontSize 16
--         , stroke <| Paint <| paletteColors.background
--         , strokeWidth 2
--         , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
--         ]
--         [ tspan
--           []
--           [ text <| "To make this jump, I should match…"
--           ]
--         ]
--     , text_
--         [ x <| w / 2
--         , y <| 32 + 23
--         , fill <| Paint <| Color.black
--         , textAnchor AnchorMiddle
--         , alignmentBaseline AlignmentCentral
--         , dominantBaseline DominantBaselineMiddle
--         , pointerEvents "none"
--         , fontSize 16
--         , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
--         , fontFamily ["Serif"]
--         , stroke <| Paint <| paletteColors.background
--         , strokeWidth 2
--         ]
--         [ tspan
--           []
--           [ text <|
--               case via of
--                 ChooseCharacter -> "Single characters"
--                 ChooseGraphReference _ -> "Computations"
--           ]
--         ]
--       -- choose-next button
--     , g
--         []
--         [ rect
--             [ x <| (w / 2) + 100
--             , y <| 16 + 23
--             , Px.width chooserButtonSize
--             , Px.height chooserButtonSize
--             , Px.rx 5
--             , Px.ry 5
--             , class [ "transition-chooser-key" ]
--             , strokeWidth 2
--             , stroke <| Paint <| Color.white
--             , onClick <| SwitchVia <|
--                 case via of
--                   ChooseCharacter -> ChooseGraphReference 0
--                   ChooseGraphReference _ -> ChooseCharacter
--             ]
--             []
--         , text_
--             [ x <| (w / 2) + 100 + (chooserButtonSize / 2)
--             , y <| 39 + (chooserButtonSize / 2)
--             , fontSize 20
--             , strokeWidth 0
--             , textAnchor AnchorMiddle
--             , alignmentBaseline AlignmentCentral
--             , dominantBaseline DominantBaselineMiddle
--             , pointerEvents "none"
--             ]
--             [ text <| "→" ]
--         ]
--       -- chose-prev button
--     , g
--         []
--         [ rect
--             [ x <| (w / 2) - 100 - chooserButtonSize
--             , y <| 16 + 23
--             , Px.width chooserButtonSize
--             , Px.height chooserButtonSize
--             , Px.rx 5
--             , Px.ry 5
--             , class [ "transition-chooser-key" ]
--             , strokeWidth 2
--             , stroke <| Paint <| Color.white
--             , onClick <| SwitchVia <|
--                 case via of
--                   ChooseCharacter -> ChooseGraphReference 0
--                   ChooseGraphReference _ -> ChooseCharacter
--             ]
--             []
--         , text_
--             [ x <| (w / 2) - 100 - (chooserButtonSize / 2)
--             , y <| 39 + (chooserButtonSize / 2)
--             , fontSize 20
--             , strokeWidth 0
--             , textAnchor AnchorMiddle
--             , alignmentBaseline AlignmentCentral
--             , dominantBaseline DominantBaselineMiddle
--             , pointerEvents "none"
--             ]
--             [ text <| "←" ]
--         ]
--       -- this is below the "keyboard"
--     , text_
--         [ x <| w / 2
--         , y <| category_chooser_space + chooser_height
--         , fill <| Paint <| Color.black
--         , textAnchor AnchorMiddle
--         , alignmentBaseline AlignmentCentral
--         , dominantBaseline DominantBaselineMiddle
--         , pointerEvents "none"
--         , fontSize 24
--         , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
--         , stroke <| Paint <| paletteColors.background
--         , strokeWidth 3
--         , class [ "link" ]
--         ]
--         ( tspan
--             [ fontFamily ["Serif"] ]
--             [ text "Selected transitions: " ] ::
--           ( if AutoSet.isEmpty conn then
--               [ tspan
--                   [ fontFamily ["sans-serif"] ]
--                   [ text "None" ]
--               ]
--             else
--               connectionToSvgText conn
--           )
--         )
--     , text_
--         [ x <| w / 2
--         , y <| category_chooser_space + chooser_height + 30
--         , fill <| Paint <| Color.darkCharcoal
--         , textAnchor AnchorMiddle
--         , alignmentBaseline AlignmentCentral
--         , dominantBaseline DominantBaselineMiddle
--         , pointerEvents "none"
--         , fontSize 16
--         , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
--         , stroke <| Paint <| paletteColors.background
--         , strokeWidth 3
--         , fontFamily ["Serif"]
--         ]
--         ( if AutoSet.isEmpty conn then
--             [ tspan [] [ text "If there are no transitions, this link will be destroyed." ] ]
--           else
--             [ tspan [] [ text "Press «Enter» to confirm these transitions." ] ]
--         )
--       -- and lastly, this is the "keyboard"
--     , chooser_svg
--     ]

-- viewSvgTransitionSplitter : Connection -> Connection -> Model -> Svg GraphView_Msg
-- viewSvgTransitionSplitter left right model =
--   let
--     ( w, _ ) = model.dimensions
--     paddingLeftRight = 15
--     calc_width n =
--       ( paddingLeftRight * 2 + -- padding on left & right
--         1 * font_size * -- space per character, with a weight factor
--         toFloat n -- multiplied by number of characters
--       )
--     max_box_width =
--       calc_width (AutoSet.size left + AutoSet.size right)
--     leftCenter = w / 2 - max_box_width
--     rightCenter = w / 2 + max_box_width
--     font_size = 24
--     box centerPosition set color =
--       rect
--         [ x <| centerPosition - calc_width (AutoSet.size set) / 2
--         , y <| 100 - font_size
--         , width <| calc_width (AutoSet.size set) 
--         , height <| font_size * 2
--         , fill <| Paint <| color
--         , rx 10
--         , ry 10
--         , class ["splitter-box"]
--         ]
--         []
--     box_text centerPosition set =
--       text_
--         [ x centerPosition
--         , y 100
--         , textAnchor AnchorMiddle
--         , alignmentBaseline AlignmentCentral
--         , dominantBaseline DominantBaselineMiddle
--         , pointerEvents "none"
--         , fontSize font_size
--         , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
--         ]
--         ( connectionToSvgText set )
--     box_title centerPosition s =
--       text_
--         [ x centerPosition
--         , y <| (100 + font_size * 1.8)
--         , textAnchor AnchorMiddle
--         , fontSize <| font_size * 0.6
--         , alignmentBaseline AlignmentCentral
--         , dominantBaseline DominantBaselineMiddle
--         , pointerEvents "none"
--         , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
--         , stroke <| Paint <| paletteColors.background
--         , strokeWidth 5
--         ]
--         [ text s ]
--   in
--     g
--       []
--       [ box leftCenter left Color.lightGreen
--       , box rightCenter right Color.lightPurple
--       , box_text leftCenter left
--       , box_text rightCenter right
--       , box_title leftCenter "Node “A”"
--       , box_title rightCenter "Node “B”"
--       , text_
--           [ x <| w / 2
--           , y <| 180
--           , fill <| Paint <| Color.darkCharcoal
--           , textAnchor AnchorMiddle
--           , alignmentBaseline AlignmentCentral
--           , dominantBaseline DominantBaselineMiddle
--           , pointerEvents "none"
--           , fontSize 16
--           , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
--           , stroke <| Paint <| paletteColors.background
--           , strokeWidth 4
--           ]
--           ( if AutoSet.isEmpty left || AutoSet.isEmpty right then
--               [ tspan [] [ text "If either side has no transitions, then no changes will be made." ] ]
--             else
--               [ tspan [] [ text "Press «Enter» to confirm this split, or «Esc» to cancel." ] ]
--           )
--       ]
--   -- rect
--   --   [ x 100
--   --   , y 100
--   --   , fill <| Paint Color.lightOrange
--   --   , width 150
--   --   , height 100
--   --   ]
--   --   []

viewUndoRedoVisualisation : GraphView -> Svg a
viewUndoRedoVisualisation { package, guest_coordinates, guest_dimensions } =
  let
    h = Tuple.second guest_coordinates + Tuple.second guest_dimensions
    rect_width = 30
    rect_height = 10
    rect_spacing = 3
    num_undo = List.length package.undoBuffer
    idxToY idx =
      h - (toFloat (40 + idx * (rect_height + 1) + idx * (rect_spacing - 1)))

    worst = Color.rgb255 0xfe 0x00 0x02 -- fire
    scale =
      [ Color.rgb255 0x00 0xf0 0xa8 -- spring
      , Color.rgb255 0x50 0xc8 0x78 -- emerald
      , Color.rgb255 0x00 0xa8 0x6b -- jade
      , Color.rgb255 0x00 0x9e 0x60 -- shamrock
      , Color.rgb255 0x80 0xf9 0xad -- sea foam
      , Color.rgb255 0x98 0xfb 0x98 -- mint
      , Color.rgb255 0xdf 0xff 0x00 -- chartreuse
      , Color.rgb255 0xff 0xff 0x00 -- yellow
      , Color.rgb255 0xff 0xd7 0x00 -- gold
      , Color.rgb255 0xff 0xa6 0x00 -- cheese
      , Color.rgb255 0xff 0x80 0x00 -- orange
      , Color.rgb255 0xfc 0x4c 0x02 -- tangelo
      , Color.rgb255 0xff 0x24 0x00 -- scarlet
      , worst
      ]
  in
    g
      []
      (
        ( List.indexedMap
            (\idx _ ->
              rect
                [ TypedSvg.Attributes.InPx.width rect_width
                , TypedSvg.Attributes.InPx.height rect_height
                , fill <| Paint (List.getAt idx scale |> Maybe.withDefault worst)
                , ry 2
                , x 15
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
                , x 15
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
    [ transform [ matrixFromZoom graph_view.pan graph_view.zoom ]
    -- , case graph_view.interactionsDict of
    --     Just (Dragging _) ->
    --       -- disable pointer-events.
    --       -- This is to stop elements from getting in the way of
    --       -- registering mouse-movement.
    --       pointerEvents "none"
    --     _ ->
    --       class []
    , class
        ( if graph_view.properties.isFrozen then
            [ "graph" ]
          else
            classList
              [ ("graph", True)
              , ("can-select-nodes", graph_view.properties.canSelectNodes)
              , ("can-select-connections", graph_view.properties.canSelectConnections)
              , ("can-select-space", graph_view.properties.canSelectEmptySpace)
              , ("can-split-nodes", graph_view.properties.canSplitNodes)
              ]
        )
    ]
    [ defs [] arrowheadDefs
    , Dict.toList graph_view.drawingData.link_drawing
      |> List.map (\((from, to), data) -> viewLink (from, to) data)
      |> g [ class [ "edges" ] ]
    , Dict.toList graph_view.drawingData.node_drawing
      |> List.map (\(nodeId, data) -> viewNode graph_view.properties nodeId data)
      |> g [ class [ "nodes" ] ]
    -- , case graph_view.interactionsDict of
    --     Just (ModifyingGraph _ { source }) ->
    --       Graph.get source graph_view.package.userGraph.graph
    --       |> Maybe.map (viewPhantom graph_view mouseCoords)
    --       |> Maybe.withDefault (g [] [])
    --     _ ->
    --       g [] []
    ]

-- {-| Given a width, get the correct height for a thumbnail -}
-- thumbnailHeight : Float -> Float
-- thumbnailHeight w =
--   -- just fudge some kind of an "appropriate" aspect ratio ;-)!
--   w / sqrt 5

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

-- viewComputationThumbnail : Float -> Model -> (Float, Float) -> GraphPackage -> Svg GraphView_Msg
-- viewComputationThumbnail width m mouseCoords { dimensions, userGraph, description } =
--   let
--     height = thumbnailHeight width
--     ( m_w, m_h ) = dimensions
--     inner_pad = 40
--     ( (min_x, max_x), (min_y, max_y) ) =    
--       Graph.fold
--         (\ctx ((xmin, xmax), (ymin, ymax)) ->
--           ( ( min ctx.node.label.x xmin
--             , max ctx.node.label.x xmax
--             )
--           , ( min ctx.node.label.y ymin
--             , max ctx.node.label.y ymax
--             )
--           )
--         )
--         ((m_w, 0), (m_h, 0))
--         userGraph.graph
--       |> \((a, b), (c, d)) -> ((a - inner_pad, b + inner_pad*2), (c - inner_pad, d + inner_pad*2))
--       -- |> Debug.log "Bounds"
--     fontSize = 16
--     lineSpacing = 1.2
--     fcc = 2.0 -- font-character-constant; typeface-specific
--     brokenString =
--       Maybe.map (breakStringBadly fcc fontSize width) description
--       |> Maybe.withDefault []
--     descriptionStart_y =
--       height -
--         (toFloat (List.length brokenString - 1) * (toFloat fontSize * lineSpacing) )
--         - 0.5 * fontSize
--     pkg = m.package
--   in
--     -- this takes the vast majority of its functionality from ForceDirectedGraph.elm
--     -- so, since I can nest a SVG inside a SVG, let me just abuse that a bit…
--     svg
--       [
--         TypedSvg.Attributes.InPx.width width
--       , TypedSvg.Attributes.InPx.height height
--       ]
--       [ svg
--         {-
--           As I get in 
--         -}
--         [ TypedSvg.Attributes.viewBox
--             min_x -- x-offset
--             min_y -- y-offset
--             (max_x - min_x) -- width
--             (max_y - min_y) -- height
--         , TypedSvg.Attributes.pointerEvents "none"
--         ]
--         [ --Automata.Debugging.debugAutomatonGraph "Thumbnail" model.userGraph |> \_ ->
--           viewMainSvgContent
--             { m
--               | package =
--                   { pkg
--                     | userGraph = .solvedGraph (computeGraphFully (m_w, m_h) userGraph)
--                   }
--               , interactionsDict = Nothing
--             }
--             mouseCoords
--         ]
--       , TypedSvg.g
--           []
--           [ viewGraphReference userGraph.graphIdentifier 0 0 ]
--       , text_
--           [ textAnchor AnchorMiddle
--           , alignmentBaseline AlignmentCentral
--           , fill <| Paint <| Color.black
--           , fontFamily [ "serif" ]
--           , strokeWidth 2
--           , stroke <| Paint <| Color.white
--           , Html.Attributes.attribute "paint-order" "stroke fill markers"
--           ]
--           ( List.indexedMap
--               (\i line ->
--                 tspan
--                   [ x <| width / 2
--                   , y <| descriptionStart_y + (toFloat i * (toFloat fontSize * lineSpacing))
--                   ]
--                   [ text line ]
--               )
--               brokenString
--           )
--       ]

viewGraph : GraphView -> Svg Msg
viewGraph graphView =
  let
    ( host_width, host_height ) = graphView.host_dimensions
    ( guest_width, guest_height) = graphView.guest_dimensions
    ( guest_x, guest_y ) = graphView.guest_coordinates
    -- ( mouse_x, mouse_y ) = mouseCoords
    -- permit_scroll : Svg Main_Msg
    -- permit_scroll =
    --   onMouseScroll
    --     (\n ->
    --       case model.interactionsDict of
    --         Just (ModifyingGraph (ChooseGraphReference _) _) ->
    --           if n > 0 then SwitchToNextComputation else SwitchToPreviousComputation
    --         Just (AlteringConnection (ChooseGraphReference _) _) ->
    --           if n > 0 then SwitchToNextComputation else SwitchToPreviousComputation
    --         _ ->
    --           Zoom n
    --     )
    panBuffer = panBufferAmount graphView.guest_dimensions
    -- permit_pan : List (Svg GraphView_Msg)
    -- permit_pan =
    --   [ onMouseMove MouseMove
    --   , cursor <|
    --       -- working around an insane Elm-compiler parser bug https://github.com/elm/compiler/issues/1261
    --       case ( 1 + round (xPanAt width panBuffer mouse_x), 1 + round (yPanAt height panBuffer mouse_y) ) of
    --         ( 2, 2 ) -> CursorSEResize
    --         ( 0, 2 ) -> CursorSWResize
    --         ( 2, 0 ) -> CursorNEResize
    --         ( 0, 0 ) -> CursorNWResize
    --         ( 1, 2 ) -> CursorNResize
    --         ( 0, 1 ) -> CursorWResize
    --         ( 1, 0 ) -> CursorNResize -- eh? where's CursorSResize?
    --         ( 2, 1 ) -> CursorEResize
    --         _ -> CursorDefault
    --   ]
    -- permit_click : Svg GraphView_Msg
    -- permit_click =
    --   Mouse.onWithOptions
    --     "mousedown"
    --     { stopPropagation = True, preventDefault = True }
    --     (\_ ->
    --       case model.interactionsDict of
    --         Just (ModifyingGraph _ _) ->
    --           case nearby_node nearby_node_lockOnDistance mouseCoords model of
    --             Just node -> -- in this case, the UI would show it being "locked-on"
    --               CreateOrUpdateLinkTo node.id
    --             Nothing ->
    --               CreateNewNodeAt mouseCoords
    --         _ ->
    --           Escape
    --     )
    -- permit_stopdrag : Svg GraphView_Msg
    -- permit_stopdrag =
    --   Mouse.onUp (\_ -> NodeDragEnd)
    -- interactivity : List (Svg GraphView_Msg)
    -- interactivity =
      -- don't permit panning if:
      -- 1. I'm splitting a node
      -- 2. I'm editing a transition/connection
      --
      -- don't permit zooming under the same circumstances as above.
      --
      -- don't permit clicking if:
      -- 1. I'm modifying the graph, and have already selected/created a node
      --
      -- but clicking has a default of Escape, so we can usually allow it.
      -- CHECK THIS!
      -- Usability: should a "mis-click" result in Escape??
      -- Let's see.
      -- case model.interactionsDict of
      --   Just (Splitting _) ->
      --     [ permit_click ]
      --   Just (AlteringConnection (ChooseGraphReference _) _) ->
      --     [ permit_scroll ]
      --   Just (ModifyingGraph (ChooseGraphReference _) _) ->
      --     [ permit_scroll ]
      --   Just (ModifyingGraph _ { dest }) ->
      --     case dest of
      --       NewNode _ ->
      --         []
      --       ExistingNode _ ->
      --         []
      --       _ ->
      --         permit_click :: permit_scroll :: permit_pan
      --   Just (AlteringConnection _ _) ->
      --     []
      --   Just (Executing _ _) ->
      --     permit_pan
      --   Just (Dragging _) ->
      --     -- must have permit_pan (for mouse-movement)
      --     -- must have permit_stopdrag
      --     -- ignore click (we only care about stop-drag right now)
      --     permit_stopdrag :: permit_scroll :: permit_pan
      --   Nothing ->
      --     permit_click :: permit_scroll :: permit_pan
  in
    svg
      ([ viewBox guest_x guest_y guest_width guest_height
       , TypedSvg.Attributes.InPx.width host_width
       , TypedSvg.Attributes.InPx.height host_height
      --  , TypedSvg.Attributes.style <| "max-width:" ++ (String.fromFloat width) ++ "px"
      --  , TypedSvg.Attributes.InPx.height height
      --  , TypedSvg.Attributes.preserveAspectRatio (Align ScaleMin ScaleMin) Meet
      -- , Mouse.onOver (\_ -> SetMouseOver model.id True)
      -- , Mouse.onOut (\_ -> SetMouseOver model.id False)
      ] {- ++ interactivity -})
      [ -- this stuff is in the background.
        viewUndoRedoVisualisation graphView
      , viewMainSvgContent graphView -- this is the "main" interactive frame, which will be zoomed, panned, etc.
      -- , case graphView.interactionsDict of
      --     Just (ModifyingGraph via { dest }) ->
      --       case dest of
      --         NoDestination ->
      --           g [] []
      --         _ ->
      --           viewSvgTransitionChooser via graphView
      --     Just (AlteringConnection via _) ->
      --       viewSvgTransitionChooser via graphView
      --     Just (Splitting { to_split, left, right }) ->
      --       Graph.get to_split graphView.package.userGraph.graph
      --       |> Maybe.map (\_ -> -- ignore node, we have all the info already
      --         viewSvgTransitionSplitter left right graphView
      --       )
      --       |> Maybe.withDefault (g [] [])
      --     Just (Executing _ _) ->
      --       g [] [] -- no overlay needed. (but if I *DO* put in some overlay at some point, it'd go here!!)
      --     Nothing ->
      --       g [] []
      --     Just (Dragging _) ->
      --       g [] []
      ,
        -- let
        --   bottomMsg message =
        --     text_
        --       [ Px.x 15
        --       , Px.y <| height - 15
        --       , fill <| Paint <| Color.black
        --       , fontFamily ["sans-serif"]
        --       , fontSize 14
        --       , textAnchor AnchorStart
        --       , alignmentBaseline AlignmentCentral
        --       , dominantBaseline DominantBaselineCentral
        --       , pointerEvents "none"
        --       ]
        --       [ text message ]
        -- in
        if graphView.properties.isFrozen then
          g [] []
        else
          g
            [ ]
            [ --rect
                -- [ x (Tuple.first model.dimensions - 121)
                -- , y (Tuple.second model.dimensions - 30)
                -- , Px.width 120
                -- , Px.height 30
                -- , stroke <| Paint <| Color.black
                -- , strokeWidth 1
                -- , Px.rx 5
                -- , Px.ry 5
                -- ]
                -- []
              text_
                [ x <| (guest_x + guest_width) - 5
                , y <| (guest_y + guest_height) - 10
                , class [ "status-line", "zoom" ]
                ]
                [ text (" 🔍 " ++ String.fromInt (round <| graphView.zoom * 100) ++ "%") ]
            , text_
                [ x <| (guest_x + guest_width) - 80
                , y <| (guest_y + guest_height) - 10
                , class [ "status-line", "pan" ]
                ]
                [ text ("🧭 " ++ panToString graphView.pan) ]
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

-- onMouseScroll : (Float -> msg) -> Html.Attribute msg
-- onMouseScroll msg =
--   HE.on "wheel" <| D.map msg (D.field "deltaY" D.float)

-- onMouseMove : (Float -> Float -> msg) -> Html.Attribute msg
-- onMouseMove msg =
--   HE.on "mousemove"
--     ( D.at ["target", "tagName"] D.string
--       |> D.andThen
--         (\tagName ->
--           if String.toUpper tagName == "SVG" then
--             D.map2 msg
--               -- offset relative to the parent
--               -- …which is the SVG element.
--               (D.field "offsetX" D.float)
--               (D.field "offsetY" D.float)
--           else
--             D.fail "Ignoring non-SVG target"
--         )
--     )

-- debugModel_ : String -> Model -> Model
-- debugModel_ message model =
--   let
--     op =
--       case model.interactionsDict of
--         Just o -> "(" ++ Debug.toString o ++ ")"
--         Nothing -> "(no op)"
--     graph =
--       Automata.Debugging.printAutomatonGraph
--         model.package.userGraph
--     screen =
--       "(" ++ String.fromFloat (Tuple.first model.dimensions) ++ ", " ++ String.fromFloat (Tuple.second model.dimensions) ++
--       " : " ++ panToString model.pan ++ ")"
--     buffers =
--       String.fromInt (List.length model.package.undoBuffer) ++ " / " ++ String.fromInt (List.length model.package.redoBuffer) ++ " undo/redo"
--     disconnected =
--       "{ " ++ (Set.map String.fromInt model.disconnectedNodes |> Set.toList |> String.join ", ") ++ " }"
--     pending =
--       "Undobuffer: " ++
--       ( case model.package.undoBuffer of
--           [] -> "nothing"
--           _ ->
--             "\n" ++
--             ( model.package.undoBuffer
--               |> List.map (DFA.encodeAutomatonGraph >> Debug.toString >> String.padLeft 4 ' ')
--               |> String.join "\n"
--             )
--       )
--   in
--     Debug.log (message
--       ++ "\n" ++ op ++ " " ++ screen ++ " " ++ buffers ++ " " ++ disconnected ++ "\n" ++
--       pending ++ "\n" ++ graph) ()
--     |> \_ -> model
