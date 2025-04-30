module ForceDirectedGraph exposing (..)
import Browser.Events
import Color
import Force
import Graph exposing (Edge, Graph, NodeContext, NodeId)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import TypedSvg exposing
  (circle, g, line, svg, title, text_, marker, path, defs, tspan)
import TypedSvg.Attributes exposing
  ( class, fill, stroke, viewBox, fontFamily, fontWeight, alignmentBaseline
  , textAnchor, cursor, id, refX, refY, orient, d, markerEnd, dominantBaseline
  , color, noFill)
import TypedSvg.Attributes.InPx exposing
  ( cx, cy, r, strokeWidth, x1, x2, y1, y2, x, y, height, fontSize
  , markerWidth, markerHeight)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing
  (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..)
  , Cursor(..), DominantBaseline(..))
import Automata.Data exposing (Node, Connection, AutomatonGraph)
import Html.Attributes exposing (attribute)
import Set
import IntDict

type Msg
  = DragStart NodeId ( Float, Float )
  | DragAt ( Float, Float )
  | DragEnd ( Float, Float )
  | Tick
  | GraphUpdated AutomatonGraph
  | ViewportUpdated (Float, Float)


type alias Model =
  { drag : Maybe Drag
  , graph : Graph Entity Connection
  , start : NodeId
  , simulation : Force.State NodeId
  , dimensions : (Float, Float) -- (w,h) of svg element
  , basicForces : List (Force.Force NodeId) -- EXCLUDING the "center" force.
  , viewportForces : List (Force.Force NodeId)
  , specificForces : IntDict.IntDict (List (Force.Force NodeId))
  }


type alias Drag =
  { start : ( Float, Float )
  , current : ( Float, Float )
  , index : NodeId
  }


type alias Entity =
    Force.Entity NodeId { value : { isTerminal : Bool, isFinal : Bool } }

{-| True if at least one transition terminates at this node -}
isTerminalNode : Node -> Bool
isTerminalNode node =
  IntDict.foldl
    (\_ conn state ->
      state || 
        Set.foldl
          (\(_, isFinal) state_ -> state_ || isFinal == 1)
          False
          conn
    )
    False
    (node.incoming)

initializeNode : Node -> NodeContext Entity Connection
initializeNode ctx =
  { node =
    { label =
        Force.entity ctx.node.id
          { isTerminal = isTerminalNode ctx
          , isFinal = IntDict.isEmpty ctx.outgoing
          }
    , id = ctx.node.id
    }
  , incoming = ctx.incoming
  , outgoing = ctx.outgoing
  }

viewportForces : (Float, Float) -> Graph Entity Connection -> List (Force.Force NodeId)
viewportForces (w, h) _ =
  [ Force.center (w / 2) (h / 2)
  --   Force.towardsX <|
  --     List.filterMap
  --       (\n ->
  --         if n.id == 0 then
  --           Just { node = 0, strength = 0.15, target = 0 }
  --         else if n.id == finalNode then
  --           Just { node = n.id, strength = 0.05, target = w }
  --         else
  --           Nothing
  --       )
  --       (Graph.nodes graph)
  -- , Force.towardsY <|
  --     List.filterMap
  --       (\n ->
  --         if n.id /= 0 && n.id /= finalNode then
  --           Just { node = n.id, strength = 0.05, target = 0 }
  --         else
  --           Just { node = n.id, strength = 0.05, target = h }
  --       )
  --       (Graph.nodes graph)
  ]
basicForces : NodeId -> Graph Entity Connection -> (Int, Int) -> List (Force.Force NodeId)
basicForces start graph (width, height) =
  [
    Force.customLinks 3 <|
      List.map
        (\e ->
          { source = e.from
          , target = e.to
          , distance = 10.0 + 25.0 * toFloat (Set.size e.label) --35-40 seems like a good distance
          , strength = Just 0.7 -- * (toFloat <| Set.size e.label)
          }
        )
      (Graph.edges graph)
    -- Force.links <| List.map link <| Graph.edges graph
  , Force.manyBodyStrength -3000.0 <| List.map .id <| Graph.nodes graph
  -- , Force.manyBody <| List.map .id <| Graph.nodes graph
  , Force.towardsX <|
      List.filterMap
        (\n ->
          if n.id == start then
            Just { node = start, strength = 0.1, target = 0 }
          else
            Nothing
        )
        (Graph.nodes graph)
  , Force.towardsY <|
      List.filterMap
        (\n ->
          if n.id == start then
            Just { node = n.id, strength = 0.8, target = toFloat (height // 2) }
          else
            Nothing
        )
        (Graph.nodes graph)
  ]

makeSimulation : NodeId -> (Float, Float) -> Graph Entity Connection -> Force.State NodeId
makeSimulation start (w, h) graph =
  Force.simulation
    (basicForces start graph (round w, round h) ++ viewportForces (w, h) graph)

toForceGraph : AutomatonGraph -> Graph Entity Connection
toForceGraph g =
  Graph.mapContexts initializeNode g.graph

receiveDAWG : AutomatonGraph -> (Float, Float) -> Model
receiveDAWG dawg (w, h) =
  let
    forceGraph = toForceGraph (dawg {- |> Debug.log "Received by ForceDirectedGraph" -} )
    basic = basicForces dawg.root forceGraph (round w, round h)
    viewport = viewportForces (w, h) forceGraph
  in
    { drag = Nothing
    , graph = forceGraph
    , simulation = Force.simulation (basic ++ viewport)
    , dimensions = (w, h)
    , basicForces = basic
    , viewportForces = viewport
    , specificForces = IntDict.empty
    , start = dawg.root
    }

init : AutomatonGraph -> (Float, Float) -> (Model, Cmd Msg)
init dawg (w, h) =
  ( receiveDAWG dawg (w, h), Cmd.none )

updateNode : ( Float, Float ) -> NodeContext Entity Connection -> NodeContext Entity Connection
updateNode ( x, y ) nodeCtx =
  let
    nodeValue =
      nodeCtx.node.label
  in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateContextWithValue : NodeContext Entity Connection -> Entity -> NodeContext Entity Connection
updateContextWithValue nodeCtx value =
  let
    node =
      nodeCtx.node
  in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity Connection -> List Entity -> Graph Entity Connection
updateGraphWithList =
  let
    graphUpdater value =
      Maybe.map (\ctx -> updateContextWithValue ctx value)
  in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : (Float, Float) -> Msg -> Model -> Model
update offset_amount msg model =
  case msg of
    Tick ->
      let
        ( newState, list ) =
          Force.tick model.simulation <| List.map .label <| Graph.nodes model.graph
      in
        case model.drag of
          Nothing ->
            { model
              | graph = updateGraphWithList model.graph list
              , simulation = newState
            }

          Just { current, index } ->
            { model
              | graph =
                  Graph.update index
                  (Maybe.map (updateNode current))
                  (updateGraphWithList model.graph list)
              , simulation = newState
            }

    GraphUpdated dawg ->
      receiveDAWG dawg model.dimensions

    ViewportUpdated dim ->
      let
        viewport = viewportForces dim model.graph
      in
      { model
        | dimensions = dim
        , viewportForces = viewport
        , simulation = Force.simulation (model.basicForces ++ model.viewportForces)
      }

    DragStart index xy ->
      { model
      | drag = Just <| Drag (offset offset_amount xy) (offset offset_amount xy) index
      -- , simulation = Force.reheat model.simulation
      }

    DragAt xy ->
      case model.drag of
        Just { start, index } ->
          { model
            | drag = Just <| Drag start xy index
            , graph = Graph.update index (Maybe.map (updateNode xy)) model.graph
            -- , simulation = Force.reheat model.simulation
          }

        Nothing ->
          { model | drag = Nothing }

    DragEnd (x,y) ->
      case model.drag of
        Just { index } ->
          let
            sf =
              IntDict.insert index
                [ Force.towardsX [{ node = index, strength = 2.5, target = x }]
                , Force.towardsY [{ node = index, strength = 2.5, target = y }]
                ]
                model.specificForces
          in
            { model
              | drag = Nothing
              , graph = Graph.update index (Maybe.map (updateNode (x,y))) model.graph
              , specificForces = sf
              , simulation = Force.simulation (List.concat (IntDict.values sf))
            }

        Nothing ->
          { model | drag = Nothing }

offset : (Float, Float) -> (Float, Float) -> (Float, Float)
offset (offset_x, offset_y) (x, y) =
  (x - offset_x, y - offset_y)


subscriptions : (Float, Float) -> Model -> Sub Msg
subscriptions offset_amount model =
  case model.drag of
    Nothing ->
      -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
      -- to the rAF.
      if Force.isCompleted model.simulation then
        Sub.none

      else
        Browser.Events.onAnimationFrame (always Tick)

    Just _ ->
      Sub.batch
        [ Browser.Events.onMouseMove (Decode.map (.clientPos >> (offset offset_amount) >> DragAt) Mouse.eventDecoder)
        , Browser.Events.onMouseUp (Decode.map (.clientPos >> (offset offset_amount) >> DragEnd) Mouse.eventDecoder)
        , Browser.Events.onAnimationFrame (always Tick)
        ]


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
  Mouse.onDown (.clientPos >> DragStart index)

textChar : Char -> String
textChar ch =
  case ch of
    ' ' ->
      "└┘"
    _ ->
      String.fromChar ch

transitionToTextSpan : (Char, Int) -> Svg msg
transitionToTextSpan transition =
  case transition of
    (ch, 0) ->
      tspan
        [ fill <| Paint <| paletteColors.transition.nonFinal
        , strokeWidth 4
        , stroke <| Paint <| paletteColors.background
        ]
        [ text <| textChar ch
        ]
    (ch, _) ->
      tspan
        [ fontWeight FontWeightBolder
        , fill <| Paint <| paletteColors.transition.final
        , strokeWidth 4
        , stroke <| Paint <| paletteColors.background
        ]
        [ text <| textChar ch
        ]

connectionToSvgText : Connection -> List (Svg msg)
connectionToSvgText =
  Set.toList
  >> List.map transitionToTextSpan

paletteColors : { state : { normal : Color.Color, start : Color.Color, terminal : Color.Color }, edge : Color.Color, transition : { nonFinal : Color.Color, final : Color.Color }, background : Color.Color }
paletteColors =
  { state =
    { normal = Color.rgb255 0 222 255
    , start = Color.rgb255 0 35 135
    , terminal = Color.rgb255 0 123 167
    }
  , edge = Color.hsl 1 0 0.35
  , transition =
      { nonFinal = Color.hsl 1 0 0.25
      -- orange is from ≈31°-39° (←red, orange, yellow→).
      , final = Color.darkOrange
      }
  , background = Color.grey -- for painting a surrounding stroke
  }

linkElement : Graph Entity Connection -> Edge Connection -> Svg msg
linkElement graph edge =
  let
    source =
      Maybe.withDefault (Force.entity 0 { isTerminal = False, isFinal = False}) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

    target =
      Maybe.withDefault (Force.entity 0 { isTerminal = False, isFinal = False}) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    label = connectionToSvgText edge.label
    em_to_px em_value = font_size * em_value
    padding =
      let
        padding_em = 0.3

        -- width_em = padding_em + (toFloat <| Set.size edge.label)
        -- height_em = 1 + padding_em -- in em
      in
        ( padding_em |> em_to_px
        -- , width_em |> em_to_px
        -- , height_em |> em_to_px
        )
    font_size = 16.0 -- this is the default, if not otherwise set
    d_y = source.y - target.y
    d_x = source.x - target.x
    line_len = sqrt (d_x * d_x + d_y * d_y)
    m = d_y / d_x
    c = source.y - (m * source.x)
    y_value y = m * y + c
    midPoint =
      { x = (source.x + target.x) / 2
      , y = y_value <| (source.x + target.x) / 2
      }
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
    parametric_direction_vector =
      -- normalised
      { x = -(target.y - source.y) / line_len
      , y = -(target.x - source.x) / line_len
      }
    desired_length = 0.4 * line_len -- increase/decrease for a larger/smaller angle
    control_vector =
      { y = desired_length * parametric_direction_vector.y
      , x = -desired_length * parametric_direction_vector.x
      }
    control_point =
      { x = midPoint.x + control_vector.x
      , y = midPoint.y + control_vector.y
      }
    transition_coordinates =
      { x = midPoint.x + control_vector.x / 2
      , y = midPoint.y + control_vector.y / 2
      }
  in
    g
      []
      [
        -- line
        --   [ strokeWidth 3
        --   , stroke <| Paint <| paletteColors.edge
        --   , x1 source.x
        --   , y1 source.y
        --   , x2 target.x
        --   , y2 target.y
        --   , markerEnd "url(#arrowhead)"
        --   ]
        --   [ title [] [ text <| Automata.Data.connectionToString edge.label ] ]
        path
          [ strokeWidth 5
          , stroke <| Paint <| paletteColors.background
          , d <| "M " ++ String.fromFloat source.x ++ " " ++ String.fromFloat source.y ++
              " Q " ++ String.fromFloat control_point.x ++ " " ++ String.fromFloat control_point.y ++
              " " ++ String.fromFloat target.x ++ " " ++ String.fromFloat target.y
          , noFill
          ]
          [ title [] [ text <| Automata.Data.connectionToString edge.label ] ]
      , path
          [ strokeWidth 3
          , stroke <| Paint <| paletteColors.edge
          , d <| "M " ++ String.fromFloat source.x ++ " " ++ String.fromFloat source.y ++
              " Q " ++ String.fromFloat control_point.x ++ " " ++ String.fromFloat control_point.y ++
              " " ++ String.fromFloat target.x ++ " " ++ String.fromFloat target.y
          , markerEnd "url(#arrowhead)"
          , noFill
          ]
          [ title [] [ text <| Automata.Data.connectionToString edge.label ] ]
      -- , circle -- show where the control point is
      --     [ r 4
      --     , fill <| Paint <| paletteColors.edge
      --     , cx control_point.x
      --     , cy control_point.y
      --     ]
      --     []
      -- , rect
      --     [ x <| midPoint.x - (width / 2)
      --     , y <| midPoint.y - (height / 2)
      --     , Px.width width
      --     , Px.height height
      --     , fill <| Paint <| Color.rgba 0.0 0.5 1.0 0.5
      --     -- , stroke <| Paint <| Color.black
      --     , rx 5
      --     ]
      --     []
      , text_
          [ x <| transition_coordinates.x
          , y <| transition_coordinates.y + (padding / 2)
          , fontFamily ["sans-serif"]
          , fontSize font_size
          , fontWeight FontWeightNormal
          , textAnchor AnchorMiddle
          , alignmentBaseline AlignmentCentral
          , attribute "paint-order" "stroke fill markers"
          , cursor CursorDefault
          ]
          label
      ]


nodeElement : NodeId -> { a | id : NodeId, label : { b | x : Float, y : Float, value : { isTerminal : Bool, isFinal : Bool } } } -> Svg Msg
nodeElement start node =
  let
    radius =
      if node.id == start || node.label.value.isTerminal then
        9
      else
        7
    outerRadius = radius + 3.5
    color =
      if node.id == start then
        paletteColors.state.start
      else if node.label.value.isTerminal  then
        paletteColors.state.terminal
      else
        paletteColors.state.normal
  in
    g
      [ onMouseDown node.id
      , class ["state-node"]
      ]
      [ circle
          [ r radius
          -- , fill <| Paint color
          -- , stroke <| Paint color
          , strokeWidth 2
          , cx node.label.x
          , cy node.label.y
          ]
          []
       ,  if node.label.value.isTerminal then
            text_
              [ x <| node.label.x
              , y <| (node.label.y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , attribute "paint-order" "stroke fill markers"
              , cursor CursorDefault
              ]
              [ text "🎯" ]
          else if node.id == start then
            text_
              [ x <| node.label.x
              , y <| (node.label.y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , attribute "paint-order" "stroke fill markers"
              , cursor CursorDefault
              , fill <| Paint <| Color.grey
              ]
              [ text "⭐" ]
          else
            g [] []
      , title
          []
          [ text <|
              -- String.fromInt node.id
              "Drag to reposition\nClick to create new transition"
          ]
      ]
      -- ::  if node.label.value.isFinal || node.id == start then
      --       [ circle
      --           [ r outerRadius
      --           , fill <| Paint <| Color.rgba 0 0 0 0 -- transparent
      --           , stroke <| Paint color
      --           , strokeWidth 2.5
      --           , onMouseDown node.id
      --           , cx node.label.x
      --           , cy node.label.y
      --           ]
      --           [ title [] [ text <| String.fromInt node.id ] ]
      --       ]
      --     else
      --       []
      -- )

arrowheadMarker : Svg msg
arrowheadMarker =
  marker
    [ id "arrowhead"
    , viewBox 0 0 10 10
    , refX "15"
    , refY "5"
    , orient "auto-start-reverse"
    , markerWidth 5
    , markerHeight 5
    , fill <| Paint <| paletteColors.edge
    ]
    [ path
        [ d "M 0 0 L 10 5 L 0 10 z" ]
        []
    ]

view : Model -> Svg Msg
view model =
  svg
    [ viewBox 0 0 (Tuple.first model.dimensions) (Tuple.second model.dimensions) ]
    [ defs [] [ arrowheadMarker ]
    , Graph.edges model.graph
      |> List.map (linkElement model.graph)
      |> g [ class [ "links" ] ]
    , Graph.nodes model.graph
      |> List.map (nodeElement model.start)
      |> g [ class [ "nodes" ] ]
    ]


-- main : Program DAWGGraph Model Msg
-- main =
--     Browser.element
--         { init = init
--         , view = view
--         , update = \msg model -> ( update msg model, Cmd.none )
--         , subscriptions = subscriptions
--         }


{- {"delay": 5} -}
