module ForceDirectedGraph exposing (..)

import Browser
import Browser.Events
import Color
import Force
import Graph exposing (Edge, Graph, NodeContext, NodeId)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import TypedSvg exposing
  (circle, g, line, svg, title, rect, text_, marker, path, defs, tspan)
import TypedSvg.Attributes exposing
  ( class, fill, stroke, viewBox, fontFamily, fontWeight, alignmentBaseline
  , textAnchor, cursor, id, refX, refY, orient, d, markerEnd)
import TypedSvg.Attributes.InPx as Px exposing
  ( cx, cy, r, strokeWidth, x1, x2, y1, y2, x, y, rx, width, height, fontSize
  , markerWidth, markerHeight)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..), Cursor(..))
import DAWG exposing (DAWGGraph, Node, Connection, DAWG)
import Html.Attributes exposing (attribute)
import Set
import IntDict

type Msg
  = DragStart NodeId ( Float, Float )
  | DragAt ( Float, Float )
  | DragEnd ( Float, Float )
  | Tick
  | GraphUpdated DAWG
  | ViewportUpdated (Float, Float)


type alias Model =
  { drag : Maybe Drag
  , graph : Graph Entity Connection
  , simulation : Force.State NodeId
  , dimensions : (Float, Float) -- (w,h) of svg element
  , basicForces : List (Force.Force NodeId) -- EXCLUDING the "center" force.
  , viewportForces : List (Force.Force NodeId)
  }


type alias Drag =
  { start : ( Float, Float )
  , current : ( Float, Float )
  , index : NodeId
  }


type alias Entity =
    Force.Entity NodeId { value : { isTerminal : Bool, isFinal : Bool } }


initializeNode : Node -> NodeContext Entity Connection
initializeNode ctx =
  { node =
    { label =
        Force.entity ctx.node.id
          { isTerminal = DAWG.isTerminalNode ctx
          , isFinal = IntDict.isEmpty ctx.outgoing
          }
    , id = ctx.node.id
    }
  , incoming = ctx.incoming
  , outgoing = ctx.outgoing
  }

viewportForces : (Float, Float) -> Graph Entity Connection -> List (Force.Force NodeId)
viewportForces (w, h) graph =
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

basicForces : Graph Entity Connection -> List (Force.Force NodeId)
basicForces graph =
  [
    Force.customLinks 2 <|
      List.map
        (\e ->
          { source = e.from
          , target = e.to
          , distance = 35.0 + 25.0 * toFloat (Set.size e.label)
          , strength = Just <| 0.4 * (toFloat <| Set.size e.label)
          }
        )
      (Graph.edges graph)
    -- Force.links <| List.map link <| Graph.edges graph
  , Force.manyBodyStrength -1000.0 <| List.map .id <| Graph.nodes graph
  -- , Force.manyBody <| List.map .id <| Graph.nodes graph
  , Force.towardsX <|
      List.filterMap
        (\n ->
          if n.id == 0 then
            Just { node = 0, strength = 0.15, target = 0 }
          else
            Nothing
        )
        (Graph.nodes graph)
  ]

makeSimulation : (Float, Float) -> Graph Entity Connection -> Force.State NodeId
makeSimulation (w, h) graph =
  Force.simulation
    (basicForces graph ++ viewportForces (w, h) graph)

toForceGraph : DAWGGraph -> Graph Entity Connection
toForceGraph g =
  Graph.mapContexts initializeNode g

receiveDAWG : DAWG -> (Float, Float) -> Model
receiveDAWG dawg (w, h) =
  let
    forceGraph = toForceGraph (DAWG.debugDAWG "Received by ForceDirectedGraph" dawg).graph |> \v -> Debug.log "\n" () |> \_ -> v
    basic = basicForces forceGraph
    viewport = viewportForces (w, h) forceGraph
  in
    { drag = Nothing
    , graph = forceGraph
    , simulation = Force.simulation (basic ++ viewport)
    , dimensions = (w, h)
    , basicForces = basic
    , viewportForces = viewport
    }

init : DAWG -> (Float, Float) -> (Model, Cmd Msg)
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
      { model | drag = Just <| Drag (offset offset_amount xy) (offset offset_amount xy) index, simulation = Force.reheat model.simulation }

    DragAt xy ->
      case model.drag of
        Just { start, index } ->
          { model
            | drag = Just <| Drag start xy index
            , graph = Graph.update index (Maybe.map (updateNode xy)) model.graph
            , simulation = Force.reheat model.simulation
          }

        Nothing ->
          { model | drag = Nothing }

    DragEnd xy ->
      case model.drag of
        Just { index } ->
          { model
            | drag = Nothing
            , graph = Graph.update index (Maybe.map (updateNode xy)) model.graph
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
      tspan [] [ text <| textChar ch ]
    (ch, _) ->
      tspan
        [ fontWeight FontWeightBolder
        , fill <| Paint <| Color.darkOrange
        ]
        [ text <| textChar ch ]

connectionToSvgText : Connection -> List (Svg msg)
connectionToSvgText =
  Set.toList
  >> List.map transitionToTextSpan

linkElement : Graph Entity Connection -> Edge Connection -> Svg msg
linkElement graph edge =
  let
    source =
      Maybe.withDefault (Force.entity 0 { isTerminal = False, isFinal = False}) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

    target =
      Maybe.withDefault (Force.entity 0 { isTerminal = False, isFinal = False}) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    label = connectionToSvgText edge.label
    em_to_px em_value = font_size * em_value
    (padding, width, height) =
      let
        padding_em = 0.3

        width_em = padding_em + (toFloat <| Set.size edge.label)
        height_em = 1 + padding_em -- in em
      in
        ( padding_em |> em_to_px
        , width_em |> em_to_px
        , height_em |> em_to_px
        )
    font_size = 16.0 -- this is the default, if not otherwise set
    m = (source.y - target.y) / (source.x - target.x)
    c = source.y - (m * source.x)
    y_value y = m * y + c
    midPoint =
      { x = (source.x + target.x) / 2
      , y = y_value <| (source.x + target.x) / 2
      }
  in
    g
      []
      [ line
          [ strokeWidth 3
          , stroke <| Paint <| Color.rgba 0 0.3 1.0 0.3
          , x1 source.x
          , y1 source.y
          , x2 target.x
          , y2 target.y
          , markerEnd "url(#arrowhead)"
          ]
          [ title [] [ text <| DAWG.connectionToString edge.label ] ]
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
          [ x <| midPoint.x
          , y <| midPoint.y + (padding / 2)
          , fontFamily ["sans-serif"]
          , fontSize font_size
          , fontWeight FontWeightNormal
          , textAnchor AnchorMiddle
          , alignmentBaseline AlignmentCentral
          , fill <| Paint <| Color.black
          , strokeWidth 3
          , stroke <| Paint <| Color.grey
          , attribute "paint-order" "stroke fill markers"
          , cursor CursorDefault
          ]
          label
      ]


nodeElement : { a | id : NodeId, label : { b | x : Float, y : Float, value : { isTerminal : Bool, isFinal : Bool } } } -> Svg Msg
nodeElement node =
  let
    radius = 8
    outerRadius = radius + 3.5
    color =
      if node.id == 0 then
        Color.rgb 0.8 0.0 0.7
      else if node.label.value.isTerminal then
        Color.rgb255 8 255 8
      else
        Color.black
  in
    g
      []
      ( circle
          [ r radius
          , fill <| Paint color
          , stroke <| Paint color
          , strokeWidth 1
          , onMouseDown node.id
          , cx node.label.x
          , cy node.label.y
          ]
          [ title [] [ text <| String.fromInt node.id ] ]
      ::  if node.label.value.isTerminal || node.id == 0 then
            [ circle
                [ r outerRadius
                , fill <| Paint <| Color.rgba 0 0 0 0
                , stroke <| Paint Color.darkCharcoal
                , strokeWidth 2.5
                , onMouseDown node.id
                , cx node.label.x
                , cy node.label.y
                ]
                [ title [] [ text <| String.fromInt node.id ] ]
            ]
          else
            []
      )

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
    , fill <| Paint <| Color.rgba 0 0.3 1.0 0.5
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
      |> List.map nodeElement
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
