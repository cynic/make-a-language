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
  , textAnchor, cursor, id, refX, refY, orient, d, markerEnd)
import TypedSvg.Attributes.InPx exposing
  ( cx, cy, r, strokeWidth, x1, x2, y1, y2, x, y, height, fontSize
  , markerWidth, markerHeight)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..), Cursor(..))
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
basicForces : Graph Entity Connection -> (Int, Int) -> List (Force.Force NodeId)
basicForces graph (width, height) =
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
          if n.id == 0 then
            Just { node = 0, strength = 0.1, target = 0 }
          else
            Nothing
        )
        (Graph.nodes graph)
  , Force.towardsY <|
      List.filterMap
        (\n ->
          if n.id == 0 then
            Just { node = n.id, strength = 0.8, target = toFloat (height // 2) }
          else
            Nothing
        )
        (Graph.nodes graph)
  ]

makeSimulation : (Float, Float) -> Graph Entity Connection -> Force.State NodeId
makeSimulation (w, h) graph =
  Force.simulation
    (basicForces graph (round w, round h) ++ viewportForces (w, h) graph)

toForceGraph : AutomatonGraph -> Graph Entity Connection
toForceGraph g =
  Graph.mapContexts initializeNode g.graph

receiveDAWG : AutomatonGraph -> (Float, Float) -> Model
receiveDAWG dawg (w, h) =
  let
    forceGraph = toForceGraph (dawg {- |> Debug.log "Received by ForceDirectedGraph" -} )
    basic = basicForces forceGraph (round w, round h)
    viewport = viewportForces (w, h) forceGraph
  in
    { drag = Nothing
    , graph = forceGraph
    , simulation = Force.simulation (basic ++ viewport)
    , dimensions = (w, h)
    , basicForces = basic
    , viewportForces = viewport
    , specificForces = IntDict.empty
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
          [ title [] [ text <| Automata.Data.connectionToString edge.label ] ]
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
      else if node.label.value.isFinal then
        Color.rgb255 255 255 8
      else if node.label.value.isTerminal then
        Color.rgb255 25 165 25
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
      ::  if node.label.value.isFinal || node.id == 0 then
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
