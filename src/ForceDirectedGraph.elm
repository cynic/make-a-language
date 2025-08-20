module ForceDirectedGraph exposing (..)
import Browser.Events
import Color
import Force
import Graph exposing (Edge, Graph, NodeContext, NodeId)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Json.Decode as Decode
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
  , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..))
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

type Msg
  = DragStart NodeId
  | DragEnd
  | Tick
  | ViewportUpdated (Float, Float) (Float, Float)
  | MouseMove Float Float
  | Pan Float Float
  | Zoom Float (Float, Float)
  | ResetView
  | SelectNode NodeId
  | ToggleSelectedTransition AcceptVia
  | SetMouseOver
  | SetMouseOut
  | CreateOrUpdateLinkTo NodeId -- this is for an already-existing node.
  | CreateNewNodeAt ( Float, Float )
  | Escape -- the universal "No! Go Back!" key & command
  | Confirm -- the universal "Yeah! Let's Go!" key & command
  | EditTransition NodeId NodeId Connection
  | Reheat
  | Undo
  | Redo
  | StartSplit NodeId
  | Load String
  | Run
  | Step
  | Stop
  -- to add: Execute, Step, Stop
  -- also: when I make a change to the graph, set .execution to Nothing!

-- For zooming, I take the approach set out at https://www.petercollingridge.co.uk/tutorials/svg/interactive/pan-and-zoom/

type UserOperation
  = Splitting Split -- split a node into two, so I can work on the parts separately
  | Dragging NodeId -- move a node around (visually)
  | ModifyingGraph GraphModification
  | AlteringConnection ConnectionAlteration 
type alias Model =
  { currentOperation : Maybe UserOperation
  , userGraph : AutomatonGraph Entity
  , simulation : Force.State NodeId
  , dimensions : (Float, Float) -- (w,h) of svg element
  , frameOffset : (Float, Float) -- (x,y) of the offset of this frame
  , basicForces : List (Force.Force NodeId) -- EXCLUDING the "center" force.
  , viewportForces : List (Force.Force NodeId)
  , specificForces : IntDict.IntDict (List (Force.Force NodeId))
  , zoom : ( Float, ( Float, Float ) ) -- ( zoom-factor, zoom-center-coordinates )
  , pan : (Float, Float) -- panning offset, x and y
  , mouseCoords : ( Float, Float )
  , mouseIsHere : Bool
  , undoBuffer : List (AutomatonGraph Entity)
  , redoBuffer : List (AutomatonGraph Entity)
  , disconnectedNodes : Set NodeId
  , execution : Maybe ExecutionResult
  }

{-| The buffer from the edges within which panning occurs -}
panBuffer : Float
panBuffer = 40

------------------------------------
-- BEGIN :: Copied & adapted from Force.elm
------------------------------------
{-| This is a convenience function for wrapping data up as Entities. The initial position of entities is arranged
in a [phylotaxic pattern](https://elm-visualization.netlify.app/Petals/). Goes well with `List.indexedMap`.
-}
entity : Int -> NodeEffect -> Entity
entity index v =
  let
    initialRadius = 10
    initialAngle =
        pi * (3 - sqrt 5)
    radius =
        sqrt (0.5 + toFloat index) * initialRadius

    angle =
        toFloat index * initialAngle
  in
    { x = radius * cos angle
    , y = radius * sin angle
    , vx = 0.0
    , vy = 0.0
    , id = index
    , effect = v
    }
------------------------------------
-- END :: Copied & adapted from Force.elm
------------------------------------

initializeNode : Node a -> NodeContext Entity Connection
initializeNode ctx =
  { node =
    { label =
        entity ctx.node.id NoEffect
    , id = ctx.node.id
    }
  , incoming = ctx.incoming
  , outgoing = ctx.outgoing
  }

canExecute : Model -> Bool
canExecute model =
  List.isEmpty model.undoBuffer

getExecutionResult : Model -> Maybe ExecutionResult
getExecutionResult = .execution

viewportForces : (Float, Float) -> Graph Entity Connection -> List (Force.Force NodeId)
viewportForces (w, h) _ =
  [ Force.center (w / 2) (h / 2)
  ]

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
          (\(k, v) forces ->
            { source = ctx.node.id
            , target = k
            , distance = 10 + 25.0 * toFloat (AutoSet.size v) -- 35-40 seems like a good distance
            , strength = Just 0.7 -- * (toFloat <| Set.size v)
            } :: forces
          )
          acc
    )
    []
    graph
  |> Force.customLinks 3

basicForces : AutomatonGraph Entity -> Int -> List (Force.Force NodeId)
basicForces g height =
  [ makeLinkForces g.graph -- the springs
  , Force.manyBodyStrength -2000.0 (List.map .id <| Graph.nodes g.graph) -- the repulsion
  , Force.towardsX <|
      List.filterMap
        (\n ->
          if n.id == g.root then
            Just { node = g.root, strength = 0.1, target = 0 }
          else
            Nothing
        )
        (Graph.nodes g.graph)
  , Force.towardsY <|
      List.filterMap
        (\n ->
          if n.id == g.root then
            Just { node = n.id, strength = 0.8, target = toFloat (height // 2) }
          else
            Nothing
        )
        (Graph.nodes g.graph)
  ]  

makeSimulation : (Float, Float) -> AutomatonGraph Entity -> Force.State NodeId
makeSimulation (w, h) g =
  Force.simulation
    (basicForces g (round h) ++ viewportForces (w, h) g.graph)

toForceGraph : AutomatonGraph a -> AutomatonGraph Entity
toForceGraph g =
  { graph = Graph.mapContexts initializeNode g.graph
  , root = g.root
  , maxId = g.maxId
  }

automatonGraphToModel : (Float, Float) -> (Float, Float) -> AutomatonGraph a -> Model
automatonGraphToModel frameOffset (w, h) g =
  let
    forceGraph = toForceGraph g
    basic = basicForces forceGraph (round h)
    viewport = viewportForces (w, h) forceGraph.graph
    nodes = Graph.nodes forceGraph.graph
    simulation =
      Force.simulation (basic ++ viewport)
    shiftedNodes =
      Force.computeSimulation
        simulation
        (List.map .label nodes)
    resultingGraph =
      -- no changes to node-ids are made; only the spatial positions change.
      { forceGraph | graph = updateGraphWithList forceGraph.graph shiftedNodes }
  in
    { currentOperation = Nothing
    , userGraph = resultingGraph
    , simulation = simulation
    , dimensions = (w, h)
    , frameOffset = frameOffset
    , basicForces = basic
    , viewportForces = viewport
    , specificForces = IntDict.empty
    , zoom = ( 1.0, ( w/2, h/2 ) )
    , mouseCoords = ( w/2, h/2 )
    , pan = ( 0, 0)
    , mouseIsHere = False
    , undoBuffer = []
    , redoBuffer = []
    , disconnectedNodes = Set.empty
    , execution = Nothing
    }

init : (Float, Float) -> (Float, Float) -> Model
init frameOffset (w, h) =
  automatonGraphToModel frameOffset (w, h) (DFA.empty { effect = NoEffect } |> toAutomatonGraph)

updateNode : ( Float, Float ) -> ( Float, Float ) -> NodeContext Entity Connection -> NodeContext Entity Connection
updateNode (offsetX, offsetY) (x, y) nodeCtx =
  let
    nodeValue =
      nodeCtx.node.label
    new_x = x + offsetX
    new_y = y + offsetY
  in
    updateContextWithValue
      nodeCtx
      { nodeValue
        | x = if isNaN new_x then nodeCtx.node.label.x + offsetX else new_x
        , y = if isNaN new_y then nodeCtx.node.label.y + offsetY else new_y
      }


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

{-| What is the amount to pan by, given the x-coordinate of the mouse? -}
xPanAt : Model -> Float -> Float
xPanAt model x =
  if x >= ( Tuple.first model.dimensions - panBuffer ) then
    1
  else if x <= panBuffer then
    -1
  else
    0

yPanAt : Model -> Float -> Float
yPanAt model y =
  if y >= ( Tuple.second (model.dimensions) - (panBuffer) ) then
    1
  else if y <= panBuffer then
    -1
  else
    0

identifyDisconnectedNodes : AutomatonGraph a -> Set NodeId
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

{-| Obtain the shortest path to a specified NodeId.

If no such path exists, then return Nothing.  Can specify an
`accept` function for additional checks, if desired.
-}
createPathTo : NodeId -> List NodeId -> (RequestedChangePath -> Bool) -> Graph Entity Connection -> NodeId -> Maybe RequestedChangePath
createPathTo target waypoints accept graph start =
  let
    -- avoid backtracking; so, we have a "seen" set.
    findPath : Set NodeId -> NodeId -> RequestedChangePath -> Maybe RequestedChangePath
    findPath seen current acc =
      if Set.member current seen && current /= target then
        Nothing --|> Debug.log "Already seen this; must be on a recursive path; backtracking"
      else
        Graph.get ({-Debug.log "current"-} current) graph
        |> Maybe.andThen
          (\node ->
            let
              nodeIsStart = node.node.id == start
              seenAllNecessaryNodes =
                List.all
                  (\id ->
                    id == current ||
                    Set.member id seen --|> Debug.log ("Is #" ++ String.fromInt id ++ " the current node or in " ++ Debug.toString seen)
                  )
                  (target::waypoints)
              nodeIsAccepted = accept acc
            in
              if nodeIsStart && seenAllNecessaryNodes && nodeIsAccepted then
                  Just acc
                -- else
                --   -- if I don't encounter `target` and all specified waypoints
                --   -- on the way, then this path is useless to me.
                --   Nothing |> Debug.log "Path failed checks"
              else
                node.incoming
                |> IntDict.toList
                -- |> Debug.log ("Incoming nodes for #" ++ String.fromInt current ++ " are")
                |> List.filterMap
                  (\(k, v) ->
                    if current /= k then
                      AutoSet.toList v
                      |> List.head
                      |> Maybe.andThen
                        (\t ->
                          findPath
                            (Set.insert current seen)
                            ({- Debug.log "going to look at" -} k)
                            ({- Debug.log "current path" -} (t::acc))
                        )
                    else
                      Nothing -- ignore purely recursive links; they won't get us anywhere.
                  )
                |> List.minimumBy List.length
          )
  in
    findPath Set.empty target []
    -- |> Debug.log ("[createPathTo] Asked to find path to #" ++ String.fromInt target ++ ", found")

{-| Obtain the NodeId at the end of the specified path.

If the path is invalid for the graph context, then return Nothing.
-}
followPathTo : RequestedChangePath -> AutomatonGraph a -> Maybe NodeId
followPathTo path g =
  let
    followPath : NodeId -> RequestedChangePath -> Maybe NodeId
    followPath current remaining =
      case remaining of
        [] ->
          Just current
        h::t ->
          Graph.get current g.graph
          |> Maybe.andThen
            (\node ->
              IntDict.toList node.outgoing
              |> List.filter (\(_, conn) -> AutoSet.member h conn)
              -- here, I am treating the graph as if it is an NFA.
              -- And that is because indeed, a user may well just treat it as an NFA
              -- and randomly create two or more paths!!  So, we need to explore each
              -- path and see which ones might be valid.
              |> List.filterMap (\(k, _) -> followPath k t)
              |> List.head
              -- |> Maybe.andThen (\(k, _) -> followPath k t)
            )
  in
    followPath g.root path
    -- |> Debug.log ("[followPathTo] Followed " ++ Debug.toString path ++ " to arrive at")

path_for_removal : Graph Entity Connection -> NodeId -> NodeId -> NodeId -> Maybe (RequestedChangePath, RequestedChangePath)
path_for_removal graph start source destination =
  -- this is called when there is a link between the source and destination,
  -- and it must be removed.  As a result, other nodes might be disconnected.
  Maybe.andThen
    (\dest ->
      let
        acceptFunction =
          -- check that there is a link between source & dest, and that
          -- the destination can be obtained via the last transition in 1 hop
          -- (unless we are dealing with recursion).
          -- Check the recursive case first.
          if source == destination then
            \_ -> True -- |> Debug.log "No accept-check, this is recursive" -- the checks in createPathTo should already cover this.
          else
            \p ->
              -- we reverse because we want to check the LAST link in the chain for 1-hop
              case List.reverse p of
                [] ->  -- you're not recursive, so there must be AT LEAST one link!
                  False
                  -- |> Debug.log "Failed accept: no links in path, but not recursive"
                h::_ ->
                  IntDict.get source dest.incoming
                  -- |> Debug.log "Is there a 1-hop link?"
                  |> Maybe.map (AutoSet.member ({- Debug.log "Checking for transition" -} h))
                  |> Maybe.withDefault False
                  -- |> Debug.log ("Is there a 1-link hop from #" ++ String.fromInt destination ++ " to #" ++ String.fromInt source)
        path =
          createPathTo destination [source] acceptFunction graph start
      in
        Maybe.map
          (\p ->
            if source == destination then
              ( p, p )
            else
              case List.reverse p of
                [] -> -- special case, this is the root
                  ( p, p )
                _::revPath ->
                  ( List.reverse revPath, p )
          )
          path
    )
    (Graph.get destination (graph {- |> debugGraph "getting userchange-data from" -}))

{-| Create a DFA consisting of all paths ending at the specified transition.
-}
wordsEndingAt : NodeId -> AutomatonGraph a -> AutomatonGraph a
wordsEndingAt nodeId g =
  let
    nodes =
      Graph.guidedBfs
        Graph.alongIncomingEdges
        (Graph.ignorePath (\context acc ->
          context.node.id :: acc
        ))
        [nodeId]
        []
        (Graph.update g.root (Maybe.map (\node -> { node | incoming = IntDict.empty })) g.graph)
      |> Tuple.first
    induced =
      Graph.inducedSubgraph nodes g.graph
  in
    graphToAutomatonGraph g.root induced

newnode_graphchange : NodeId -> Float -> Float -> Connection -> AutomatonGraph Entity -> AutomatonGraph Entity
newnode_graphchange src x y conn g =
  { g
    | graph =
        Graph.insert
          { node =
            { label =
                let
                  initial = entity (g.maxId + 1) NoEffect
                in
                  { initial | x = x, y = y }
            , id = g.maxId + 1
            }
          , incoming = IntDict.singleton src conn
          , outgoing = IntDict.empty
          }
          g.graph
    , maxId = g.maxId + 1
  }
  -- |> debugAutomatonGraph "[create_union_graphchange] updated graph"

updateLink_graphchange : NodeId -> NodeId -> Connection -> AutomatonGraph Entity -> AutomatonGraph Entity
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

removeLink_graphchange : NodeId -> NodeId -> AutomatonGraph Entity -> AutomatonGraph Entity
removeLink_graphchange src dest g =
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

splitNode : NodeContext Entity Connection -> Connection -> Connection -> Model -> AutomatonGraph Entity
splitNode node left _ model = -- turns out that "right" isn't needed. Hmmm!!!?
  let
    (recursive, nonRecursive) =
      node.incoming
      |> IntDict.partition (\k _ -> k == node.node.id)
    recursive_connection =
      recursive
      |> IntDict.values
      |> List.foldl AutoSet.union (AutoSet.empty transitionToString)
    ( leftConnections, rightConnections ) =
      nonRecursive
      |> IntDict.foldl
          (\k conn (l, r) ->
            -- classify each transition into 'left' or 'right'.
            AutoSet.foldl
              (\transition (l_, r_) ->
                if AutoSet.member transition left then
                  ( IntDict.update k
                      ( Maybe.map (AutoSet.insert transition)
                        >> Maybe.orElseLazy (\() -> Just <| AutoSet.singleton transitionToString transition)
                      )
                      l_
                  , r_
                  )
                else
                  ( l_
                  , IntDict.update k
                    ( Maybe.map (AutoSet.insert transition)
                      >> Maybe.orElseLazy (\() -> Just <| AutoSet.singleton transitionToString transition)
                    )
                    r_
                  )
              )
              (l, r)
              conn
          )
          (IntDict.empty, IntDict.empty)
    ag = model.userGraph
    newUserGraph =
      { ag
        | graph =
            ag.graph
            |> Graph.insert
              { node =
                  { label = entity (ag.maxId + 1) NoEffect
                  , id = ag.maxId + 1
                  }
              , incoming =
                  leftConnections
                  |> IntDict.insert (ag.maxId + 1) recursive_connection
              , outgoing =
                  node.outgoing
                  |> IntDict.insert (ag.maxId + 1) recursive_connection
              }
            |> Graph.update node.node.id
              (\_ ->
                Just <|
                  { node
                    | incoming =
                        rightConnections
                        |> IntDict.insert node.node.id recursive_connection
                  }
              )
        , maxId = ag.maxId + 1
      }
  in
    newUserGraph

mapCorrespondingPair : (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
mapCorrespondingPair f (a1, a2) (b1, b2) =
  (f a1 b1, f a2 b2)

update : Msg -> Model -> Model
update msg model =
  case msg of
    Tick ->
      let
        g = model.userGraph
        ( newState, list ) =
          Force.tick model.simulation <| List.map .label <| Graph.nodes g.graph
      in
        { model
          | userGraph = { g | graph = updateGraphWithList model.userGraph.graph list }
          , simulation = newState
        }

    ViewportUpdated frameOffset dim ->
      let
        viewport = viewportForces dim model.userGraph.graph
      in
      { model
        | dimensions = dim
        , frameOffset = frameOffset
        , viewportForces = viewport
        , simulation = Force.simulation (model.basicForces ++ model.viewportForces)
      }

    DragStart nodeId ->
      { model
      | currentOperation = Just <| Dragging nodeId
      -- , simulation = Force.reheat model.simulation
      }

    DragEnd ->
      case model.currentOperation of
        Just (Dragging nodeId) ->
          let
            ( offsetX, offsetY ) =
              mapCorrespondingPair (+) model.pan model.frameOffset
            ( x, y ) =
              model.mouseCoords
            nearby =
              nearby_nodes nearby_node_repulsionDistance model
              -- |> Debug.log "Nearby nodes at end of drag"
            sf =
              IntDict.insert nodeId
                [ Force.towardsX [{ node = nodeId, strength = 2, target = x + offsetX }]
                , Force.towardsY [{ node = nodeId, strength = 2, target = y + offsetY }]
                , Force.customLinks 2
                    ( List.map
                        (\node ->
                            { source = nodeId
                            , target = node.id
                            -- in other words, 8 radii will separate the centers.
                            -- This should be sufficient for directional arrows to show up correctly.
                            , distance = nodeRadius * 10
                            , strength = Just 1
                            }
                        )
                        nearby
                    )
                ]
                model.specificForces
            g = model.userGraph
          in
            { model
              | currentOperation = Nothing
              , userGraph =
                  { g
                    | graph =
                        Graph.update nodeId (Maybe.map (updateNode (x,y) model.pan >> Debug.log "Dragging ended with updated node")) g.graph
                  }
              , specificForces = sf
              , simulation = Force.simulation (List.concat (IntDict.values sf))
            }

        _ ->
          model

    Zoom amount ( x, y ) ->
      let
        zoomAmount = if amount < 0 then 0.05 else -0.05
        newZoom = clamp 0.5 2.5 (Tuple.first model.zoom + zoomAmount)
      in
        if newZoom == Tuple.first model.zoom then
          model
        else
          { model | zoom = ( newZoom, ( x, y ) ) }

    ResetView ->
      { model | zoom = ( 1.0, Tuple.mapBoth (\x -> x/2) (\y -> y/2) model.dimensions ), pan = ( 0, 0 ) }

    MouseMove x y ->
      let
        ug = model.userGraph
      in
        { model
          | mouseCoords = (x, y) |> Debug.log "Set mouse-coords"
          , userGraph =
              case model.currentOperation of
                Just (Dragging nodeId) ->
                  { ug
                  | graph =
                      Graph.update nodeId
                        (Maybe.map
                          (\ctx ->
                            let
                              node = ctx.node
                              l = node.label
                              ( node_x, node_y ) =
                                --mapCorrespondingPair (+) model.pan ( x, y )
                                ( x, y )
                            in
                              { ctx
                              | node =
                                { node
                                | label = { l | x = node_x, y = node_y }
                                }
                              }
                          )
                        )
                        model.userGraph.graph
                  }
                _ ->
                  model.userGraph
        }
    
    Pan xAmount yAmount ->
      let
        ( xPan, yPan ) = model.pan
      in
      { model
      | pan =
          case model.currentOperation of
            Just (ModifyingGraph {dest}) ->
              case dest of
                NoDestination ->
                  ( xPan + xAmount, yPan + yAmount )
                _ ->
                  model.pan
            Nothing ->
              ( xPan + xAmount, yPan + yAmount )
            Just (Dragging _) ->
              ( xPan + xAmount, yPan + yAmount )
            Just (AlteringConnection _) ->
              model.pan
            Just (Splitting _) ->
              model.pan
      }

    SelectNode index ->
      { model | currentOperation = Just <| ModifyingGraph <| GraphModification index NoDestination (AutoSet.empty transitionToString) }

    SetMouseOver ->
      { model | mouseIsHere = True }

    SetMouseOut ->
      { model | mouseIsHere = False }

    CreateOrUpdateLinkTo dest ->
      case model.currentOperation of
        Just (ModifyingGraph { source }) ->
          let
            transitions =
              Graph.get dest model.userGraph.graph
              |> Maybe.map (\node ->
                case IntDict.get source node.incoming of
                  Just conn ->
                    conn
                  Nothing -> -- no link to this node, at present.
                    AutoSet.empty transitionToString
              )
              |> Maybe.withDefault (AutoSet.empty transitionToString)
          in
            { model
              | currentOperation =
                  Just <| ModifyingGraph <| GraphModification source (ExistingNode dest) transitions
            }
        _ ->
          model

    CreateNewNodeAt ( x, y ) ->
      case model.currentOperation of
        Just (ModifyingGraph { source, transitions }) ->
          { model
            | currentOperation =
                Just <| ModifyingGraph <| GraphModification source (NewNode ( x, y )) transitions
          }
        _ ->
          model

    Escape ->
      let
        escapery =
          case model.currentOperation of
            Just (ModifyingGraph { source, dest, transitions }) ->
              case dest of
                NoDestination ->
                  -- I must be escaping from something earlier.
                  { model | currentOperation = Nothing }
                EditingTransitionTo _ ->
                  -- back off by one level
                  { model
                    | currentOperation =
                        Just <|
                          ModifyingGraph
                            { source = source
                            , dest = NoDestination
                            , transitions = AutoSet.empty transitionToString
                            }
                  }
                _ ->
                  { model
                    | currentOperation =
                        Just <| ModifyingGraph <| GraphModification source NoDestination transitions
                  }
            Just (Splitting _) ->
              { model | currentOperation = Nothing }
            Just (AlteringConnection { source, dest, transitions }) ->
              { model | currentOperation = Nothing }
            Nothing ->
              model
            Just (Dragging _) ->
              -- stop dragging.
              { model | currentOperation = Nothing }
      in
      -- ooh!  What are we "escaping" from, though?
        escapery

    Confirm ->
      -- What am I confirming?
      let
        commit_change : AutomatonGraph Entity -> Model -> Model
        commit_change updatedGraph model_ =
          let
            basic =
              basicForces updatedGraph (round <| Tuple.second model_.dimensions)
            viewport =
              viewportForces model_.dimensions updatedGraph.graph
          in
            { model_
            | currentOperation = Nothing
            , userGraph = updatedGraph
                    -- NOTE ⬇ WELL! This isn't a typo!
            , undoBuffer = model.userGraph :: model_.undoBuffer
            , redoBuffer = [] -- when we make a new change, the redo-buffer disappears; we're not storing a tree!
            , basicForces = basic
            , viewportForces = viewport
            , simulation = Force.simulation (basic ++ viewport)
            , disconnectedNodes =
                identifyDisconnectedNodes updatedGraph
            , execution = Nothing
            }
        
        createNewNode : NodeId -> Connection -> Float -> Float -> Model
        createNewNode src conn x y =
          newnode_graphchange src x y conn model.userGraph
          |> \newGraph -> commit_change newGraph model

        updateExistingNode src dest conn =
          updateLink_graphchange src dest conn model.userGraph
          |> \newGraph -> commit_change newGraph model

        removeLink : NodeId -> NodeId -> Model
        removeLink src dest =
          removeLink_graphchange src dest model.userGraph
          |> \newGraph -> commit_change newGraph model

      in
        case model.currentOperation of
          Just (ModifyingGraph { source, dest, transitions }) ->
            case dest of
              ( NewNode ( x, y ) ) ->
                -- create a totally new node, never before seen!
                createNewNode source transitions x y
              ( ExistingNode destination ) ->
                if AutoSet.isEmpty transitions then
                  removeLink source destination
                else
                  updateExistingNode source destination transitions
              ( EditingTransitionTo destination ) ->
                if AutoSet.isEmpty transitions then
                  removeLink source destination
                else
                  updateExistingNode source destination transitions
              ( NoDestination ) ->
                -- ??? Nothing for me to do!  The user is just pressing Enter because… uh… eh, who knows?
                model
          Just (AlteringConnection { source, dest, transitions }) ->
                if AutoSet.isEmpty transitions then
                  removeLink source dest
                else
                  updateExistingNode source dest transitions
          Just (Splitting { to_split, left, right }) ->
            if AutoSet.isEmpty left || AutoSet.isEmpty right then
              { model | currentOperation = Nothing }
            else
              Graph.get to_split model.userGraph.graph
              |> Maybe.map (\node ->
                splitNode node left right model
                |> \g -> commit_change g model
              )
              |> Maybe.withDefault model
          Nothing -> -- I'm not in an active operation. But do I have changes to confirm?
            case model.undoBuffer of
              [] -> -- no changes are proposed, so…
                model -- …there is nothing for me to do!
              _ ->
                confirmChanges model
          Just (Dragging _) ->
            model -- confirmation does nothing for this (visual) operation.

    ToggleSelectedTransition acceptCondition ->
      let
        alterTransitions transitions =
          if AutoSet.member (acceptCondition, 0) transitions then
            AutoSet.remove (acceptCondition, 0) transitions
            |> AutoSet.insert (acceptCondition, 1)
          else if AutoSet.member (acceptCondition, 1) transitions then
            AutoSet.remove (acceptCondition, 1) transitions
          else
            AutoSet.insert (acceptCondition, 0) transitions
      in
        case model.currentOperation of
          Just (ModifyingGraph ({ transitions } as mod)) ->
            { model
              | currentOperation =
                  Just <| ModifyingGraph <|
                    { mod
                      | transitions = alterTransitions transitions
                    }
            }
          Just (AlteringConnection ({ transitions } as mod)) ->
            { model
              | currentOperation =
                  Just <| AlteringConnection <|
                    { mod
                      | transitions = alterTransitions transitions
                    }
            }
          Just (Splitting { to_split, left, right }) ->
            let
              onLeft_0 = AutoSet.member (acceptCondition, 0) left
              onLeft_1 = AutoSet.member (acceptCondition, 1) left
              onRight_0 = AutoSet.member (acceptCondition, 0) right
              onRight_1 = AutoSet.member (acceptCondition, 1) right
              pushToRight t =
                ( AutoSet.remove t left, AutoSet.insert t right )
              pushToLeft t =
                ( AutoSet.insert t left, AutoSet.remove t right )
              ( newLeft, newRight ) =
                if onLeft_0 && onLeft_1 then
                  -- push the non-terminal to the right.
                  pushToRight (acceptCondition, 0)
                else if onLeft_1 then
                  -- push the terminal to the right
                  pushToRight (acceptCondition, 1)
                else if onRight_0 && onRight_1 then
                  -- push the non-terminal to the left
                  pushToLeft (acceptCondition, 0)
                else if onRight_1 then
                  pushToLeft (acceptCondition, 1)
                else if onLeft_0 then
                  pushToRight (acceptCondition, 0)
                else if onRight_0 then
                  pushToLeft (acceptCondition, 0)
                else
                  ( left, right )
            in
              { model
                | currentOperation =
                    Just <| Splitting <| Split to_split newLeft newRight
              }
          _ ->
            model

    EditTransition src dest conn ->
      -- … and this will take us to much same place that
      -- UpdateOrCreateLinkTo took us.  But the difference
      -- is that we indicate an edit via EditingTransitionTo,
      -- rather than ExistingNode.  This indicates that if we
      -- escape out of it, we clear source & dest & transitions
      -- all at once.
      { model
        | currentOperation =
            Just <| AlteringConnection <| ConnectionAlteration src dest conn
      }

    Reheat ->
      -- If I'm not doing anything else, permit auto-layout
      case model.currentOperation of
        Nothing ->
          { model
          | simulation = Force.simulation (model.basicForces ++ model.viewportForces)
          , specificForces = IntDict.empty -- cancel moves that were made before
          }
        _ ->
          model

    StartSplit nodeId ->
      Graph.get nodeId model.userGraph.graph
      |> Maybe.map
        (\node ->
          { model
            | currentOperation =
                Just <| Splitting <|
                  Split
                    node.node.id
                    ( IntDict.foldl
                        (\k v acc ->
                            if k /= node.node.id then
                              AutoSet.union v acc
                            else
                              acc
                        )
                        (AutoSet.empty transitionToString)
                        node.incoming
                    )
                    (AutoSet.empty transitionToString)
          }
        )
      |> Maybe.withDefault model

    Undo ->
      case (model.currentOperation, model.undoBuffer) of
        (_, []) ->
          model
        (Just _, _) ->
          model -- do not permit undo/redo while I'm performing any operation.
        (Nothing, h::t) ->
          { model
            | undoBuffer = t
            , redoBuffer = model.userGraph :: model.redoBuffer
            , userGraph = h
            , disconnectedNodes = identifyDisconnectedNodes h
          }

    Redo ->
      case (model.currentOperation, model.redoBuffer) of
        (_, []) ->
          model
        (Just _, _) ->
          model -- do not permit undo/redo while I'm performing any operation.
        (Nothing, h::t) ->
          { model
            | redoBuffer = t
            , undoBuffer = model.userGraph :: model.undoBuffer
            , userGraph = h
            , disconnectedNodes = identifyDisconnectedNodes h
          }
    
    Load s ->
      -- first, can we?
      case model.undoBuffer of
        [] ->
          { model | execution = Just <| DFA.stepThroughInitial s model.userGraph }
        _ ->
          model -- nothing to do!
    
    Run ->
      { model
        | execution = Maybe.map (DFA.run model.userGraph) model.execution
      }

    Step ->
      { model
        | execution = Maybe.map (DFA.step model.userGraph) model.execution
      }

    Stop ->
      { model | execution = Nothing }

offset : (Float, Float) -> (Float, Float) -> (Float, Float)
offset (offset_x, offset_y) (x, y) =
  (x - offset_x, y - offset_y)

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    ( xCoord, yCoord ) = model.mouseCoords -- |> Debug.log "Mouse coords"
    panSubscription =
      let
        xPan = xPanAt model xCoord
        yPan = yPanAt model yCoord
      in
        if model.mouseIsHere && (xPan /= 0 || yPan /= 0) then
          Time.every 20 (\_ -> Pan xPan yPan |> Debug.log "Pan request")
        else
          Sub.none
    keyboardSubscription =
      if model.mouseIsHere then
        Browser.Events.onKeyDown
          ( Decode.map2
              (\key ctrlPressed -> ( key, ctrlPressed ))
              (Decode.field "key" Decode.string)
              (Decode.field "ctrlKey" Decode.bool)
            |> Decode.andThen
              (\v ->
                case v of
                  ( "1", True ) ->
                    -- Debug.log "yup" v |> \_ ->
                    Decode.succeed ResetView
                  ( "Enter", False ) ->
                    Decode.succeed Confirm
                  ( "Escape", False ) ->
                    Decode.succeed Escape
                  ( "Tab", False) ->
                    Decode.succeed Reheat
                  ( "z", True) ->
                    Decode.succeed Undo
                  ( "Z", True) ->
                    Decode.succeed Undo
                  ( "y", True) ->
                    Decode.succeed Redo
                  ( "Y", True) ->
                    Decode.succeed Redo
                  ( ch, _ ) ->
                    let
                      decodeChar =
                        case String.toList ch of
                          [char] ->
                            Decode.succeed (ToggleSelectedTransition <| Character char)
                          _ ->
                            Decode.fail "Not a character key"
                    in
                    case model.currentOperation of
                      Just (ModifyingGraph { dest }) ->
                        case dest of
                          NoDestination ->
                            Decode.fail "Not a recognized key combination"
                          _ ->
                            decodeChar
                      Just (AlteringConnection _) ->
                        decodeChar
                      Just (Splitting _) ->
                        decodeChar
                      _ ->
                        Decode.fail "Not a recognized key combination"
              )
          )
      else
        Sub.none
  in
    if Force.isCompleted model.simulation then
      Sub.batch
        [ keyboardSubscription
        , panSubscription
        ]
    else
      Sub.batch
        [ keyboardSubscription
        , panSubscription
        , Browser.Events.onAnimationFrame (always Tick)
        ]

applyChangesToGraph : AutomatonGraph Entity -> AutomatonGraph Entity
applyChangesToGraph g =
    { g
      | graph =
          -- first, actually remove all disconnected nodes.
          identifyDisconnectedNodes g
          |> Set.foldl Graph.remove g.graph
    }
    |> (fromAutomatonGraph >> toAutomatonGraph)

confirmChanges : Model -> Model
confirmChanges model =
  let
    mkSim g model_ =
      let
        (w, h)  = model_.dimensions
        forceGraph = toForceGraph (g {- |> Debug.log "Received by ForceDirectedGraph" -} )
        basic = basicForces forceGraph (round h)
        viewport = viewportForces (w, h) forceGraph.graph
      in
        { model_ -- make sure we are referencing the correct Model!
          | simulation = Force.simulation (basic ++ viewport)
          , basicForces = basic
          , userGraph = forceGraph
          , viewportForces = viewport
          , specificForces = IntDict.empty
        }
  in
    applyChangesToGraph model.userGraph
    |> (\g -> mkSim g { model | undoBuffer = [], redoBuffer = [] })

textChar : Char -> String
textChar ch =
  case ch of
    ' ' ->
      "└┘"
    _ ->
      String.fromChar ch

transitionToTextSpan : Transition -> (AcceptVia -> List String) -> Svg msg
transitionToTextSpan transition otherClasses =
  case transition of
    (Character ch, 0) ->
      tspan
        [ class <| "nonfinal" :: otherClasses (Character ch) ]
        [ text <| textChar ch ]
    (Character ch, _) ->
      tspan
        [ class <| "final" :: otherClasses (Character ch) ]
        [ text <| textChar ch ]

connectionToSvgText : Connection -> List (Svg msg)
connectionToSvgText =
  AutoSet.toList
  >> List.map (\t -> transitionToTextSpan t (\_ -> []))

connectionToSvgTextHighlightingChars : Connection -> (AcceptVia -> List String) -> List (Svg msg)
connectionToSvgTextHighlightingChars conn highlightFunction =
  AutoSet.toList conn
  |> List.map (\t -> transitionToTextSpan t highlightFunction)

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

type Cardinality
  = Bidirectional
  | Unidirectional
  | Recursive

type LinkType
  = Confirmed Cardinality -- in the confirmed graph.
  | Prospective Cardinality -- user is playing around, hasn't clicked yet.
  | New Cardinality -- user has clicked, but entirety isn't approved yet.
                    -- When it is approved, we'll see it under Confirmed.

linkExistsInGraph : Model -> NodeId -> NodeId -> Bool
linkExistsInGraph model from to =
  -- does a link exist from `from` to `to`?
  Graph.get from model.userGraph.graph
  |> Maybe.map (.outgoing >> IntDict.member to)
  |> Maybe.withDefault False

identifyCardinality : Model -> Edge Connection -> Cardinality
identifyCardinality model { to, from } =
  if to == from then
    Recursive
  else if linkExistsInGraph model to from then
    Bidirectional
  else
    Unidirectional

type alias PathBetweenReturn =
  { pathString : String
  , transition_coordinates : { x : Float, y : Float }
  , length : Float
  , control_point : { x : Float, y : Float }
  , source_connection_point : { x : Float, y : Float }
  , target_connection_point : { x : Float, y : Float }
  }

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

executing_edges : ExecutionResult -> Dict (NodeId, NodeId) (AutoSet.Set String AcceptVia)
executing_edges result =
  let
    trace data acc =
      case data.transitionsTaken of
        [] ->
          acc
        (src, (acceptanceCondition, _))::tail ->
          trace
            { data | transitionsTaken = tail, currentNode = src }
            (Dict.update (src, data.currentNode)
              ( Maybe.map (AutoSet.insert acceptanceCondition)
                >> Maybe.orElse (Just <| AutoSet.singleton acceptConditionToString acceptanceCondition)
              )
              acc
            )
  in
    executionData result
    |> Maybe.map (\data -> trace data Dict.empty)
    |> Maybe.withDefault Dict.empty

viewLink : Model -> Dict (NodeId, NodeId) (AutoSet.Set String AcceptVia) -> Edge Connection -> Svg Msg
viewLink ({ userGraph } as model) executing edge =
  let
    source =
      Maybe.withDefault (entity 0 NoEffect) <| Maybe.map (.node >> .label) <| Graph.get edge.from userGraph.graph

    target =
      Maybe.withDefault (entity 0 NoEffect) <| Maybe.map (.node >> .label) <| Graph.get edge.to userGraph.graph
    cardinality = identifyCardinality model edge
    positioning =
      path_between source target cardinality 7 7
    font_size = 16.0 -- this is the default, if not otherwise set
    labelText =
      case Dict.get (edge.from, edge.to) executing of
        Nothing ->
          connectionToSvgText edge.label
        Just to_highlight ->
          connectionToSvgTextHighlightingChars
            edge.label
            (\c ->
              if AutoSet.member c to_highlight then
                [ "executed" ]
              else
                []
            )
  in
    g
      []
      [
        path
          [ d positioning.pathString
          , noFill
          , class [ "link", "background" ]
          ]
          [ {- title [] [ text <| Automata.Data.connectionToString edge.label ] -} ]
      , path
          [ strokeWidth 3
          , stroke <| Paint <| paletteColors.edge
          , d positioning.pathString
          , markerEnd "url(#arrowhead)"
          , noFill
          , class
              [ "link"
              , if Dict.member (edge.from, edge.to) executing then
                  "executed"
                else
                  ""
              ]
          ]
          [ {- title [] [ text <| Automata.Data.connectionToString edge.label ] -} ]
      , text_
          [ x <| positioning.transition_coordinates.x
          , y <| positioning.transition_coordinates.y
          , fontFamily ["sans-serif"]
          , fontSize font_size
          , fontWeight FontWeightNormal
          , textAnchor AnchorMiddle
          , alignmentBaseline AlignmentCentral
          , Html.Attributes.attribute "paint-order" "stroke fill markers"
          , class [ "link" ]
          , onClick (EditTransition edge.from edge.to edge.label)
          ]
          ( title [] [ text "Click to modify" ] :: labelText 
          )
      , rect
          [ x <| positioning.transition_coordinates.x
          , y <| positioning.transition_coordinates.y - 70
          , width 28
          , height 16
          , rx 2
          , ry 2
          , fill <| Paint <| Color.black
          ]
          []
      , rect
          [ x <| positioning.transition_coordinates.x + 4
          , y <| positioning.transition_coordinates.y - 70 + 4
          , width 4
          , height 4
          , fill <| Paint <| Color.white
          ]
          []
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

viewNode : Model -> Graph.Node Entity -> Svg Msg
viewNode { userGraph, currentOperation, disconnectedNodes, execution, frameOffset, mouseCoords } { label, id } =
  let
    selectableClass =
      case currentOperation of
        Just (ModifyingGraph { source }) ->
          if source == id then
            "selected"
          else
            ""
        _ ->
          if Set.member id disconnectedNodes then
            "disconnected"
          else
            execution
            |> Maybe.andThen executionData
            |> Maybe.map
              (\{ currentNode } ->
                if id == currentNode then
                  "current-node"
                else
                  ""
              )
            |> Maybe.withDefault ""
    graphNode =
      Graph.get id userGraph.graph
    splittable =
      Maybe.map
        (\node ->
          let
            nonRecursive = IntDict.filter (\k _ -> k /= id) node.incoming
          in
          IntDict.size nonRecursive > 1 ||
          ( IntDict.findMin nonRecursive
            |> Maybe.map (\(_, conn) -> AutoSet.size conn > 1)
            |> Maybe.withDefault False
          )
        )
        graphNode
      |> Maybe.withDefault False
    thisNodeIsTerminal =
      Maybe.map Automata.Data.isTerminalNode graphNode
      |> Maybe.withDefault False
    permit_node_reselection =
      Mouse.onWithOptions
        "mousedown"
        { stopPropagation = True, preventDefault = True }
        (\e ->
          if e.keys.shift then
            DragStart id
          else if e.keys.ctrl && splittable then
            StartSplit id
          else
            case currentOperation of
              Just (ModifyingGraph _) ->
                -- ANY node is fine!  If it's the same node, that's also fine.  Recursive links are okay.
                CreateOrUpdateLinkTo id
              _ ->
                SelectNode id
        )

    interactivity =
      case currentOperation of
        Just (ModifyingGraph { dest }) ->
          case dest of
            NoDestination ->
              [ permit_node_reselection ]
            _ ->
              []
        _ ->
          if Set.member id disconnectedNodes then
            []
          else
            [ permit_node_reselection ]

    titleText =
      (if thisNodeIsTerminal && id == userGraph.root then
        "Start AND end of computation\n"
      else if thisNodeIsTerminal then
        "End of computation\n"
      else if id == userGraph.root then
        "Start of computation\n"
      else
        "")
      ++ "Shift-drag to reposition" ++
      (Maybe.map
        (\_ ->
          if splittable then
            "\nCtrl-click to split transitions"
          else
            ""
        ) graphNode
      |> Maybe.withDefault "")
      ++ "\nClick to create or link a new transition"
      ++ "\n(" ++ String.fromInt id ++ ")" -- DEBUGGING
    ( node_x, node_y ) =
      let
        nodeCoords =
          Maybe.map (\ctx -> ( ctx.node.label.x, ctx.node.label.y )) graphNode
          |> Maybe.withDefaultLazy
              (\() -> ( 0, 0 ) |> Debug.log "MWXCGUOEI") -- should never reach here
      in
      case currentOperation of
        Just (Dragging nodeId) ->
          if nodeId == id then
            mapCorrespondingPair (-) mouseCoords frameOffset            
          else
            nodeCoords
        _ ->
          nodeCoords
  in
    g
      ( class ["state-node", selectableClass]
      ::interactivity
      )
      [ circle
          [ r nodeRadius
          , strokeWidth 2
          , cx node_x
          , cy node_y
          , class
              ( if id == userGraph.root then [ "start" ] else [] )
          ]
          []
       ,  if thisNodeIsTerminal && id == userGraph.root then
            text_
              [ x <| node_x
              , y <| (node_y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              ]
              [ text "💥"
              , title [] [text titleText]
              ]
          else if thisNodeIsTerminal then
            text_
              [ x <| node_x
              , y <| (node_y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              ]
              [ text "🎯"
              , title [] [text titleText]
              ]
          else if id == userGraph.root then
            text_
              [ x <| node_x
              , y <| (node_y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 12
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              , fill <| Paint <| Color.grey
              ]
              [ text "⭐"
              , title [] [text titleText]
              ]
          else
            g [] []
      , title [] [text titleText]
      ]

nearby_node_lockOnDistance : Float
nearby_node_lockOnDistance = nodeRadius + 9

nearby_node_repulsionDistance : Float
nearby_node_repulsionDistance =
  nodeRadius * 12

{-| Used by nearby_node and nearby_nodes. Not for other use. -}
nearby_node_func : ((Graph.Node Entity -> Bool) -> List (Graph.Node Entity) -> b) -> Float -> Model -> b
nearby_node_func f distance { userGraph, pan, mouseCoords } =
  -- a good distance value is nodeRadius + 9 = 7 + 9 = 16, for "locking on".
  let
    ( xPan, yPan ) = pan
    ( mouse_x, mouse_y ) =
      mouseCoords -- |> Debug.log "Mousecoords"
    adjustment_x = xPan + mouse_x
    adjustment_y = yPan + mouse_y
    square_dist = distance * distance
  in
    f
      (\node ->
        let
          dx = node.label.x - adjustment_x
          dy = node.label.y - adjustment_y
        in
          -- Debug.log ("Checking (" ++ String.fromFloat node.label.x ++ ", " ++ String.fromFloat node.label.y ++ ") against (" ++ String.fromFloat mouse_x ++ ", " ++ String.fromFloat mouse_y ++ ")") () |> \_ ->
          dx * dx + dy * dy <= square_dist -- 7 + 9 = 16
      )
      (Graph.nodes userGraph.graph)

nearby_node : Float -> Model -> Maybe (Graph.Node Entity)
nearby_node =
  nearby_node_func List.find

nearby_nodes : Float -> Model -> List (Graph.Node Entity)
nearby_nodes =
  nearby_node_func List.filter

viewPhantomSvg : { x : Float, y : Float } -> PathBetweenReturn -> Svg Msg
viewPhantomSvg target positioning =
  let
    radius = 9
  in
    g
      []
      [ circle
          [ r radius
          , cx target.x
          , cy target.y
          , noFill
          , strokeWidth 2
          , strokeDasharray "1 5.2"
          , strokeLinecap StrokeLinecapRound
          , class ["phantom-state-node"]
          ]
          []
      -- now draw the path to it
      , path
          [ strokeWidth 5
          , stroke <| Paint <| paletteColors.background
          , d positioning.pathString
          , noFill
          , class [ "link" ]
          ]
          []
      , path
          [ strokeWidth 3
          , d positioning.pathString
          , if positioning.length > 10 then markerEnd "url(#phantom-arrowhead)" else markerStart "invalid_ref"
          , noFill
          , strokeDasharray "1 5"
          , strokeLinecap StrokeLinecapRound
          , class [ "phantom-link" ]
          ]
          []
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

{-| A "phantom move" that the user MIGHT make, or might not.
    This function is called when the user is still choosing.
    `viewPhantomChosen` is called when the user has chosen.
-}
viewPhantomChoosing : Model -> NodeContext Entity Connection -> Svg Msg
viewPhantomChoosing model sourceNode =
  let
    ( xPan, yPan ) =
      model.pan
    nearby = nearby_node nearby_node_lockOnDistance model
    ( center_x, center_y) =
      -- Now, if the mouse is over an actual node, then we want to "lock" to that node.
      -- But if the mouse is anywhere else, just use the mouse coordinates.
      nearby
      |> Maybe.map (\node -> (node.label.x - xPan, node.label.y - yPan))
      |> Maybe.withDefault model.mouseCoords
    target =
      { x = center_x + xPan
      , y = center_y + yPan
      }
    cardinality =
      case nearby of
        Nothing ->
          Unidirectional
        Just some_node ->
          if some_node.id == sourceNode.node.id then
            Recursive
          else
            case IntDict.get some_node.id sourceNode.incoming of
              Just _ ->
                Bidirectional
              Nothing ->
                Unidirectional
    positioning =
      path_between sourceNode.node.label target cardinality 7 9
  in
    viewPhantomSvg target positioning

{-| A "phantom move" that the user MIGHT make, or might not.
    This function is called when the user has chosen their
    phantom move.
-}
viewPhantomChosen : Model -> NodeContext Entity Connection -> (Float, Float) -> Cardinality -> Svg Msg
viewPhantomChosen model sourceNode (dest_x, dest_y) cardinality =
  let
    ( xPan, yPan ) =
      model.pan
    target =
      { x = dest_x + xPan
      , y = dest_y + yPan
      }
    positioning =
      path_between sourceNode.node.label target cardinality 7 9
  in
    viewPhantomSvg target positioning

{-| A "phantom move" that the user MIGHT make, or might not.
-}
viewPhantom : Model -> NodeContext Entity Connection -> Svg Msg
viewPhantom model sourceNode =
  let
    cardinalityOf nodeId =
      if nodeId == sourceNode.node.id then
        Recursive
      else
        case IntDict.get nodeId sourceNode.incoming of
          Just _ ->
            Bidirectional
          Nothing ->
            Unidirectional
    nodeCoordinates id =
      Graph.get id model.userGraph.graph
      |> Maybe.map
          (\ctx -> ( ctx.node.label.x, ctx.node.label.y ))
      |> Maybe.withDefaultLazy
          (\() -> ( 0, 0 ) |> Debug.log "Error K<UFOUDFI8") -- should never get here!
  in
    case model.currentOperation of
      Just (ModifyingGraph { dest }) ->
        case dest of
          NoDestination ->
            viewPhantomChoosing model sourceNode
          ExistingNode id ->
            viewPhantomChosen model sourceNode
              (nodeCoordinates id)
              (cardinalityOf id)
          NewNode coords ->
            viewPhantomChosen model sourceNode coords Unidirectional
          EditingTransitionTo id ->
            viewPhantomChosen model sourceNode
              (nodeCoordinates id)
              (cardinalityOf id)
      _ ->
        g [] []

arrowheadMarker : Svg msg
arrowheadMarker =
  marker
    [ id "arrowhead"
    , viewBox 0 0 10 10
    , refX "0"
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

phantomArrowheadMarker : Svg msg
phantomArrowheadMarker =
  marker
    [ id "phantom-arrowhead"
    , viewBox 0 0 10 10
    , refX "0"
    , refY "5"
    , orient "auto-start-reverse"
    , markerWidth 5
    , markerHeight 5
    , strokeWidth 1.5
    , strokeDasharray "1.5 2"
    , strokeLinecap StrokeLinecapRound
    , stroke <| Paint <| Color.rgb255 102 102 102
    , noFill
    ]
    [ path
        [ d "M 0 0 L 10 5 L 0 10 z" ]
        []
    ]

transition_buttonSize : Float
transition_buttonSize = 55

transition_spacing : Float
transition_spacing = 15

-- https://ishadeed.com/article/target-size/
viewSingleKey : Char -> Connection -> (Int, Int) -> Svg Msg
viewSingleKey ch conn (gridX, gridY) =
  let
    buttonX = transition_spacing * toFloat (gridX + 1) + transition_buttonSize * toFloat gridX
    buttonY = transition_spacing * toFloat (gridY + 1) + transition_buttonSize * toFloat gridY
    isThisNodeTerminal = AutoSet.member (Character ch, 1) conn
    keyClass =
      if AutoSet.member (Character ch, 0) conn then
        [ "transition-chooser-key", "selected" ]
      else if isThisNodeTerminal then
        [ "transition-chooser-key", "selected", "terminal" ]
      else
        [ "transition-chooser-key" ]
  in
  g
    []
    [ rect
        [ x buttonX
        , y buttonY
        , Px.width transition_buttonSize
        , Px.height transition_buttonSize
        , Px.rx 5
        , Px.ry 5
        , strokeWidth 2
        , stroke <| Paint <| Color.white
        , class keyClass
        , onClick <| ToggleSelectedTransition (Character ch)
        ]
        ( if isThisNodeTerminal then [ title [] [ text "This is a terminal transition" ] ] else [] )
    , text_
        [ x <| buttonX + transition_buttonSize / 2
        , y <| buttonY + transition_buttonSize / 2
        , fontSize 20
        , strokeWidth 0
        , textAnchor AnchorMiddle
        , alignmentBaseline AlignmentCentral
        , dominantBaseline DominantBaselineMiddle
        , pointerEvents "none"
        ]
        [ text <| String.fromChar ch ]
    ]


viewSvgTransitionChooser : Model -> Svg Msg
viewSvgTransitionChooser model =
  let
    ( w, _ ) = model.dimensions
    items_per_row = round ( (w - transition_spacing * 2) / (transition_buttonSize + transition_spacing) )
    alphabet = String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890~`[{}]-_=\\|;:,.<>/?!@#$%^&*()+ abcdefghijklmnopqrstuvwxyz"
    numRows = ceiling <| toFloat (List.length alphabet) / toFloat items_per_row
    gridItemsAndCoordinates =
      List.foldl
        (\item (acc, ( col, row )) ->
          if col + 1 >= items_per_row then
            ((item, col, row) :: acc, (0, row + 1))
          else
            ((item, col, row) :: acc, (col + 1, row))
        )
        ([], (0, 0))
        alphabet
      |> Tuple.first
    conn =
      case model.currentOperation of
        Just (ModifyingGraph { transitions }) ->
          transitions
        Just (AlteringConnection { transitions }) ->
          transitions
        _ ->
          (AutoSet.empty transitionToString)
          |> Debug.log "CXO<DPO(*EU I should not be here!"
  in
  g
    []
    ( g
        []
        [ text_
            [ x <| w / 2
            , y <| transition_spacing * toFloat (numRows + 2) + toFloat numRows * transition_buttonSize + transition_spacing
            , fill <| Paint <| Color.black
            , textAnchor AnchorMiddle
            , alignmentBaseline AlignmentCentral
            , dominantBaseline DominantBaselineMiddle
            , pointerEvents "none"
            , fontSize 24
            , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
            , stroke <| Paint <| paletteColors.background
            , strokeWidth 5
            , class [ "link" ]
            ]
            ( tspan [] [ text "Selected transitions: " ] ::
              ( if AutoSet.isEmpty conn then
                  [ tspan [] [ text "None" ] ]
                else
                  connectionToSvgText conn
              )
            )
        , text_
            [ x <| w / 2
            , y <| transition_spacing * toFloat (numRows + 2) + toFloat numRows * transition_buttonSize + transition_spacing + 30
            , fill <| Paint <| Color.darkCharcoal
            , textAnchor AnchorMiddle
            , alignmentBaseline AlignmentCentral
            , dominantBaseline DominantBaselineMiddle
            , pointerEvents "none"
            , fontSize 16
            , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
            , stroke <| Paint <| paletteColors.background
            , strokeWidth 4
            ]
            ( if AutoSet.isEmpty conn then
                [ tspan [] [ text "If there are no transitions, this link will be destroyed." ] ]
              else
                [ tspan [] [ text "Press «Enter» to confirm these transitions." ] ]
            )
        ]
    :: List.map (\(item, col, row) -> viewSingleKey item conn (col, row)) gridItemsAndCoordinates
    )

viewSvgTransitionSplitter : Connection -> Connection -> Model -> Svg Msg
viewSvgTransitionSplitter left right model =
  let
    ( w, _ ) = model.dimensions
    paddingLeftRight = 15
    calc_width n =
      ( paddingLeftRight * 2 + -- padding on left & right
        1 * font_size * -- space per character, with a weight factor
        toFloat n -- multiplied by number of characters
      )
    max_box_width =
      calc_width (AutoSet.size left + AutoSet.size right)
    leftCenter = w / 2 - max_box_width
    rightCenter = w / 2 + max_box_width
    font_size = 24
    box centerPosition set color =
      rect
        [ x <| centerPosition - calc_width (AutoSet.size set) / 2
        , y <| 100 - font_size
        , width <| calc_width (AutoSet.size set) 
        , height <| font_size * 2
        , fill <| Paint <| color
        , rx 10
        , ry 10
        , class ["splitter-box"]
        ]
        []
    box_text centerPosition set =
      text_
        [ x centerPosition
        , y 100
        , textAnchor AnchorMiddle
        , alignmentBaseline AlignmentCentral
        , dominantBaseline DominantBaselineMiddle
        , pointerEvents "none"
        , fontSize font_size
        , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
        ]
        ( connectionToSvgText set )
    box_title centerPosition s =
      text_
        [ x centerPosition
        , y <| (100 + font_size * 1.8)
        , textAnchor AnchorMiddle
        , fontSize <| font_size * 0.6
        , alignmentBaseline AlignmentCentral
        , dominantBaseline DominantBaselineMiddle
        , pointerEvents "none"
        , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
        , stroke <| Paint <| paletteColors.background
        , strokeWidth 5
        ]
        [ text s ]
  in
    g
      []
      [ box leftCenter left Color.lightGreen
      , box rightCenter right Color.lightPurple
      , box_text leftCenter left
      , box_text rightCenter right
      , box_title leftCenter "Node “A”"
      , box_title rightCenter "Node “B”"
      , text_
          [ x <| w / 2
          , y <| 180
          , fill <| Paint <| Color.darkCharcoal
          , textAnchor AnchorMiddle
          , alignmentBaseline AlignmentCentral
          , dominantBaseline DominantBaselineMiddle
          , pointerEvents "none"
          , fontSize 16
          , Html.Attributes.attribute "paint-order" "stroke fill markers" -- this is pretty important!
          , stroke <| Paint <| paletteColors.background
          , strokeWidth 4
          ]
          ( if AutoSet.isEmpty left || AutoSet.isEmpty right then
              [ tspan [] [ text "If either side has no transitions, then no changes will be made." ] ]
            else
              [ tspan [] [ text "Press «Enter» to confirm this split, or «Esc» to cancel." ] ]
          )
      ]
  -- rect
  --   [ x 100
  --   , y 100
  --   , fill <| Paint Color.lightOrange
  --   , width 150
  --   , height 100
  --   ]
  --   []

viewUndoRedoVisualisation : Model -> Svg a
viewUndoRedoVisualisation { undoBuffer, redoBuffer, dimensions } =
  let
    (_, h) = dimensions
    rect_width = 30
    rect_height = 10
    rect_spacing = 3
    num_undo = List.length undoBuffer
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
                [ width rect_width
                , height rect_height
                , fill <| Paint (List.getAt idx scale |> Maybe.withDefault worst)
                , ry 2
                , x 15
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
                [ width rect_width
                , height rect_height
                , ry 2
                , x 15
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

matrixFromZoom : (Float, Float) -> (Float, Float) -> ( Float, ( Float, Float ) ) -> ( Float, Float ) -> Transform
matrixFromZoom (w, h) (panX, panY) ( factor, _ ) (pointerX, pointerY) =
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
    ( (1 - factor) * pointerX - panX )
    ( (1 - factor) * pointerY - panY )
  -- |> Debug.log "matrix transform applied"

view : Model -> Svg Msg
view model =
  let
    permit_zoom = onMouseScroll Zoom
    permit_pan =
      [ onMouseMove MouseMove
      , cursor <|
          -- working around an insane Elm-compiler parser bug https://github.com/elm/compiler/issues/1261
          case ( 1 + round (xPanAt model (Tuple.first model.mouseCoords)), 1 + round (yPanAt model (Tuple.second model.mouseCoords)) ) of
            ( 2, 2 ) -> CursorSEResize
            ( 0, 2 ) -> CursorSWResize
            ( 2, 0 ) -> CursorNEResize
            ( 0, 0 ) -> CursorNWResize
            ( 1, 2 ) -> CursorNResize
            ( 0, 1 ) -> CursorWResize
            ( 1, 0 ) -> CursorNResize -- eh? where's CursorSResize?
            ( 2, 1 ) -> CursorEResize
            _ ->
              case model.currentOperation of
                Just (ModifyingGraph _) -> Cursor "none"
                Just (AlteringConnection _) -> Cursor "none"
                _ -> CursorDefault
      ]
    permit_click =
      Mouse.onWithOptions
        "mousedown"
        { stopPropagation = True, preventDefault = True }
        (\_ ->
          case model.currentOperation of
            Just (ModifyingGraph _) ->
              case nearby_node nearby_node_lockOnDistance model of
                Just node -> -- in this case, the UI would show it being "locked-on"
                  CreateOrUpdateLinkTo node.id
                Nothing ->
                  CreateNewNodeAt model.mouseCoords
            _ ->
              Escape
        )
    permit_stopdrag =
      Mouse.onUp (\_ -> DragEnd)
    interactivity =
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
      case model.currentOperation of
        Just (Splitting _) ->
          [ permit_click ]
        Just (ModifyingGraph { dest }) ->
          case dest of
            EditingTransitionTo _ ->
              []
            NewNode _ ->
              []
            ExistingNode _ ->
              []
            _ ->
              permit_click :: permit_zoom :: permit_pan
        Just (AlteringConnection _) ->
          [ permit_click ]
        Just (Dragging _) ->
          -- must have permit_pan (for mouse-movement)
          -- must have permit_stopdrag
          -- ignore click (we only care about stop-drag right now)
          permit_stopdrag :: permit_zoom :: permit_pan
        Nothing ->
          permit_click :: permit_zoom :: permit_pan
  in
  svg
    ([ viewBox 0 0 (Tuple.first model.dimensions) (Tuple.second model.dimensions)
    , Mouse.onOver (\_ -> SetMouseOver)
    , Mouse.onOut (\_ -> SetMouseOut)
    ] ++ interactivity)
    [ -- this stuff is in the background.
      viewUndoRedoVisualisation model
    , g -- this is the "main" interactive frame, which will be zoomed, panned, etc.
      [ transform [ matrixFromZoom model.dimensions model.pan model.zoom model.mouseCoords ]
      , case model.currentOperation of
          Just (Dragging _) ->
            -- disable pointer-events.
            -- This is to stop elements from getting in the way of
            -- mouse-movement.
            pointerEvents "none"
          _ ->
            class []
      ]
      [ defs [] [ arrowheadMarker, phantomArrowheadMarker ]
      , Graph.edges model.userGraph.graph
        |> List.filter (\edge -> not (AutoSet.isEmpty edge.label))
        |> List.map
          (viewLink
            model
            ( Maybe.map executing_edges model.execution
              |> Maybe.withDefault Dict.empty))
        |> g [ class [ "links" ] ]
      , Graph.nodes model.userGraph.graph
        |> List.map (viewNode model)
        |> g [ class [ "nodes" ] ]
      , case model.currentOperation of
          Just (ModifyingGraph { source, dest }) ->
            case ( source, dest ) of
              ( _, EditingTransitionTo _ ) ->
                g [] []
              ( id, _ ) ->
                Graph.get id model.userGraph.graph
                |> Maybe.map (viewPhantom model)
                |> Maybe.withDefault (g [] [])
          _ ->
            g [] []
      ]
    , case model.currentOperation of
        Just (ModifyingGraph { dest }) ->
          case dest of
            NoDestination ->
              g [] []
            _ ->
              viewSvgTransitionChooser model
        Just (AlteringConnection _) ->
          viewSvgTransitionChooser model
        Just (Splitting { to_split, left, right }) ->
          Graph.get to_split model.userGraph.graph
          |> Maybe.map (\_ -> -- ignore node, we have all the info already
            viewSvgTransitionSplitter left right model
          )
          |> Maybe.withDefault (g [] [])
        Nothing ->
          g [] []
        Just (Dragging _) ->
          g [] []
    ,
      let
        bottomMsg message =
          text_
            [ Px.x 15
            , Px.y (Tuple.second model.dimensions - 15)
            , fill <| Paint <| Color.black
            , fontFamily ["sans-serif"]
            , fontSize 14
            , textAnchor AnchorStart
            , alignmentBaseline AlignmentCentral
            , dominantBaseline DominantBaselineCentral
            , pointerEvents "none"
            ]
            [ text message ]
      in
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
            [ x (Tuple.first model.dimensions - 15)
            , y (Tuple.second model.dimensions - 15)
            , fill <| Paint <| Color.black
            , fontFamily ["sans-serif"]
            , fontSize 14
            , textAnchor AnchorEnd
            , alignmentBaseline AlignmentCentral
            , dominantBaseline DominantBaselineCentral
            , pointerEvents "none"
            ]
            [ text (" 🔍 " ++ String.fromInt (round <| Tuple.first model.zoom * 100) ++ "%") ]
        , text_
            [ x (Tuple.first model.dimensions - 90)
            , y (Tuple.second model.dimensions - 15)
            , fill <| Paint <| Color.black
            , fontFamily ["sans-serif"]
            , fontSize 14
            , textAnchor AnchorEnd
            , alignmentBaseline AlignmentCentral
            , dominantBaseline DominantBaselineCentral
            , pointerEvents "none"
            ]
            [ text (" 🧭 " ++ panToString model.pan) ]
        , case model.currentOperation of
            Just (ModifyingGraph { dest }) ->
              case dest of
                NoDestination ->
                  bottomMsg "Press «Esc» to cancel link creation"
                ExistingNode _ ->
                  bottomMsg "Choose transitions to connect these nodes. Press «Esc» to cancel."
                NewNode _ ->
                  bottomMsg "Choose transitions for this link. Press «Esc» to cancel."
                EditingTransitionTo _ ->
                  bottomMsg "Choose transitions for this link. Press «Esc» to cancel."
            Just (AlteringConnection _) ->
              bottomMsg "Choose transitions for this link. Press «Esc» to cancel."
            Just (Splitting _) ->
              g [] []
            _ ->
              case model.undoBuffer of
                [] ->
                  g [] []
                _ ->
                  bottomMsg "Press «Enter» to apply these changes; press «Ctrl-Z» / «Ctrl-Y» to undo / redo"
        ]
    ]

onMouseScroll : (Float -> (Float, Float) -> msg) -> Html.Attribute msg
onMouseScroll msg =
  HE.on "wheel" <|
    Decode.map3
      (\x y deltaY -> msg deltaY (x, y))
      (Decode.field "offsetX" Decode.float)
      (Decode.field "offsetY" Decode.float)
      (Decode.field "deltaY" Decode.float)

onMouseMove : (Float -> Float -> msg) -> Html.Attribute msg
onMouseMove msg =
  HE.on "mousemove"
    ( Decode.at ["target", "tagName"] Decode.string
      |> Decode.andThen
        (\tagName ->
          if String.toUpper tagName == "SVG" then
            Decode.map2 msg
              -- offset relative to the parent
              -- …which is the SVG element.
              (Decode.field "offsetX" Decode.float)
              (Decode.field "offsetY" Decode.float)
          else
            Decode.fail "Ignoring non-SVG target"
        )
    )

debugModel_ : String -> Model -> Model
debugModel_ message model =
  let
    op =
      case model.currentOperation of
        Just o -> "(" ++ Debug.toString o ++ ")"
        Nothing -> "(no op)"
    graph = Automata.Debugging.printAutomatonGraph model.userGraph
    screen =
      "(" ++ String.fromFloat (Tuple.first model.dimensions) ++ ", " ++ String.fromFloat (Tuple.second model.dimensions) ++
      " : " ++ panToString model.pan ++ ")"
    buffers =
      String.fromInt (List.length model.undoBuffer) ++ " / " ++ String.fromInt (List.length model.redoBuffer) ++ " undo/redo"
    disconnected =
      "{ " ++ (Set.map String.fromInt model.disconnectedNodes |> Set.toList |> String.join ", ") ++ " }"
    pending =
      "Undobuffer: " ++
      ( case model.undoBuffer of
          [] -> "nothing"
          _ ->
            "\n" ++
            ( model.undoBuffer
              |> List.map (DFA.serializeAutomatonGraph >> String.padLeft 4 ' ')
              |> String.join "\n"
            )
      )
  in
    Debug.log (message
      ++ "\n" ++ op ++ " " ++ screen ++ " " ++ buffers ++ " " ++ disconnected ++ "\n" ++
      pending ++ "\n" ++ graph) ()
    |> \_ -> model