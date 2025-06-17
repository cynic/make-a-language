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
  , markerWidth, markerHeight)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing
  (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..)
  , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..))
import Automata.Data exposing (Node, Connection, AutomatonGraph)
import TypedSvg.Attributes.InPx as Px
import Html
import Html.Attributes
import Set
import IntDict
import Time
import List.Extra as List
import Automata.DFA exposing (wordsEndingAt, modifyConnection, fromAutomatonGraph)
import Automata.Data exposing (isTerminal)
import Automata.DFA exposing (toGraph)
import Automata.DFA exposing (union)
import Automata.Debugging exposing (debugGraph)
import Maybe.Extra as Maybe exposing (withDefaultLazy)

type Msg
  = DragStart NodeId ( Float, Float )
  | DragAt ( Float, Float )
  | DragEnd ( Float, Float )
  | Tick
  | WordsUpdated (List String)
  | ViewportUpdated (Float, Float)
  | MouseMove Float Float
  | Zoom Float (Float, Float)
  | ResetView
  | SelectNode NodeId
  | DeselectNode
  | ToggleSelectedTransition Char
  | SetMouseOver
  | SetMouseOut
  | CreateOrUpdateLinkTo NodeId -- this is for an already-existing node.
  | CreateNewNodeAt ( Float, Float )
  | Escape -- the universal "No! Go Back!" key & command
  | Confirm -- the universal "Yeah! Let's Go!" key & command
  | EditTransition NodeId NodeId Connection
  | Reheat

-- For zooming, I take the approach set out at https://www.petercollingridge.co.uk/tutorials/svg/interactive/pan-and-zoom/

type LinkDestination
  = NoDestination
  | ExistingNode NodeId
  | EditingTransitionTo NodeId
  | NewNode ( Float, Float ) -- with X and Y coordinates

type alias Model =
  { drag : Maybe Drag
  , graph : Graph Entity Connection
  , start : NodeId
  , simulation : Force.State NodeId
  , dimensions : (Float, Float) -- (w,h) of svg element
  , basicForces : List (Force.Force NodeId) -- EXCLUDING the "center" force.
  , viewportForces : List (Force.Force NodeId)
  , specificForces : IntDict.IntDict (List (Force.Force NodeId))
  , zoom : ( Float, ( Float, Float ) ) -- ( zoom-factor, zoom-center-coordinates )
  , pan : (Float, Float) -- panning offset, x and y
  , mouseCoords : ( Float, Float )
  , selectedSource : Maybe NodeId
  , selectedDest : LinkDestination
  , selectedTransitions : Connection
  , mouseIsHere : Bool
  , userRequestedChanges : List RequestedGraphChanges
  , unusedId : NodeId
  }

type RequestedGraphChanges
  -- transition-related changes: convert non-final to final, convert final to non-final, remove a transition, add a transition
  -- all of which can & should be handled via ModifyTransition tbh
  = ModifyTransition
      { from : NodeId
      , to : NodeId
      , oldState : Connection
      , newState : Connection
      }
  | NewLinkToNode
      { from : NodeId
      , to : NodeId
      , conn : Connection
      }
  | AddNewNode
      { from : NodeId
      , newNodeId : NodeId
      , x : Float
      , y : Float
      , conn : Connection
      }
  | RemoveNode NodeId

type alias Drag =
  { start : ( Float, Float )
  , current : ( Float, Float )
  , index : NodeId
  }

type alias Entity =
  Force.Entity NodeId { value : { } }

{-| The buffer from the edges within which panning occurs -}
panBuffer : Float
panBuffer = 40

initializeNode : Node a -> NodeContext Entity Connection
initializeNode ctx =
  { node =
    { label =
        Force.entity ctx.node.id { }
    , id = ctx.node.id
    }
  , incoming = ctx.incoming
  , outgoing = ctx.outgoing
  }

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
            , distance = 10 + 25.0 * toFloat (Set.size v) -- 35-40 seems like a good distance
            , strength = Just 0.7 -- * (toFloat <| Set.size v)
            } :: forces
          )
          acc
    )
    []
    graph
  |> Force.customLinks 3

basicForces : NodeId -> Graph Entity Connection -> Int -> List (Force.Force NodeId)
basicForces start graph height =
  [ makeLinkForces graph -- the springs
  , Force.manyBodyStrength -2000.0 (List.map .id <| Graph.nodes graph) -- the repulsion
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
    (basicForces start graph (round h) ++ viewportForces (w, h) graph)

toForceGraph : AutomatonGraph a -> Graph Entity Connection
toForceGraph g =
  Graph.mapContexts initializeNode g.graph

receiveWords : List String -> (Float, Float) -> Model
receiveWords words (w, h) =
  let
    graph = Automata.DFA.fromWords words
    forceGraph = toForceGraph (graph {- |> Debug.log "Received by ForceDirectedGraph" -} )
    basic = basicForces graph.root forceGraph (round h)
    viewport = viewportForces (w, h) forceGraph
  in
    { drag = Nothing
    , graph = forceGraph
    , simulation = Force.simulation (basic ++ viewport)
    , dimensions = (w, h)
    , basicForces = basic
    , viewportForces = viewport
    , specificForces = IntDict.empty
    , start = graph.root
    , zoom = ( 1.0, ( w/2, h/2 ) )
    , mouseCoords = ( w/2, h/2 )
    , selectedSource = Nothing
    , selectedDest = NoDestination
    , selectedTransitions = Set.empty
    , pan = ( 0, 0)
    , mouseIsHere = False
    , userRequestedChanges = []
    , unusedId = graph.maxId + 1
    }

init : List String -> (Float, Float) -> (Model, Cmd Msg)
init words (w, h) =
  ( receiveWords words (w, h), Cmd.none )

updateNode : ( Float, Float ) -> ( Float, Float ) -> NodeContext Entity Connection -> NodeContext Entity Connection
updateNode ( x, y ) (offsetX, offsetY) nodeCtx =
  let
    nodeValue =
      nodeCtx.node.label
  in
    updateContextWithValue nodeCtx { nodeValue | x = x + offsetX, y = y + offsetY }


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
  if y >= ( Tuple.second model.dimensions - panBuffer ) then
    1
  else if y <= panBuffer then
    -1
  else
    0

userchange_modifyTransition : Model -> NodeId -> NodeId -> List RequestedGraphChanges
userchange_modifyTransition model source dest =
  -- this is called when there is already a link between source and dest,
  -- and that link must be modified.
  {-So, there are a few things to look at here.
      1. Is there a corresponding `NewLinkToNode` or `AddNewNode`? If so, then
         I can (and should) just modify the `conn` there: there is no "baseline".
      2. Is there a corresponding `ModifyTransition`? If so, then I should just modify that.
      3. Otherwise, I should add in a `ModifyTransition`.
  -}
  let
    baselineIndex =
      List.findIndex
        (\item ->
          case item of
            NewLinkToNode { from, to } ->
              from == source && to == dest
            AddNewNode { from, newNodeId } ->
              from == source && newNodeId == dest
            ModifyTransition { from, to } ->
              from == source && to == dest
            _ -> False
        )
        model.userRequestedChanges
  in
    case baselineIndex of
      Nothing ->
        -- this is not found in the baseline; therefore, it must be from the graph.
        ModifyTransition
          { from = source
          , to = dest
          , newState = model.selectedTransitions
          , oldState =
              Graph.get dest model.graph
              |> Maybe.andThen (.incoming >> IntDict.get source)
              |> Maybe.withDefault Set.empty
          }
        :: model.userRequestedChanges
      Just idx ->
        List.updateAt idx
          (\orig ->
            case orig of
              NewLinkToNode v ->
                NewLinkToNode { v | conn = model.selectedTransitions }
              AddNewNode v ->
                AddNewNode { v | conn = model.selectedTransitions }
              ModifyTransition v ->
                ModifyTransition { v | newState = model.selectedTransitions }
              _ ->
                orig -- impossible.
          )
          model.userRequestedChanges

userchange_newLinkToNode : Model -> NodeId -> NodeId -> List RequestedGraphChanges
userchange_newLinkToNode model source dest =
  -- this is called when there is no existing link between source and dest,
  -- but both nodes already exist in the graph.
  NewLinkToNode
    { from = source
    , to = dest
    , conn = model.selectedTransitions
    } :: model.userRequestedChanges

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
                  (Maybe.map (updateNode current model.pan))
                  (updateGraphWithList model.graph list)
              , simulation = newState
            }

    WordsUpdated words ->      
      receiveWords words model.dimensions

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
            , graph = Graph.update index (Maybe.map (updateNode xy model.pan)) model.graph
            -- , simulation = Force.reheat model.simulation
          }

        Nothing ->
          { model | drag = Nothing }

    DragEnd (x,y) ->
      case model.drag of
        Just { index } ->
          let
            ( offsetX, offsetY ) = model.pan
            sf =
              IntDict.insert index
                [ Force.towardsX [{ node = index, strength = 2.5, target = x + offsetX }]
                , Force.towardsY [{ node = index, strength = 2.5, target = y + offsetY }]
                ]
                model.specificForces
          in
            { model
              | drag = Nothing
              , graph = Graph.update index (Maybe.map (updateNode (x,y) model.pan)) model.graph
              , specificForces = sf
              , simulation = Force.simulation (List.concat (IntDict.values sf))
            }

        Nothing ->
          { model | drag = Nothing }

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
        ( xPan, yPan ) = model.pan
      in
      { model
      | mouseCoords = (x, y)
      , pan =
          case model.selectedDest of
            NoDestination ->
              ( xPan + xPanAt model x, yPan + yPanAt model y )
            _ ->
              model.pan
      }

    SelectNode index ->
      { model | selectedSource = Just index }

    DeselectNode ->
      { model | selectedSource = Nothing }

    SetMouseOver ->
      { model | mouseIsHere = True }

    SetMouseOut ->
      { model | mouseIsHere = False }

    CreateOrUpdateLinkTo dest ->
      { model
      | selectedDest = ExistingNode dest
      , selectedTransitions =
          Graph.get dest model.graph
          |> Maybe.map (\node ->
            case model.selectedSource of
              Just source ->
                case IntDict.get source node.incoming of
                  Just conn ->
                    conn
                  Nothing -> -- no link to this node, at present.
                    Set.empty
              Nothing ->
                -- NO SOURCE?!!!
                Set.empty
          )
          |> Maybe.withDefault Set.empty
      }

    CreateNewNodeAt ( x, y ) ->
      { model | selectedDest = NewNode ( x, y ) }

    Escape ->
      let
        escape_from_initial_node_selection =
          case model.selectedSource of
            Nothing ->
              -- ??? I guess I'm escaping from nothing.
              model
            Just _ ->
              { model | selectedSource = Nothing }
        escape_from_node_link =
          case model.selectedDest of
            NoDestination ->
              -- hmm. I must be escaping from something earlier!
              escape_from_initial_node_selection
            EditingTransitionTo _ ->
              -- back off ALL the way.
              { model
              | selectedDest = NoDestination
              , selectedTransitions = Set.empty
              , selectedSource = Nothing
              }
            _ ->
              { model | selectedDest = NoDestination }
        escape_from_anything = escape_from_node_link
      in
      -- ooh!  What are we "escaping" from, though?
      escape_from_anything

    Confirm ->
      -- What am I confirming?
      let
        createNewNode src x y =
          let
            newGraph =
              Graph.insert
                { node =
                  { label =
                      let
                        initial =
                          Force.entity model.unusedId { }
                      in
                        { initial | x = x, y = y }
                  , id = model.unusedId
                  }
                , incoming = IntDict.singleton src model.selectedTransitions
                , outgoing = IntDict.empty
                }
                model.graph
          in
          { model
          | selectedTransitions = Set.empty
          , selectedDest = NoDestination
          , selectedSource = Nothing
          , userRequestedChanges =
              AddNewNode
                { from = src
                , newNodeId = model.unusedId
                , x = x
                , y = y
                , conn = model.selectedTransitions
                } :: model.userRequestedChanges
          , graph = newGraph
          , basicForces =
              basicForces model.start newGraph (round <| Tuple.second model.dimensions)
          , unusedId = model.unusedId + 1
          }

        updateExistingNode src dest =
          let
            updatedGraph =
              Graph.update dest
                (Maybe.map (\node ->
                  { node
                    | incoming =
                        IntDict.insert src model.selectedTransitions node.incoming
                    , outgoing =
                        -- if it's recursive, I must add it to both so that it reflects in the graph.
                        if src == dest then
                          IntDict.insert src model.selectedTransitions node.outgoing
                        else
                          node.outgoing
                  }
                ))
                model.graph
          in
          { model
          | selectedTransitions = Set.empty
          , selectedDest = NoDestination
          , selectedSource = Nothing
          , userRequestedChanges =
              if linkExistsInGraph model src dest then
                -- if these two are connected already [in the correct direction], then we must just adjust the transition.
                -- Debug.log "MOO" () |> \_ ->
                userchange_modifyTransition model src dest
              else
                -- however, if there is no connection between existing nodes, then we must create a new link.
                -- Debug.log "NOO" () |> \_ ->
                userchange_newLinkToNode model src dest
          , graph = updatedGraph
          , basicForces =
              basicForces model.start updatedGraph (round <| Tuple.second model.dimensions)
          }

      in
        model.selectedSource
        |> Maybe.map (\src -> -- if we have a source, there may be "active", confirmable transitions that the user has selected
          case model.selectedDest of
            ( NewNode ( x, y ) ) ->
              -- create a totally new node, never before seen!
              createNewNode src x y
            ( ExistingNode dest ) -> -- TODO: Existing Nodes.  The full gamut, in sha Allah!
              updateExistingNode src dest
            ( EditingTransitionTo dest ) ->
              updateExistingNode src dest
            ( NoDestination ) ->
              -- ??? Nothing for me to do!  The user is just pressing Enter because… uh… eh, who knows?
              model
        )
        |> Maybe.withDefaultLazy (\() ->
          -- in this branch, there are no "active", confirmable transitions to confirm.
          -- however…
          case model.userRequestedChanges of
            [] ->
              model -- nothing for me to do!
            _ ->
              confirmChanges model
        )

    ToggleSelectedTransition ch ->
      let
        newSelectedTransitions =
          if Set.member (ch, 0) model.selectedTransitions then
            Set.remove (ch, 0) model.selectedTransitions
            |> Set.insert (ch, 1)
          else if Set.member (ch, 1) model.selectedTransitions then
            Set.remove (ch, 1) model.selectedTransitions
          else
            Set.insert (ch, 0) model.selectedTransitions
      in
        { model | selectedTransitions = newSelectedTransitions }

    EditTransition src dest conn ->
      -- … and this will take us to much same place that
      -- UpdateOrCreateLinkTo took us.  But the difference
      -- is that we indicate an edit via EditingTransitionTo,
      -- rather than ExistingNode.  This indicates that if we
      -- escape out of it, we clear source & dest & transitions
      -- all at once.
      { model
      | selectedSource = Just src
      , selectedDest = EditingTransitionTo dest
      , selectedTransitions = conn
      }

    Reheat ->
      -- If I'm not doing anything else, permit auto-layout
      case model.selectedSource of
        Nothing ->
          { model
          | simulation = Force.simulation (model.basicForces ++ model.viewportForces)
          , specificForces = IntDict.empty -- cancel moves that were made before
          }
        _ ->
          model

offset : (Float, Float) -> (Float, Float) -> (Float, Float)
offset (offset_x, offset_y) (x, y) =
  (x - offset_x, y - offset_y)

subscriptions : (Float, Float) -> Model -> Sub Msg
subscriptions offset_amount model =
  let
    ( xCoord, yCoord ) = model.mouseCoords
    panSubscription =
      if model.mouseIsHere && (xPanAt model xCoord /= 0 || yPanAt model yCoord /= 0) then
        Time.every 10 (\_ -> MouseMove xCoord yCoord)
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
                  ( ch, False ) ->
                    case model.selectedDest of
                      NoDestination ->
                        Decode.fail "Not a recognized key combination"
                      _ ->
                        case String.toList ch of
                          [char] ->
                            Decode.succeed (ToggleSelectedTransition char)
                          _ ->
                            Decode.fail "Not a character key"
                  _ ->
                    -- Debug.log "hmm" v |> \_ ->
                    Decode.fail "Not a recognized key combination"
              )
          )
      else
        Sub.none
  in
  case model.drag of
    Nothing ->
      -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
      -- to the rAF.
      if Force.isCompleted model.simulation then
        Sub.batch [ keyboardSubscription, panSubscription ]

      else
        Sub.batch [ keyboardSubscription, panSubscription, Browser.Events.onAnimationFrame (always Tick) ]

    Just _ ->
      Sub.batch
        [ Browser.Events.onMouseMove (Decode.map (.clientPos >> (offset offset_amount) >> DragAt) Mouse.eventDecoder)
        , Browser.Events.onMouseUp (Decode.map (.clientPos >> (offset offset_amount) >> DragEnd) Mouse.eventDecoder)
        , Browser.Events.onAnimationFrame (always Tick)
        , panSubscription
        , keyboardSubscription
        ]

confirmChanges : Model -> Model
confirmChanges model_ =
  let
    ag : AutomatonGraph Entity
    ag =
      { root = model_.start
      , graph = model_.graph
      , maxId = model_.unusedId - 1
      }
    applyChange_addOrLinkNode : NodeId -> AutomatonGraph Entity -> AutomatonGraph Entity
    applyChange_addOrLinkNode nodeId graph =
      let
        newDFA =
          { start = graph.root
          , transition_function = IntDict.empty
          , states =
              Graph.get graph.root graph.graph
              |> Maybe.map (\node -> IntDict.singleton node.node.id node.node.label)
              |> Maybe.withDefaultLazy (\() -> Debug.todo "MIGY>EFY") -- IntDict.empty -- SHOULD NEVER BE HERE!
          , finals =
              if Automata.Data.isTerminal graph.root graph.graph then
                Set.singleton graph.root
              else
                Set.empty
          } -- |> Automata.DFA.debugDFA_ "[AddNewNode/NewLinkToNode] 'Template' DFA"
      in
        Graph.get nodeId model_.graph
        |> Maybe.map
          (\node ->
            wordsEndingAt (node.node.id, node.node.label) ({- debugGraph "Initial graph" -} graph.graph) Set.empty newDFA
            --|> Automata.DFA.debugDFA_ "[AddNewNode/NewLinkToNode] DFA ending at target"
            |> \dfa -> union dfa (Automata.DFA.fromGraph graph.root graph.graph)
            |> Automata.DFA.debugDFA_ "[AddNewNode/NewLinkToNode] After union"
            |> toGraph
          )
        |> Maybe.withDefault graph
    applyChange_modifyTransition : NodeId -> NodeId -> Connection -> AutomatonGraph a -> AutomatonGraph a
    applyChange_modifyTransition from to newState g =
      let
        newGraph = modifyConnection from to newState g.graph
      in
        { graph = newGraph
        , maxId = List.maximum (Graph.nodes newGraph |> List.map .id) |> Maybe.withDefault 0
        , root = g.root
        }
    mkSim g model =
      let
        (w, h)  = model_.dimensions
        forceGraph = toForceGraph (g {- |> Debug.log "Received by ForceDirectedGraph" -} )
        basic = basicForces g.root forceGraph (round h)
        viewport = viewportForces (w, h) forceGraph
      in
        { model
          | simulation = Force.simulation (basic ++ viewport)
          , basicForces = basic
          , graph = forceGraph
          , viewportForces = viewport
          , specificForces = IntDict.empty
        }
  in
  List.foldl
    (\change g ->
        case change of
          AddNewNode { newNodeId } ->
            applyChange_addOrLinkNode newNodeId g
          NewLinkToNode { to } ->
            applyChange_addOrLinkNode to g
          ModifyTransition { from, to, newState } ->
            applyChange_modifyTransition from to newState g
          RemoveNode nodeId ->
            -- This one is a fairly low-level and brutal operation.  Since the node IDs are the same
            -- in the Automaton graph and in the DFA, we can just remove the node from the DFA
            -- and then recalculate the graph from that.
            Graph.get nodeId ag.graph
            |> Maybe.map
              (\node ->
                IntDict.toList node.incoming
                |> List.map (\(from, conn) -> (from, node.node.id, conn))
              )
            |> Maybe.withDefault []
            |> List.foldl
              (\(from, to, conn) -> applyChange_modifyTransition from to conn)
              g
    )
    ag
    (List.reverse model_.userRequestedChanges)
  |> \g ->
    { model_
    | userRequestedChanges = []
    , start = g.root
    , unusedId = g.maxId + 1
    } |> mkSim g

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
        [ class [ "nonfinal" ]
        ]
        [ text <| textChar ch
        ]
    (ch, _) ->
      tspan
        [ class [ "final" ]
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
  Graph.get from model.graph
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


viewLink : Model -> Edge Connection -> Svg Msg
viewLink ({ graph } as model) edge =
  let
    source =
      Maybe.withDefault (Force.entity 0 { }) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

    target =
      Maybe.withDefault (Force.entity 0 { }) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    cardinality = identifyCardinality model edge
    positioning =
      path_between source target cardinality 7 7
    font_size = 16.0 -- this is the default, if not otherwise set
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
          , class [ "link" ]
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
          ( title [] [ text "Click to modify" ] :: connectionToSvgText edge.label )
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
viewNode { start, graph, selectedSource, selectedDest } { label, id } =
  let
    isSelected = selectedSource == Just id
    graphNode =
      Graph.get id graph
    thisNodeIsTerminal =
      Maybe.map Automata.Data.isTerminalNode graphNode
      |> Maybe.withDefault False
    permit_node_reselection =
      Mouse.onWithOptions
        "mousedown"
        { stopPropagation = True, preventDefault = True }
        (\e ->
          if e.keys.shift then
            e.clientPos |> DragStart id
          else
            case selectedSource of
              Just _ ->
                -- ANY node is fine!  If it's the same node, that's also fine.  Recursive links are okay.
                CreateOrUpdateLinkTo id
              Nothing ->
                SelectNode id
        )

    interactivity =
      case selectedDest of
        NoDestination ->
          [ permit_node_reselection ]
        _ ->
          []
  in
    g
      ( class ("state-node" :: if isSelected then [ "selected" ] else [])
      ::interactivity
      )
      [ circle
          [ r nodeRadius
          , strokeWidth 2
          , cx label.x
          , cy label.y
          , class
              ( if id == start then [ "start" ] else [] )
          ]
          []
       ,  if thisNodeIsTerminal && id == start then
            text_
              [ x <| label.x
              , y <| (label.y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              ]
              [ text "💥"
              , title
                  []
                  [ text <| "Start AND end of computation"
                    ++ "\n(" ++ String.fromInt id ++ ")" -- DEBUGGING
                  ]
              ]
          else if thisNodeIsTerminal then
            text_
              [ x <| label.x
              , y <| (label.y + 1)
              , fontFamily ["sans-serif"]
              , fontSize 14
              , fontWeight FontWeightNormal
              , textAnchor AnchorMiddle
              , alignmentBaseline AlignmentBaseline
              , dominantBaseline DominantBaselineMiddle
              , Html.Attributes.attribute "paint-order" "stroke fill markers"
              ]
              [ text "🎯"
              , title
                  []
                  [ text <| "End of computation"
                    ++ "\n(" ++ String.fromInt id ++ ")" -- DEBUGGING
                  ]
              ]
          else if id == start then
            text_
              [ x <| label.x
              , y <| (label.y + 1)
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
              , title
                  []
                  [ text <| "Start of computation"
                    ++ "\n(" ++ String.fromInt id ++ ")" -- DEBUGGING
                  ]
              ]
          else
            g [] []
      , title
          []
          [ text <|
              -- String.fromInt node.id
              "Shift-drag to reposition\nClick to create or link a new transition"
              ++ "\n(" ++ String.fromInt id ++ ")" -- DEBUGGING
          ]
      ]

nearby_node : Model -> Maybe (Graph.Node Entity)
nearby_node { graph, pan, mouseCoords } =
  let
    ( xPan, yPan ) = pan
    ( mouse_x, mouse_y ) = mouseCoords
  in
  Graph.nodes graph
  |> List.find
    (\node ->
      let
        dx = (node.label.x - xPan) - mouse_x
        dy = (node.label.y - yPan) - mouse_y
      in
        -- Debug.log ("Checking (" ++ String.fromFloat node.label.x ++ ", " ++ String.fromFloat node.label.y ++ ") against (" ++ String.fromFloat mouse_x ++ ", " ++ String.fromFloat mouse_y ++ ")") () |> \_ ->
        dx * dx + dy * dy <= 16*16 -- 7 + 9 = 16
    )

{-| A "phantom move" that the user MIGHT make, or might not -}
viewPhantom : Model -> NodeContext Entity Connection -> Svg Msg
viewPhantom model sourceNode =
  let
    radius = 9    
    ( xPan, yPan ) = model.pan
    nearby = nearby_node model
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
viewSingleKey : Model -> Char -> (Int, Int) -> Svg Msg
viewSingleKey model ch (gridX, gridY) =
  let
    buttonX = transition_spacing * toFloat (gridX + 1) + transition_buttonSize * toFloat gridX
    buttonY = transition_spacing * toFloat (gridY + 1) + transition_buttonSize * toFloat gridY
    isThisNodeTerminal = Set.member (ch, 1) model.selectedTransitions
    keyClass =
      if Set.member (ch, 0) model.selectedTransitions then
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
        , onClick <| ToggleSelectedTransition ch
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
              ( if Set.isEmpty model.selectedTransitions then
                  [ tspan [] [ text "None" ] ]
                else
                  connectionToSvgText model.selectedTransitions
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
            ( if Set.isEmpty model.selectedTransitions then
                [ tspan [] [ text "If there are no transitions, this link will be destroyed." ] ]
              else
                [ tspan [] [ text "Press «Enter» to confirm these transitions" ] ]
            )
        ]
    :: List.map (\(item, col, row) -> viewSingleKey model item (col, row)) gridItemsAndCoordinates
    )

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
              case model.selectedSource of
                Just _ -> Cursor "none"
                _ -> CursorDefault
      ]
    permit_node_selection =
      Mouse.onWithOptions
        "mousedown"
        { stopPropagation = True, preventDefault = True }
        (\_ ->
          case model.selectedSource of
            Just _ ->
              case nearby_node model of
                Just node -> -- in this case, the UI would show it being "locked-on"
                  CreateOrUpdateLinkTo node.id
                Nothing ->
                  CreateNewNodeAt model.mouseCoords
            Nothing ->
              Escape
        )
    interactivity =
      case model.selectedDest of
        NoDestination ->
          permit_node_selection :: permit_zoom :: permit_pan
        _ ->
          []
  in
  svg
    ([ viewBox 0 0 (Tuple.first model.dimensions) (Tuple.second model.dimensions)
    , Mouse.onOver (\_ -> SetMouseOver)
    , Mouse.onOut (\_ -> SetMouseOut)
    ] ++ interactivity)
    [ g
      [ transform [ matrixFromZoom model.dimensions model.pan model.zoom model.mouseCoords ]
      ]
      [ defs [] [ arrowheadMarker, phantomArrowheadMarker ]
      , Graph.edges model.graph
        |> List.map (viewLink model)
        |> g [ class [ "links" ] ]
      , Graph.nodes model.graph
        |> List.map (viewNode model)
        |> g [ class [ "nodes" ] ]
      , case ( model.selectedSource, model.selectedDest ) of
          ( _, EditingTransitionTo _ ) ->
            g [] []
          ( Just id, _ ) ->
            Graph.get id model.graph
            |> Maybe.map (viewPhantom model)
            |> Maybe.withDefault (g [] [])
          ( Nothing, _ ) ->
            g [] []
      ]
    , case model.selectedDest of
        NoDestination ->
          g [] []
        _ ->
          viewSvgTransitionChooser model
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
            [ text (" 🧭 " ++
                (case Tuple.mapBoth round round model.pan of
                  ( 0, 0 ) -> "centered"
                  ( x, y) ->
                    let
                      xString = if x > 0 then "+" ++ String.fromInt x else String.fromInt x
                      yString = if y > 0 then "+" ++ String.fromInt y else String.fromInt y
                    in
                    ("(" ++ xString ++ ", " ++ yString ++ ")")
                )
              )
            ]
        , case (model.selectedSource, model.selectedDest) of
            (Just _, NoDestination) ->
              bottomMsg "Press «Esc» to cancel link creation"
            (_, ExistingNode _) ->
              bottomMsg "Choose transitions to connect these nodes. Press «Esc» to cancel."
            (_, NewNode _) ->
              bottomMsg "Choose transitions for this link. Press «Esc» to cancel."
            (_, EditingTransitionTo _) ->
              bottomMsg "Choose transitions for this link. Press «Esc» to cancel."
            _ ->
              case model.userRequestedChanges of
                [] ->
                  g [] []
                _ ->
                  bottomMsg "Press «Enter» to apply these changes."
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
              (Decode.field "offsetX" Decode.float)
              (Decode.field "offsetY" Decode.float)
          else
            Decode.fail "Ignoring non-SVG target"
        )
    )