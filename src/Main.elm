module Main exposing (..)
import Browser
import Browser.Events as BE
import Html.Styled exposing
  (Html, div, h1, p, ul, li, input, textarea, span, toUnstyled, text, button)
import Html.Styled.Events as HE
import Json.Encode as E
import Json.Decode as D
-- import GraphEditor exposing (..)
import Automata.Data exposing (..)
import Platform.Cmd as Cmd
import Html.Styled.Attributes as HA
import Html
import Maybe.Extra as Maybe
import Css
import Platform.Cmd as Cmd
import Uuid
import Random.Pcg.Extended as Random
import Time
import Ports exposing (..)
import Platform.Cmd as Cmd
import Automata.DFA as DFA
import TypedSvg exposing (g)
import TypedSvg.Attributes.InPx exposing (x, y, height, width)
import TypedSvg.Types exposing
  (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..)
  , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..))
import Html.Styled exposing (h2, h4)
import AutoSet
import AutoDict
import Uuid exposing (Uuid)
import Basics.Extra exposing (..)
import Automata.Debugging
import IntDict
import Set
import Graph exposing (NodeId)
import Force
import Automata.Debugging exposing (debugAutomatonGraph)
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes
import Automata.Debugging exposing (println)
import GraphEditor
import Dict exposing (Dict)
import Jsonify exposing (..)
import Graph exposing (Graph)
import Css exposing (px)
import Basics.Extra exposing (maxSafeInteger, minSafeInteger)
import WebGL exposing (entityWith)
import List.Extra as List
import GraphEditor exposing (viewGraph)
import Automata.Debugging exposing (debugLog_)
import Automata.Debugging exposing (debugAutomatonGraphXY)
import Automata.Debugging exposing (debugGraph)
import Browser.Dom
import Task
import Process
import TypedSvg.Attributes
import Html.Styled exposing (ol)
import Math.Vector2 exposing (distance)

{-
Quality / technology requirements:

1. Use CSS3, HTML5, and classes.  The file `style.css` can be modified with the
   correct styles.
2. Html.Styled should be used to maintain type safety, unless that is impossible
   or impractical.  If impossible or impractical, a comment should be left to
   explain why that is the case.
-}

canExecute : Model -> Uuid -> Bool
canExecute { graph_views } uuid =
  AutoDict.get uuid graph_views
  |> Maybe.map (.undoBuffer >> List.isEmpty)
  |> Maybe.withDefault False

getUuid : Model -> (Uuid, Model)
getUuid model =
  let
    (v, newSeed) =
      Random.step
      --(Random.int Basics.minSafeInteger Basics.maxSafeInteger)
      Uuid.generator
      model.randomSeed
    updatedModel =
      { model | randomSeed = newSeed }
  in
    ( v, updatedModel )

getUuid2 : Model -> (Uuid, Uuid, Model)
getUuid2 model =
  let
    (a, m) = getUuid model
    (b, m_) = getUuid m
  in
    ( a, b, m_ )

nodeDrawingForPackage : AutomatonGraph -> Uuid -> InteractionsDict -> Dict NodeId NodeDrawingData
nodeDrawingForPackage ag graphView_uuid interactions =
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
          , { exclusiveAttributes =
                case peekInteraction (Just graphView_uuid) interactions of
                  Just (DraggingNode node_id) ->
                    maybe_fromBool
                      (node.id == node_id)
                      DrawSelected
                  Just (Executing (result::_) _) ->
                    maybe_fromBool
                      ( result.currentNode == node.id )
                      DrawCurrentExecutionNode
                  Just (ChoosingDestinationFor _ (NewNode phantom _)) ->
                    maybe_fromBool
                      (node.id == phantom)
                      DrawPhantom
                  _ ->
                    Nothing
            , isDisconnected = Set.member node.id disconnectedNodes
            , isTerminal =
                Maybe.map isTerminalNode nodeContext
                |> Maybe.withDefault False
            , coordinates = ( node.label.x, node.label.y )
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

linkExistsInGraph : Graph.NodeContext Entity Connection -> NodeId -> Bool
linkExistsInGraph from to =
  -- does a link exist from `from` to `to`?
  IntDict.member to from.outgoing
  -- |> Debug.log ("Checking for #" ++ String.fromInt to ++ " in #" ++ String.fromInt from.node.id ++ "'s outgoing list " ++ Debug.toString (IntDict.keys from.outgoing))

identifyCardinalityViaContext : NodeId -> Graph.NodeContext Entity Connection -> Cardinality
identifyCardinalityViaContext from to =
  if to.node.id == from then
    Recursive
  else if linkExistsInGraph to from then -- i.e. in opposite direction
    Bidirectional
  else
    Unidirectional

identifyCardinality : NodeId -> NodeId -> GraphView -> Cardinality
identifyCardinality from to {computation} =
  Graph.get to computation.graph
  |> Maybe.map
    (\toContext -> identifyCardinalityViaContext from toContext)
  |> Maybe.withDefault Unidirectional

descriptionsForConnection : Connection -> PackageDict -> AutoDict.Dict String Uuid String
descriptionsForConnection connection packages =
  AutoSet.toList connection
  |> List.filterMap
    (\{via} ->
        case via of
          ViaCharacter _ ->
            Nothing
          ViaGraphReference uuid ->
            AutoDict.get uuid packages
            |> Maybe.andThen (.computation >> .description)
            |> Maybe.map (\s -> ( uuid, s ))
    )
  |> AutoDict.fromList Uuid.toString

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
          descriptionsForConnection connection packages
      , connection = connection
      , pathBetween =
          path_between
            sourceNode.node.label
            destNode.node.label
            cardinality
      , highlighting = Nothing
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

fitGraphViewToGraph : GraphView -> GraphView
fitGraphViewToGraph graphView =
  let
    guest_viewport =
      calculateGuestDimensionsForHost
        graphView.host_dimensions
        graphView.fitClosely -- we are fitting closely around.
        graphView.computation.graph
  in
    { graphView
    | guest_dimensions = guest_viewport.dimensions
    , guest_coordinates = guest_viewport.coordinates
    }

mkGraphView : Uuid -> AutomatonGraph -> (Float, Float) -> Bool -> InterfaceLocation -> PackageDict -> InteractionsDict -> GraphView
mkGraphView id ag (w, h) removePadding location packages interactions =
  let
    -- now that we have the positions, calculate the dimensions of
    -- the viewport.
    -- first, let's get the aspect ratio.  That will let us figure out
    -- the height of the viewport "automatically" once we have the right width.
    guest_viewport =
      calculateGuestDimensionsForHost
        (w, h)
        removePadding
        ag.graph
  in
    { id = id
    , computation = ag
    , graphPackage = Nothing
    , interfaceLocation = location
    , fitClosely = removePadding
    , host_dimensions = (w, h)
    , host_coordinates = (-1, -1)
    , guest_dimensions = guest_viewport.dimensions
    , guest_coordinates = guest_viewport.coordinates
    , pan = ( 0, 0 )
    , activePanDirection = Nothing
    , disconnectedNodes = Set.empty
    , properties = nilViewProperties
    , drawingData =
        { link_drawing = linkDrawingForPackage ag packages
        , node_drawing = nodeDrawingForPackage ag id interactions
        }
    , undoBuffer = []
    , redoBuffer = []
    }

upsertGraphView : Uuid -> GraphView -> Model -> Model
upsertGraphView uuid graph_view model =
  let
    new_main =
      if graph_view.interfaceLocation == MainEditor then
        uuid
      else
        model.mainGraphView
  in
  { model
    | graph_views =
        AutoDict.insert uuid { graph_view | id = uuid } model.graph_views
    , mainGraphView = new_main
  }

{-| Probably not the function you want. Look at `solvedViewFromPackage` and
    `naiveViewFromPackage` instead.
-}
viewFromComputation : GraphEditor.ComputeGraphResult -> (Float, Float) -> InterfaceLocation -> Bool -> Model -> (GraphView, Model)
viewFromComputation computed (w, h) location removePadding model =
  let
    (id, model_) = getUuid model
    graph_view =
      mkGraphView id computed.solvedGraph (w, h) removePadding location model_.packages model_.interactionsDict
  in
    ( graph_view
    , upsertGraphView id graph_view model_
    )

{-| Creates a new GraphView from a provided GraphPackage, accepting the provided layout
    uncritically, and adds the GraphView to the `graph_views` dictionary in the `Model`.
-}
naiveViewFromComputation : (Float, Float) -> InterfaceLocation -> Bool -> AutomatonGraph -> Model -> (GraphView, Model)
naiveViewFromComputation (w, h) location createFrozen ag model =
  viewFromComputation
    { solvedGraph = ag
    , simulation = Force.simulation []
    , forces = []
    }
    (w, h) location createFrozen model

{-| Creates a new GraphView from a provided GraphPackage, solves the forces for the relevant
    AutomatonGraph, and adds the GraphView to the `graph_views` dictionary in the `Model`.
-}
solvedViewFromComputation : (AutomatonGraph -> List (Force.Force NodeId)) -> (Float, Float) -> InterfaceLocation -> Bool -> AutomatonGraph -> Model -> (GraphView, Model)
solvedViewFromComputation computer (w, h) location createFrozen ag model =
  viewFromComputation
    (GraphEditor.computeGraphFully computer ag)
    (w, h) location createFrozen model

linkToPackage : Uuid -> (GraphView, Model) -> (GraphView, Model)
linkToPackage package_uuid (graph_view, model) =
    if AutoDict.member package_uuid model.packages then
      let
        gv =
          { graph_view | graphPackage = Just package_uuid }
      in
        (  gv , upsertGraphView gv.id gv model )
    else
      ( graph_view, model )

centerAndHighlight : List (NodeId, NodeId) -> GraphView -> GraphView
centerAndHighlight links graph_view =
  let
    bounds =
      bounds_for
        (List.foldl (\(a, b) acc -> a :: b :: acc) [] links)
        graph_view.computation.graph
    inner_padding = 60
    adjusted =
      expand_bounds inner_padding bounds
    aspect_ratio =
      Tuple.first graph_view.host_dimensions / Tuple.second graph_view.host_dimensions
    last_node =
      List.last links
      |> Maybe.map Tuple.second
      |> Maybe.withDefault (graph_view.computation.root)
    linkSet = Set.fromList links
    determine_dimensions (x, y) (w, h) =
      if w / Tuple.first graph_view.host_dimensions < 0.25 then
        -- expand to at least this amount.
        let
          new_w = 0.25 * Tuple.first graph_view.host_dimensions
          -- so, how much do I need to expand on either side?
          diff = (new_w - w) / 2
        in
          ((x - diff, y), (new_w, new_w / aspect_ratio))
      else
        ((x, y), (w, h))
    ( guest_coords, guest_dims ) =
      determine_dimensions
        (adjusted.min.x, adjusted.min.y)
        (adjusted.max.x - adjusted.min.x, (adjusted.max.x - adjusted.min.x) / aspect_ratio)
  in
    { graph_view
      | drawingData =
          let drawingData = graph_view.drawingData in
            { drawingData
              | link_drawing =
                  Dict.map (\(src, dest) link ->
                    if Set.member (src, dest) linkSet then
                      -- this is a link to highlight.
                      { link | highlighting = Just Highlight }
                    else if src == last_node then
                      { link | highlighting = Nothing }
                    else
                      { link | highlighting = Just Lowlight }
                  )
                  drawingData.link_drawing
            }
      , guest_coordinates = guest_coords
          -- ( adjusted.min.x, adjusted.min.y )
      , guest_dimensions = guest_dims
          -- ( adjusted.max.x - adjusted.min.x, (adjusted.max.x - adjusted.min.x) / aspect_ratio )
    }

-- MAIN

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }

createNewPackage : Uuid -> Uuid -> Time.Posix -> AutomatonGraph -> GraphPackage
createNewPackage testUuid packageUuid currentTime g = -- `dimensions` is the width & height of the panel
  { computation = g
  , packageIdentifier = packageUuid
  , created = currentTime
  , currentTestKey = testUuid
  , tests = AutoDict.empty Uuid.toString
  }

init : E.Value -> (Model, Cmd Msg)
init flags =
  let
    decoded =
      D.decodeValue decodeFlags flags
      |> Result.mapError (\err -> Debug.log "OH NO! FLAGS WAS NOT PARSED CORRECTLY!!" err)
      |> Result.withDefault (Flags 800 600 0 [] (Time.millisToPosix 0) [])
    
    (packages, mainPackage, initialSeed) =
      let
        seed0 = Random.initialSeed decoded.initialSeed decoded.extendedSeeds
        allPackagesList =
          decoded.packages
          -- |> List.map (\v -> Automata.Debugging.debugAutomatonGraph "Loaded" v.model.computation |> \_ -> v)
          |> List.map (\v -> ( v.packageIdentifier, v ))
        allPackagesDict =
          AutoDict.fromList Uuid.toString allPackagesList
          -- for the initial graph, choose the most recent graph.
          -- if one doesn't exist, then let's make one and see how we go.
      in
        case List.sortBy (.created >> Time.posixToMillis >> (*) -1) decoded.packages |> List.head of
          Nothing -> -- ooh. No packages exist!
            -- I'd better make a new one.
            let
              (testUuid, seed1) = Random.step Uuid.generator seed0
              (mainUuid, seed2) = Random.step Uuid.generator seed1
              pkg =
                createNewPackage
                  testUuid
                  mainUuid
                  decoded.startTime
                  ( { graph =
                        Graph.fromNodesAndEdges
                          [ Graph.Node 0 (entity 0 NoEffect |> (\e -> { e | x = 0, y = 0 }))
                          ]
                          []
                    , description = Nothing
                    , root = 0
                    }
                    |> debugAutomatonGraphXY "[init] new initial graph"
                  )
            in
              ( AutoDict.insert mainUuid pkg allPackagesDict
              , pkg
              , seed2
              )
          Just pkg ->
            ( allPackagesDict
            , pkg
            , seed0
            )
    constants : UIConstants
    constants =
      { sideBarWidth =
          { min = 100
          , max = decoded.width / 2 - 60
          , initial = clamp 100 (decoded.width / 2 - 60) 300
          }
      , toolsPanelHeight =
          { min = 80
          , max = decoded.height / 2 - 40
          , initial = clamp 80 (decoded.height / 2 - 40) 200
          }
      }
    state : UILayout
    state =
      { dimensions =
          { sideBar = ( constants.sideBarWidth.initial, decoded.height )
          , bottomPanel =
              ( decoded.width - constants.sideBarWidth.initial - 8 - 48
              , constants.toolsPanelHeight.initial
              )
          , mainEditor =
              ( decoded.width - constants.sideBarWidth.initial - 8 - 48
              , decoded.height - constants.toolsPanelHeight.initial - 8
              )
          , viewport = ( decoded.width, decoded.height )
          }
      , open =
          { bottomPanel = True
          , sideBar = True
          }
      , selected =
        { bottomPanel = TestingToolIcon
        , sideBar = ComputationsIcon
        }
      }
    model_excl_views : Model
    model_excl_views =
      { graph_views =
          AutoDict.empty Uuid.toString
      , mainGraphView = mainPackage.packageIdentifier -- this is—temporarily—the wrong value!
      , selectedPackage = mainPackage.packageIdentifier
      , packages = packages
      , uiLayout = state
      , uiConstants = constants
      , randomSeed = initialSeed
      , interactionsDict = AutoDict.empty (Maybe.map Uuid.toString >> Maybe.withDefault "")
      , properties = nilMainProperties
      , computationsExplorer = []
      }
    model =
      -- I _know_ that this will succeed, because I've added this
      -- exact one
      let
        ( v, model_with_viewDict) =
          solvedViewFromComputation
            GraphEditor.coordinateForces
            state.dimensions.mainEditor
            MainEditor
            False
            mainPackage.computation
            model_excl_views
          |> linkToPackage mainPackage.packageIdentifier
      in
        { model_with_viewDict | mainGraphView = v.id }
  in
    ( selectNavIcon ComputationsIcon model
      |> refreshComputationsList
      |> setProperties
    , Cmd.none
    )

updateGraphInView : (AutomatonGraph -> AutomatonGraph) -> GraphView -> Model -> Model
updateGraphInView updater graph_view model =
  let
    ag = updater graph_view.computation
  in
  upsertGraphView graph_view.id
    ( { graph_view
        | computation = ag
        , drawingData =
            { link_drawing = linkDrawingForPackage ag model.packages
            , node_drawing = nodeDrawingForPackage ag graph_view.id model.interactionsDict
            }
        , disconnectedNodes = DFA.identifyDisconnectedNodes ag
      }
      |> fitGraphViewToGraph
    )
    model

-- UPDATE

{-| Note: EVERYWHERE that I use persistPackage, I should ALSO
    update the `packages` dictionary!

    (yes, I should one day figure out an automagic way to do this…
     maybe. but it's in so few places right now that hey, YAGNI?)
-}
persistPackage : GraphPackage -> Cmd Msg
persistPackage =
  Ports.saveToStorage << encodeGraphPackage

updateGraphView : Uuid -> (GraphView -> Maybe GraphView) -> AutoDict.Dict String Uuid GraphView -> AutoDict.Dict String Uuid GraphView
updateGraphView uuid f dict =
  AutoDict.get uuid dict
  |> Maybe.andThen f
  |> Maybe.map (\updatedView -> AutoDict.insert uuid updatedView dict)
  |> Maybe.withDefault dict

{-
updateGraphView : Uuid -> GraphView_Msg -> Model -> Model
updateGraphView uuid msg model =
  case msg of
    Zoom amount ->
      let
        zoomAmount = if amount < 0 then 0.05 else -0.05
        newZoom = clamp 0.5 2.5 (model.zoom + zoomAmount)
      in
        if newZoom == model.zoom then
          model
        else
          { model | zoom = newZoom }

    ResetView ->
      { model | zoom = 1.0, pan = ( 0, 0 ) }
    
    Reheat ->
      -- If I'm not doing anything else, permit auto-layout
      case model.interactionsDict of
        Nothing ->
          { model
          | simulation = Force.simulation (model.basicForces ++ model.viewportForces)
          , specificForces = IntDict.empty -- cancel moves that were made before
          }
        _ ->
          model
        
    RunComputation ->
      let
        pkg = model.package
      in
        case model.interactionsDict of
          Just (Executing original executionResult) ->
            { model
              | interactionsDict = Just <| Executing original (DFA.run executionResult)
              , currentPackage =
                  { pkg
                    | computation =
                        executionResultAutomatonGraph executionResult
                        |> Maybe.withDefault pkg.computation
                  }
            }
          _ ->
            model -- 'run' isn't valid in any other context.

    StepExecution ->
      let
        pkg = model.package
      in
        case model.interactionsDict of
          Just (Executing original executionResult) ->
            { model
              | interactionsDict = Just <| Executing original (DFA.step executionResult)
              , currentPackage =
                  { pkg
                    | computation =
                        executionResultAutomatonGraph executionResult
                        |> Maybe.withDefault pkg.computation
                        |> debugAutomatonGraph "After step"
                  }
            }
          _ ->
            model -- 'step' isn't valid in any other context.

    StopExecution ->
      let
        pkg = model.package
      in
        case model.interactionsDict of
          Just (Executing original _) ->
            { model
              | interactionsDict = Nothing
              , currentPackage =
                  { pkg
                    | computation = original
                  }
            }
          _ ->
            model -- 'stop' isn't valid in any other context.
-}

{-| When either the sidebar or the bottom-panel have changed dimensions, this should be
    called to figure out what changes need to be made to any of the other dimensions.
-}
recalculate_uistate : UILayout -> UILayout
recalculate_uistate ({dimensions} as ui) =
  let
    sidebar_width =
        Tuple.first dimensions.sideBar
    panel_height =
        Tuple.second dimensions.bottomPanel
    visible_sidebar_width_plus_splitter =
      if ui.open.sideBar then sidebar_width + 8 + 48 else 0
    visible_panel_height_plus_splitter =
      if ui.open.bottomPanel then panel_height + 8 else 0
  in
  { ui
    | dimensions =
        { dimensions
          | bottomPanel =
              ( Tuple.first dimensions.viewport - visible_sidebar_width_plus_splitter
              , panel_height
              )
          , mainEditor =
              ( Tuple.first dimensions.viewport - visible_sidebar_width_plus_splitter
              , Tuple.second dimensions.viewport - visible_panel_height_plus_splitter
              )
        }
  }

type alias GuestDimensions =
  { dimensions : (Float, Float)
  , coordinates : (Float, Float)
  , inner_coordinates : (Float, Float)
  , inner_dimensions : (Float, Float)
  }

type alias Bounds =
  { min : Coordinate {}
  , max : Coordinate {}
  }

bounds_for : List NodeId -> Graph.Graph Entity Connection -> Bounds
bounds_for nodeIds graph =
  let
    contexts =
      (Set.fromList >> Set.toList) nodeIds
      |> List.filterMap (\id -> Graph.get id graph)
      -- |> debugLog_ "[bounds_for] contexts to check" (List.map (.node >> .label))
  in
  -- Now find out: where is the bounding box for the nodes?
    case contexts of
      [] ->
        { min = { x = 0, y = 0 }, max = { x = 0, y = 0 } }
        -- |> Debug.log "Default Bounds"
      h::t ->
        List.foldl
          (\ctx best ->
            { min =
                { x = min ctx.node.label.x best.min.x
                , y = min ctx.node.label.y best.min.y
                }
            , max =
                { x = max ctx.node.label.x best.max.x
                , y = max ctx.node.label.y best.max.y
                }
            }
          )
          { min = { x = h.node.label.x, y = h.node.label.y }
          , max = { x = h.node.label.x, y = h.node.label.y }
          }
          t
          -- |> Debug.log "[bounds_for] Raw Bounds"

expand_bounds : Float -> Bounds -> Bounds
expand_bounds n bounds =
  { bounds
    | min = { x = bounds.min.x - n, y = bounds.min.y - n }
    , max = { x = bounds.max.x + n, y = bounds.max.y + n }
  }

{-| Accepts host dimensions, view properties, and a graph, and calculates the
    appropriate guest coordinates & guest dimensions (i.e. viewport).
-}
calculateGuestDimensionsForHost : (Float, Float) -> Bool -> Graph.Graph Entity Connection -> GuestDimensions
calculateGuestDimensionsForHost (w, h) removePadding graph =
  let
    aspectRatio : Float
    aspectRatio =
      w / h
      -- |> Debug.log "Viewport aspect-ratio"
    raw =
      bounds_for (Graph.nodeIds graph) graph
    inner_pad : Float -- 85-105 in SVG-coordinates seems to be a "good" amount of space
    inner_pad =
      if removePadding then
        -- we don't need any buffer.
        -- So, just put in a "buffer" for the node radius.
        if raw.max.x - raw.min.x < 60 then
          60 -- this is the minimum amount. It is enough to show a single node.
        else
          15
      else
        -- when we edit (e.g. make new nodes etc), we want some free space around to put
        -- those nodes.  That is what this is for.
        95
    -- the forces on the graph place the root at (0, 0).
    -- they also pull all the other nodes to the right and to the
    -- y-axis center.
    -- So I can take the number of nodes and multiply by, say, 150 and
    -- I shouldn't be too far off from a maximum.
    adjusted =
      expand_bounds inner_pad raw
      -- |> Debug.log "After-padding Bounds"
    -- from this, I can figure out the appropriate coordinates
    center_y =
      (adjusted.min.y + adjusted.max.y) / 2
      -- |> Debug.log "center-y"
    autoHeight =
      max
        ((adjusted.max.x - adjusted.min.x) / aspectRatio) -- aspect-ratio calc
        (raw.max.y - raw.min.y) -- bounds calc
      -- |> Debug.log "Auto-height (via aspect-ratio)"
    -- now, we want a center within that autoHeight.
    guestCoordinates =
      ( adjusted.min.x, center_y - autoHeight / 2 )
      -- |> Debug.log "Top-left (X,Y) of SVG viewport"
    guestDimensions =
      ( adjusted.max.x - adjusted.min.x, autoHeight )
      -- |> Debug.log "(Width, Height) of SVG viewport" 
    pad_inner_x =
      0.15 * (raw.max.x - raw.min.x)
      -- |> Debug.log "Inner-rectangle X padding (for panning)"
    pad_inner_y =
      0.15 * (raw.max.y - raw.min.y)
      -- |> Debug.log "Inner-rectangle Y padding (for panning)"
    x_inner =
      raw.min.x + pad_inner_x
      -- |> Debug.log "Inner-rectangle X (for panning)"
    y_inner =
      raw.min.y + pad_inner_y
      -- |> Debug.log "Inner-rectangle Y (for panning)"
    width_inner =
      (raw.max.x - pad_inner_x * 2) - raw.min.x
      -- |> Debug.log "Inner-rectangle width (for panning)"
    height_inner =
      (raw.max.y - pad_inner_y * 2) - raw.min.y
      -- |> Debug.log "Inner-rectangle height (for panning)"
  in
    { dimensions = guestDimensions
    , coordinates = guestCoordinates
    , inner_coordinates = ( x_inner, y_inner )
    , inner_dimensions = ( width_inner, height_inner )
    }

sidebarGraphDimensions : UILayout -> ( Float, Float )
sidebarGraphDimensions uiLayout =
  let
    ( w, _ ) = uiLayout.dimensions.sideBar
  in
  ( w - 20 , 9/16 * ( w - 20 ) )

handleConnectionRemoval : Uuid -> ConnectionAlteration -> List Uuid -> Model -> Model
handleConnectionRemoval uuid {source, dest, connection, deleteTargetIfCancelled} viewsToRemove model =
  if deleteTargetIfCancelled then
    removeViews viewsToRemove model
    |> deleteNodeFromView uuid source dest
  else
    -- does link deletion IF appropriate.
    removeViews viewsToRemove model
    |> deleteLinkFromView uuid source dest connection


updateMainEditorDimensions : Model -> Model
updateMainEditorDimensions ({uiLayout} as model) =
  let
    updateMain : GraphView -> GraphView
    updateMain graph_view =
      let
        guest_viewport =
          calculateGuestDimensionsForHost
            uiLayout.dimensions.mainEditor
            False
            graph_view.computation.graph
      in
        { graph_view
          | host_dimensions = uiLayout.dimensions.mainEditor
          , guest_dimensions = guest_viewport.dimensions
          , guest_coordinates = guest_viewport.coordinates
          , host_coordinates =
              if uiLayout.open.sideBar then
                (Tuple.first uiLayout.dimensions.sideBar + 8 + 48, 0)
              else
                (0, 0)
        }
    updateSidebar : GraphView -> GraphView
    updateSidebar graph_view =
      let
        sidebarDimensions =
          sidebarGraphDimensions uiLayout
        guest_viewport =
          calculateGuestDimensionsForHost
            sidebarDimensions
            True
            graph_view.computation.graph
      in
        { graph_view
          | host_dimensions = sidebarDimensions
          , guest_dimensions = guest_viewport.dimensions
          , guest_coordinates = guest_viewport.coordinates
          , host_coordinates =
              (0, 0) -- this is nonsense. But since I'm not panning in these, I think it's fine.
        }
  in
    -- On resize, I must update the dimensions for the graph view that
    -- is displayed in the "main" editor section as well.
    { model
      | graph_views =
          AutoDict.map
            (\_ graph_view ->
                case graph_view.interfaceLocation of
                  MainEditor ->
                    updateMain graph_view
                  Sidebar ->
                    if uiLayout.open.sideBar then
                      updateSidebar graph_view
                    else
                      graph_view -- don't bother.
                  Independent ->
                    -- no resizing.
                    graph_view
            )
            model.graph_views
    }

{-| Drag a specified splitter to a specified coordinate. Returns an updated `UIState`.
-}
dragSplitter : Float -> SplitterMovement -> UIConstants -> UILayout -> UILayout
dragSplitter coord movement constants ({dimensions} as ui) =
  let
    sidebar_width =
      if movement == LeftRight then
        clamp constants.sideBarWidth.min constants.sideBarWidth.max coord
      else
        Tuple.first dimensions.sideBar
    panel_height =
      if movement == UpDown then
        clamp constants.toolsPanelHeight.min constants.toolsPanelHeight.max (Tuple.second dimensions.viewport - 8 - coord)
      else
        Tuple.second dimensions.bottomPanel
  in
    { ui
      | dimensions =
          { dimensions
            | sideBar =
                ( sidebar_width, Tuple.second dimensions.sideBar )
            , bottomPanel =
                ( Tuple.first dimensions.bottomPanel, panel_height )
          }
    }
    |> recalculate_uistate

{-| Collapse or expand an area (i.e. either sidebar or bottom-panel).
    Returns the updated `UIState` with correct dimensions.
-}
toggleAreaVisibility : AreaUITarget -> UILayout -> UILayout
toggleAreaVisibility where_ ({open} as ui) =
  { ui
    | open =
        { open
          | sideBar =
              if where_ == NavigatorsArea then not open.sideBar else open.sideBar
          , bottomPanel =
              if where_ == ToolsArea then not open.bottomPanel else open.bottomPanel
        }
  }
  |> recalculate_uistate

resizeViewport : (Float, Float) -> Model -> Model
resizeViewport (w, h) ({uiLayout, uiConstants} as model) =
  let
    dim = uiLayout.dimensions
    constants : UIConstants
    constants =
      { uiConstants
        | sideBarWidth =
            let sbw = uiConstants.sideBarWidth in
            { sbw
              | max = w / 2 - 60
              , initial = clamp sbw.min (w / 2 - 60) (Tuple.first dim.sideBar)
            }
      , toolsPanelHeight =
          let tph = uiConstants.toolsPanelHeight in
          { tph
            | max = h / 2 - 40
            , initial = clamp tph.min (h / 2 - 40) (Tuple.second dim.bottomPanel)
          }
      }
    
    state : UILayout
    state =
      { uiLayout
        | dimensions =
            { dim
              | viewport = ( w, h )
              , sideBar =
                  ( clamp constants.sideBarWidth.min constants.sideBarWidth.max (Tuple.first dim.sideBar)
                  , Tuple.second dim.sideBar
                  )
              , bottomPanel =
                  ( Tuple.first dim.bottomPanel
                  , clamp constants.toolsPanelHeight.min constants.toolsPanelHeight.max (Tuple.second dim.bottomPanel)
                  )
            }
      }
      |> recalculate_uistate
  in
    { model
      | uiLayout = state
      , uiConstants = constants
    }

{-| Push an interaction onto the an interaction-stack.  If no such interaction-stack
    exists, create one before pushing.
-}
pushInteractionForStack : Maybe Uuid -> InteractionState -> Model -> Model
pushInteractionForStack uuid interaction model =
  let
    new_recent_number =
      AutoDict.values model.interactionsDict
      |> List.maximumBy Tuple.first
      |> Maybe.map (Tuple.first >> (+) 1)
      |> Maybe.withDefault 0
  in
  { model
    | interactionsDict =
        AutoDict.update uuid
          ( Maybe.withDefault (0, [])
          >> (\(_, stack) -> Just (new_recent_number, interaction :: stack))
          )
          model.interactionsDict
  }

{-| Pop an interaction from a particular stack.  If there are no interactions left on the
    stack, then the stack disappears.
-}
popInteraction : Maybe Uuid -> Model -> Maybe (InteractionState, Model)
popInteraction uuid model =
  -- annoyingly, I can't use .update for this because I want to _also_ return the head…
  -- so, in most cases, there are going to be two key lookups.
  -- Oh well!

  -- get the stack
  case AutoDict.get uuid model.interactionsDict of
    Just (_, [h]) ->
      println "One interaction on this stack; removing the stack itself."
      Just <|
        ( h
        , { model | interactionsDict = AutoDict.remove uuid model.interactionsDict }
        )
    Just (r, h :: t) ->
      -- if it has something, pop, and also return an updated model
      println "More than one interaction on this stack; removing the topmost interaction."
      Just <|
        ( h
        , { model | interactionsDict = AutoDict.insert uuid (r - 1, t) model.interactionsDict }
        )
    _ ->
      println "Was called to pop, but there's nothing to pop!"
      -- if there's nothing, or there was no such dict, there's nothing
      -- to do
      Nothing

popMostRecentInteraction : Model -> Maybe (Maybe Uuid, InteractionState, Model)
popMostRecentInteraction model =
  mostRecentInteraction model
  |> Maybe.andThen (\(uuid, _) ->
    popInteraction uuid model
    |> Maybe.map (\(interaction, model_) -> (uuid, interaction, model_))
  )

peekInteraction : Maybe Uuid -> InteractionsDict -> Maybe InteractionState
peekInteraction uuid interactions =
  AutoDict.get uuid interactions
  |> Maybe.map Tuple.second
  |> Maybe.andThen List.head

replaceInteraction : Maybe Uuid -> InteractionState -> Model -> Model
replaceInteraction uuid interaction model =
  popInteraction uuid model
  |> Maybe.map (\(_, model_) -> pushInteractionForStack uuid interaction model_)
  |> Maybe.withDefault model

mostRecentInteraction : Model -> Maybe (Maybe Uuid, InteractionState)
mostRecentInteraction model =
  AutoDict.toList model.interactionsDict
  |> List.maximumBy (Tuple.second >> Tuple.first)
  |> Maybe.andThen
    (\(uuid, (_, interaction)) ->
      case interaction of
        [] -> Nothing
        h::_ -> Just (uuid, h)
    )

type PanMovement
  = Less
  | More
  | Same

panGraphView : Int -> Int -> GraphView -> GraphView
panGraphView xMovement yMovement ({guest_coordinates, guest_dimensions, pan, computation, properties} as graph_view) =
  let
    ( guest_x, guest_y ) = guest_coordinates
    ( guest_w, guest_h ) = guest_dimensions
    ( pan_x, pan_y ) = pan
    -- this is a rectangle that is inset from the maximum bounds.
    movement_amount_x =
      -- if the space to traverse is large, then move by a larger amount.
      max 1 (ceiling (guest_w / 250)) |> toFloat
    movement_amount_y =
      -- if the space to traverse is large, then move by a larger amount.
      max 1 (ceiling (guest_h / 250)) |> toFloat
    ( new_pan_x, new_pan_y ) =
      ( pan_x + toFloat xMovement * movement_amount_x
      , pan_y + toFloat yMovement * movement_amount_y
      )
    ((sx, sy), (ex, ey)) = -- this is after the proposed pan.
      ( ((guest_x + new_pan_x), (guest_y + new_pan_y))
      , ((guest_x + new_pan_x + guest_w), (guest_y + new_pan_y + guest_h))
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
            orig_center = Math.Vector2.vec2 (guest_x + pan_x + guest_w / 2) (guest_y + pan_y + guest_h / 2)
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

packagesToGraphViews : (Float, Float) -> Model -> (List GraphView, Model)
packagesToGraphViews (w, h) model =
  List.foldl
    (\g (acc, model_) ->
      let
        ( v, model__) =
          solvedViewFromComputation GraphEditor.coordinateForces (w, h) Sidebar True g.computation model_
      in
        (v :: acc, model__)
    )
    ( [], model )
    ( AutoDict.values model.packages
      |> List.sortBy (.created >> Time.posixToMillis >> (*) -1)
    )

{-| Expensive. Refresh all computations. -}
refreshComputationsList : Model -> Model
refreshComputationsList model =
  let
    (computation_views, updated_model) =
      packagesToGraphViews (sidebarGraphDimensions model.uiLayout) model
    as_dict =
      List.foldl (\v -> AutoDict.insert v.id v) (AutoDict.empty Uuid.toString) computation_views
  in
    { updated_model
      | graph_views =
          List.foldl AutoDict.remove model.graph_views model.computationsExplorer
          |> AutoDict.union as_dict
      , computationsExplorer =
          List.map .id computation_views
    }

selectNavIcon : NavigatorIcon -> Model -> Model
selectNavIcon icon model =
  { model
    | uiLayout =
        let uiLayout = model.uiLayout in
        { uiLayout
          | selected =
            let selected = uiLayout.selected in
            { selected | sideBar = icon }
        }
  }

removeViews : List Uuid -> Model -> Model
removeViews uuids model =
  { model
    | graph_views =
        List.foldl AutoDict.remove model.graph_views uuids
  }

update_graphview : Uuid -> GraphViewMsg -> Model -> ( Model, Cmd Msg )
update_graphview uuid ui_msg model =
  case ui_msg of
    StartDraggingNode nodeId ->
      ( pushInteractionForStack (Just uuid) (DraggingNode nodeId) model
        |> setProperties
      , Cmd.none
      )

    Pan pan_x pan_y ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (panGraphView pan_x pan_y))
                model.graph_views
        }
      , Cmd.none
      )

    StopPan ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (\graph_view ->
                  { graph_view | activePanDirection = Nothing }
                ))
                model.graph_views
        }
      , Cmd.none
      )

    ResetPan ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (\graph_view ->
                  { graph_view | pan = (0, 0), activePanDirection = Nothing }
                ))
                model.graph_views
        }
      , Cmd.none
      )

    SelectNode node_id ->
      ( case popInteraction (Just uuid) model of
          Just (ChoosingDestinationFor src etc, model_) ->
            selectDestination uuid src etc model_
          _ -> -- includes 'Nothing'
            -- this is the initial selection.
            selectSourceNode model uuid node_id
            |> setProperties
      , Cmd.none
      )

    SelectSpace ->
      ( case popInteraction (Just uuid) model of
          Just (ChoosingDestinationFor src etc, model_) ->
            selectDestination uuid src etc model_
          _ ->
            model
      , Cmd.none
      )
    
    MoveNode (x, y) ->
      ( case peekInteraction (Just uuid) model.interactionsDict of
          Just (DraggingNode node_id) ->
            dragNode uuid (x, y) node_id model
            |> setProperties
          Just (ChoosingDestinationFor _ _) ->
            movePhantomNode uuid (x, y) model
            |> setProperties
          _ ->
            model
      , Cmd.none
      )

    StopDraggingNode ->
      ( popInteraction (Just uuid) model
        |> Maybe.map (Tuple.second >> setProperties)
        |> Maybe.withDefault model
      , Cmd.none
      )

    StartSplit nodeId ->
      ( AutoDict.get uuid model.graph_views
        |> Maybe.andThen
            (\gv ->
                ( gv
                , gv.computation.graph |> Graph.get nodeId
                ) |> Maybe.combineSecond
            )
        |> Maybe.map
          (\(graph_view, nodeContext) ->
            startSplit uuid graph_view nodeContext model
          )
        |> Maybe.withDefault model
      , Cmd.none
      )

splitNode : Graph.NodeContext Entity Connection -> Connection -> GraphView -> AutomatonGraph
splitNode node left graph_view = -- turns out that "right" isn't needed. Hmmm!!!?
  let
    (recursive, nonRecursive) =
      node.incoming
      |> IntDict.partition (\k _ -> k == node.node.id)
    recursive_connection =
      recursive
      |> IntDict.values
      |> List.foldl AutoSet.union (AutoSet.empty transitionToString)
      |> debugLog_ "recursive_connection" AutoSet.toList
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
                    |> debugLog_ "Added to Left" IntDict.toList
                  , r_
                  )
                else
                  ( l_
                  , IntDict.update k
                    ( Maybe.map (AutoSet.insert transition)
                      >> Maybe.orElseLazy (\() -> Just <| AutoSet.singleton transitionToString transition)
                    )
                    r_
                    |> debugLog_ "Added to Right" IntDict.toList
                  )
              )
              (l, r)
              conn
          )
          (IntDict.empty, IntDict.empty)
    ag =
      graph_view.computation
      |> debugAutomatonGraph "Before node-split"
    id = maxId ag + 1
    newUserGraph =
      { ag
        | graph =
            ag.graph
            |> Graph.insert
              { node =
                  { label = entity id NoEffect
                  , id = id
                  }
              , incoming =
                  leftConnections
                  |>if AutoSet.isEmpty recursive_connection then identity
                    else IntDict.insert id recursive_connection
              , outgoing =
                  node.outgoing
                  |>if AutoSet.isEmpty recursive_connection then identity
                    else IntDict.insert id recursive_connection
              }
              |> debugGraph "After inserting left connections"
            |> Graph.update node.node.id
              (\_ ->
                Just <|
                  { node
                    | incoming =
                        rightConnections
                        |> IntDict.insert node.node.id recursive_connection
                  }
              )
              |> debugGraph "After inserting right connections"
      }
  in
    newUserGraph
    |> debugAutomatonGraphXY "After node-split"


nodeSplitSwitch : SplitNodeInterfaceProperties -> Maybe Uuid -> AcceptVia -> NodeSplitData -> Model -> Model
nodeSplitSwitch props key_uuid via ({left, right} as data) model =
  let
    tr finality =
      Transition finality via
    onLeft_0 = AutoSet.member (tr False) left
    onLeft_1 = AutoSet.member (tr True) left
    onRight_0 = AutoSet.member (tr False) right
    onRight_1 = AutoSet.member (tr True) right
    pushToRight t =
      ( AutoSet.remove t left, AutoSet.insert t right )
    pushToLeft t =
      ( AutoSet.insert t left, AutoSet.remove t right )
    ( newLeft, newRight ) =
      if onLeft_0 && onLeft_1 then
        -- push the non-terminal to the right.
        pushToRight (tr False)
      else if onLeft_1 then
        -- push the terminal to the right
        pushToRight (tr True)
      else if onRight_0 && onRight_1 then
        -- push the non-terminal to the left
        pushToLeft (tr False)
      else if onRight_1 then
        pushToLeft (tr True)
      else if onLeft_0 then
        pushToRight (tr False)
      else if onRight_0 then
        pushToLeft (tr False)
      else
        ( left, right )
  in
    replaceInteraction key_uuid
      ( SplittingNode
          { data | left = newLeft, right = newRight } 
          props
      )
      model
    |> setProperties

handleConnectionEditorInput : Maybe Uuid -> String -> ConnectionAlteration -> ConnectionEditorProperties -> Model -> Model
handleConnectionEditorInput key_uuid input alteration props model =
  case props.editingMode of
    CharacterInput ->
      String.toList input
      |> List.head
      |> Maybe.map
        (\ch -> handleConnectionCharacterInput props key_uuid ch alteration model)
      |> Maybe.withDefault model
    GraphReferenceSearch _ ->
      if String.contains "`" input then
        replaceInteraction key_uuid
          ( EditingConnection alteration { props | editingMode = CharacterInput } )
          model
      else
        replaceInteraction key_uuid
          ( EditingConnection alteration
              { props
                | editingMode = GraphReferenceSearch input
                , shownList = filterConnectionEditorGraphs input props.referenceList model
              }
          )
          model

handleConnectionCharacterInput : ConnectionEditorProperties -> Maybe Uuid -> Char -> ConnectionAlteration -> Model -> Model
handleConnectionCharacterInput props key_uuid ch alteration model =
  case ch of
    '`' ->
      replaceInteraction key_uuid
        ( EditingConnection alteration { props | editingMode = GraphReferenceSearch "" } )
        model
    _ ->
      replaceInteraction key_uuid
        ( EditingConnection
            { alteration | connection = toggleConnectionTransition (ViaCharacter ch) alteration.connection }
            props
        )
        model

toggleConnectionTransition : AcceptVia -> Connection -> Connection
toggleConnectionTransition acceptCondition conn =
  AutoSet.foldl
    (\t (seen, state) ->
      if t.via == acceptCondition then
        -- I've found the right transition.  Now, update or remove or…?
        if t.isFinal then
          ( True, state ) -- skip it; i.e., remove it.
        else
          ( True
          , AutoSet.insert { t | isFinal = True } state -- make it final
          )
      else
        (seen, AutoSet.insert t state)
    )
    (False, AutoSet.empty transitionToString)
    conn
  |>  (\(seen, resultSet) ->
        if seen then
          resultSet -- I've handled this above.
        else
          -- need to insert it.
          AutoSet.insert
            (Transition
              False
              acceptCondition
            )
            resultSet
      )

filterConnectionEditorGraphs : String -> List Uuid -> Model -> List Uuid
filterConnectionEditorGraphs s referenceList model =
  let
    v = String.toLower s
  in
  List.filterMap (\uuid -> AutoDict.get uuid model.graph_views) referenceList
  |> List.filter
    (\gv ->
      case gv.computation.description of
        Nothing ->
          True -- include; there's nothing to filter on.
        Just desc ->
          String.contains v (String.toLower desc)
    )
  |> List.map .id

editConnection : Maybe Uuid -> (Float, Float) -> ConnectionAlteration -> Model -> Model
editConnection view_uuid (x, y) ({source, dest, connection} as alteration) model =
  packagesToGraphViews (430, 3/6 * 430) model
  |>  (\(graph_views, model_) ->
        AutoDict.get model.mainGraphView model.graph_views
        |> Maybe.map (\gv ->
          let
            ag =
              case Graph.get dest gv.computation.graph of
                Nothing ->
                  GraphEditor.newnode_graphchange source x y connection gv.computation
                Just _ ->
                  GraphEditor.updateLink_graphchange source dest connection gv.computation
            ( main_view, updated_model ) =
              naiveViewFromComputation
                ( 250 , 250 ) Independent True
                ag model_
            solidified_model =
              solidifyPhantoms main_view.id source dest updated_model
          in
            (List.map .id graph_views, main_view.id, solidified_model)
          )
        |> Maybe.withDefault ( List.map .id graph_views, model.mainGraphView, model_ )
      )
  |>  (\(uuids, main_uuid, model_) ->
        let
          stackModify =
            case peekInteraction view_uuid model_.interactionsDict of
              Just (EditingConnection _ _) -> replaceInteraction
              _ -> pushInteractionForStack
        in
          stackModify view_uuid
            ( EditingConnection alteration
                { referenceList = uuids
                , shownList = uuids
                , mainGraph = main_uuid
                , editingMode = CharacterInput
                }
            )
            { model_
              | graph_views =
                  updateGraphView main_uuid
                    (centerAndHighlight [ (source, dest) ] >> Just)
                    model_.graph_views
            }
      )
  |> setProperties

unusedNodeId : AutomatonGraph -> NodeId
unusedNodeId {graph} =
  Graph.nodeIdRange graph
  |> Maybe.map (Tuple.second >> (+) 1)
  |> Maybe.withDefault 0

selectSourceNode : Model -> Uuid -> NodeId -> Model
selectSourceNode model view_uuid node_id =
  -- initially, you must click on a node to select it.
  -- therefore, we are initially always looking at a recursive
  -- connection to the same node!
  AutoDict.get view_uuid model.graph_views
  |> Maybe.andThen (.computation >> .graph >> Graph.get node_id)
  |> Maybe.map
    (\nodeContext ->
      let
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
          , highlighting = Just Phantom
          }
        interaction =
          ChoosingDestinationFor node_id
            ( ExistingNode node_id
                ( IntDict.get node_id nodeContext.incoming
                  |> Maybe.withDefault (AutoSet.empty transitionToString)
                )
            )
      in
        pushInteractionForStack (Just view_uuid) interaction model
          -- and now modify the drawing-data for that view
        |> updateDrawingData view_uuid
            (\drawingData ->
              { drawingData
                | node_drawing =
                    Dict.update node_id
                      (Maybe.map (\source_node ->
                        { source_node | isSelected = True }
                      ))
                      drawingData.node_drawing
                , link_drawing =
                    Dict.insert (node_id, node_id) linkDrawingData drawingData.link_drawing
              }
            )
    )
  |> Maybe.withDefault model


{- moving phantom node.  Wow, this is a ridiculous amount of code, separated into
   an equaly ridiculous number of functions!!
-}

{-| Called to turn phantoms, in the node-drawing data only, into solid things
-}
solidifyPhantoms : Uuid -> NodeId -> NodeId -> Model -> Model
solidifyPhantoms view_uuid src phantom_id model =
  updateDrawingData view_uuid
    (\drawingData ->
      { drawingData
        | node_drawing =
            Dict.update phantom_id
              (Maybe.map (\phantom ->
                { phantom | exclusiveAttributes = Nothing }
              ))
              drawingData.node_drawing
            |> Dict.update src
              (Maybe.map (\srcNode ->
                { srcNode | isSelected = False }
              ))
        , link_drawing =
            ( Dict.update (src, phantom_id)
                (Maybe.map (\existing_link ->
                  { existing_link | highlighting = Nothing }
                ))
                drawingData.link_drawing
            )
      }
    )
    model

switchFromExistingToPhantom : Uuid -> Bool -> NodeId -> GraphView -> (Float, Float) -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromExistingToPhantom view_uuid old_conn_is_empty existing_id graph_view (x_, y_) sourceNodeContext model =
  let
    phantom_nodeid =
      unusedNodeId graph_view.computation
    nodeData =
      { exclusiveAttributes = Just DrawPhantom
      , isTerminal = True
      , isDisconnected = False
      , coordinates = (x_, y_)
      , isRoot = False
      , canSplit = False
      , view_uuid = view_uuid
      , isSelected = False
      }
    linkData = -- this is the new calculated link-path
      { cardinality = Unidirectional
      , graphReferenceDescriptions = AutoDict.empty Uuid.toString
      , pathBetween =
          path_between -- calculate the link path
            sourceNodeContext.node.label
            { x = x_, y = y_ }
            Unidirectional
      , executionData = Nothing
      , connection = AutoSet.empty transitionToString
      , highlighting = Just Phantom
      }
  in
    updateDrawingData view_uuid
      (\drawingData ->
        { drawingData
          | node_drawing = Dict.insert phantom_nodeid nodeData drawingData.node_drawing
          , link_drawing =
              ( if old_conn_is_empty then
                  Dict.remove (sourceNodeContext.node.id, existing_id) drawingData.link_drawing
                else
                  Dict.update (sourceNodeContext.node.id, existing_id)
                    (Maybe.map (\existing_link ->
                      { existing_link | highlighting = Nothing }
                    ))
                    drawingData.link_drawing
              )
              -- and add the link path to `some_node`
              |> Dict.insert (sourceNodeContext.node.id, phantom_nodeid) linkData
        }
      )
      model
    -- we've made the changes, so set the interaction
    |> replaceInteraction (Just view_uuid)
        ( ChoosingDestinationFor sourceNodeContext.node.id
            ( NewNode phantom_nodeid (x_, y_) )
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
        descriptionsForConnection connection model.packages
    , pathBetween =
        path_between -- calculate the link path
          sourceNodeContext.node.label
          existingNodeContext.node.label
          cardinality
    , executionData = Nothing
    , connection = connection
    , highlighting = Just Phantom
    }

switchFromPhantomToExisting : Uuid -> NodeId -> Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromPhantomToExisting view_uuid phantom_id sourceNodeContext existingNodeContext model =
  let
    linkData = -- this is the new calculated link-path
      phantomLinkDrawingForExisting sourceNodeContext existingNodeContext model
  in
    updateDrawingData view_uuid
      (\drawingData ->
        { drawingData
          | node_drawing =
              Dict.remove phantom_id drawingData.node_drawing
          , link_drawing =
              -- get rid of the old phantom link
              Dict.remove (sourceNodeContext.node.id, phantom_id) drawingData.link_drawing
              -- and add the link path to `some_node`
              |> Dict.insert (sourceNodeContext.node.id, existingNodeContext.node.id) linkData
        }
      )
      model
    -- we've made the changes, so set the interaction
    |> replaceInteraction (Just view_uuid)
        ( ChoosingDestinationFor sourceNodeContext.node.id
            ( ExistingNode existingNodeContext.node.id linkData.connection )
        )

switchFromExistingToExisting : Uuid -> Bool -> NodeId -> Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromExistingToExisting view_uuid old_conn_is_empty old_existing sourceNodeContext nearbyNodeContext model =
  let
    linkData = -- this is the new calculated link-path
      phantomLinkDrawingForExisting sourceNodeContext nearbyNodeContext model
  in
    updateDrawingData view_uuid
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
        }
      )
      model
    -- we've made the changes, so set the interaction
    |> replaceInteraction (Just view_uuid)
        ( ChoosingDestinationFor sourceNodeContext.node.id
            ( ExistingNode nearbyNodeContext.node.id linkData.connection )
        )

updatePhantomMovement : Uuid -> NodeId -> ( Float, Float ) -> Graph.NodeContext Entity Connection -> Model -> Model
updatePhantomMovement view_uuid phantom_id (x_, y_) sourceNodeContext model =
  updateDrawingData view_uuid
    (\drawingData ->
        { drawingData
          | node_drawing =
              Dict.update phantom_id
                (Maybe.map (\nodeData ->
                  { nodeData | coordinates = (x_, y_) }
                ))
                drawingData.node_drawing
          , link_drawing =
              Dict.update (sourceNodeContext.node.id, phantom_id)
                (Maybe.map (\linkData ->
                  { linkData
                    | pathBetween =
                        path_between
                          sourceNodeContext.node.label
                          { x = x_, y = y_ }
                          Unidirectional
                  }
                ))
                drawingData.link_drawing
        }
    )
    model
  |> replaceInteraction (Just view_uuid)
    ( ChoosingDestinationFor sourceNodeContext.node.id (NewNode phantom_id (x_, y_)) )

movePhantomNodeInView : Uuid -> GraphView -> (Float, Float) -> Model -> Model
movePhantomNodeInView view_uuid graph_view (x_, y_) model =
  -- in the event, the `x_` and `y_` have already been translated
  -- into guest-viewport coordinates, accounting for pan information.
  let
    -- what is the destination of this link?
    -- Sounds like a simple question: it's the phantom node, of course.  But when the
    -- phantom node gets too close to a REAL node, then it should switch to that node;
    -- and when it is far enough away, then it should switch back.
    nearby_node_lockOnDistance : Float
    nearby_node_lockOnDistance = 36 -- min distance before arrow inverts…

    nearby_node_func : ((Graph.Node Entity -> Bool) -> List (Graph.Node Entity) -> b) -> Float -> (Float, Float) -> GraphView -> b
    nearby_node_func f distance mouseCoords { computation } =
      -- a good distance value is nodeRadius + 9 = 7 + 9 = 16, for "locking on".
      let
        ( mouse_x, mouse_y ) =
          mouseCoords -- |> Debug.log "Mousecoords"
        square_dist = distance * distance
      in
        f
          (\node ->
            let
              dx = node.label.x - mouse_x
              dy = node.label.y - mouse_y
            in
              -- Debug.log ("Checking (" ++ String.fromFloat node.label.x ++ ", " ++ String.fromFloat node.label.y ++ ") against (" ++ String.fromFloat mouse_x ++ ", " ++ String.fromFloat mouse_y ++ ")") () |> \_ ->
              dx * dx + dy * dy <= square_dist -- 7 + 9 = 16
          )
          (Graph.nodes computation.graph)

    nearby_node : Float -> (Float, Float) -> GraphView -> Maybe (Graph.Node Entity)
    nearby_node =
      nearby_node_func List.find

    -- nearby_nodes : Float -> (Float, Float) -> GraphView -> List (Graph.Node Entity)
    -- nearby_nodes =
    --   nearby_node_func List.filter

  in
    -- first, let's find this thing.
    case peekInteraction (Just view_uuid) model.interactionsDict of
      Just (ChoosingDestinationFor source (NewNode phantom_id _)) ->
        -- okay, so there is a phantom node already there and active.
        -- in the easiest case, we just need to move its (x,y) coordinates, and be done.
        -- But if we are close enough to "lock on" to a nearby node, then we need to
        -- change this interaction to reflect that instead.  So, which case do we
        -- have?
        case nearby_node nearby_node_lockOnDistance (x_, y_) graph_view of
          Just nearbyNode ->
            -- ooh, we're close to a lock-on node. Okay. Let's get rid of the phantom
            -- node; then calculate the link path (might be straight or curved or recursive)
            -- based on the node; and then get rid of the old link path, and put in the
            -- new one.
            -- Lastly, we set the interaction to the correct value.            
            Maybe.map2
              (\sourceNodeContext nearbyNodeContext ->
                  switchFromPhantomToExisting view_uuid phantom_id sourceNodeContext nearbyNodeContext model
              )
              (Graph.get source graph_view.computation.graph)
              (Graph.get nearbyNode.id graph_view.computation.graph)
            |> Maybe.withDefault model
          Nothing ->
            -- great, there is no nearby node; just update the (x, y).
            Maybe.map
              (\sourceNodeContext ->
                updatePhantomMovement view_uuid phantom_id (x_, y_) sourceNodeContext model
              )
              (Graph.get source graph_view.computation.graph)
            |> Maybe.withDefault model
      Just (ChoosingDestinationFor source (ExistingNode existing_node conn)) ->
        -- here the situation is "reversed":
        -- If I find a neighbour, and it is the existing neighbour, then I need do
        -- nothing.
        -- If I find a neighbour, and it is NOT the existing neighbour, then I need to
        -- switch to it.
        -- If I don't find a neighbour, then I must switch to a phantom node.
        case nearby_node nearby_node_lockOnDistance (x_, y_) graph_view of
          Just nearbyNode ->
            if existing_node == nearbyNode.id then
              -- no change needed.
              model
            else
              -- switch to the new node.
              Maybe.map2
                (\sourceNodeContext nearbyNodeContext ->
                    switchFromExistingToExisting view_uuid (AutoSet.isEmpty conn) existing_node sourceNodeContext nearbyNodeContext model
                )
                (Graph.get source graph_view.computation.graph)
                (Graph.get nearbyNode.id graph_view.computation.graph)
              |> Maybe.withDefault model -- no changes made.
          Nothing ->
            -- there is no nearby node; move from the existing node to a phantom node. 
            Maybe.map
              (\sourceNodeContext -> switchFromExistingToPhantom view_uuid (AutoSet.isEmpty conn) existing_node graph_view (x_, y_) sourceNodeContext model)
              (Graph.get source graph_view.computation.graph)
            |> Maybe.withDefault model
      _ ->
        model

movePhantomNode : Uuid -> (Float, Float) -> Model -> Model
movePhantomNode view_uuid (x_, y_) model =
  AutoDict.get view_uuid model.graph_views
  |> Maybe.map (\gv -> movePhantomNodeInView view_uuid gv (x_, y_) model)
  |> Maybe.withDefault model

dragNodeInView : Uuid -> GraphView -> (Float, Float) -> Graph.NodeContext Entity Connection -> Model -> Model
dragNodeInView view_uuid graph_view (x_, y_) nodeContext model =
  let
    vertices_in =
      IntDict.toList nodeContext.incoming
      |> List.filterMap
        (\(k, conn) ->
          Graph.get k graph_view.computation.graph
          |> Maybe.map
            (\sourceCtx ->
              linkDrawingForEdge sourceCtx nodeContext conn model.packages
            )
        )
    vertices_out =
      IntDict.toList nodeContext.outgoing
      |> List.filterMap
        (\(k, conn) ->
          Graph.get k graph_view.computation.graph
          |> Maybe.map
            (\targetCtx ->
              linkDrawingForEdge nodeContext targetCtx conn model.packages
            )
        )
  in
    updateDrawingData view_uuid
      (\drawingData ->
        { node_drawing =
            Dict.update nodeContext.node.id
              (Maybe.map (\node ->
                { node | coordinates = (x_, y_) }
              ))
              drawingData.node_drawing
        , link_drawing =
            List.foldl
              (\( (src, dest), data ) ->
                Dict.insert (src, dest) data
              )
              drawingData.link_drawing
              (vertices_in ++ vertices_out)
        }
      )
      model

dragNode : Uuid -> (Float, Float) -> NodeId -> Model -> Model
dragNode view_uuid (x_, y_) nodeId model =
  AutoDict.get view_uuid model.graph_views
  |> Maybe.andThen
    (\gv ->
      Maybe.combineSecond
        ( gv
        , Graph.get nodeId gv.computation.graph
        )
    )
  |> Maybe.map
    (\(gv, nodeContext) ->
      let
        updatedCtx : Graph.NodeContext Entity Connection
        updatedCtx =
          { nodeContext
            | node =
                { id = nodeId
                , label =
                  let e = nodeContext.node.label in
                  { e | x = x_, y = y_ }
                }
          }
        gv_ : GraphView
        gv_ = -- 🤮🤮🤮🤮🤮🤮🤮🤮🤮 seriously, Elm, this syntax is disgusting
          -- yes, reader, I know about lenses.
          -- no, reader, I don't spend my days crafting dozens of lines of
          -- boilerplate because somebody can't figure out how to get the
          -- language syntax right.
          -- *sigh*…
          -- { gv | package.computation.graph = … } is how this SHOULD go.
          -- but I guess this is just where we are today!
          { gv
            | computation =
              let ag = gv.computation in
              { ag | graph = Graph.insert updatedCtx ag.graph }
          }
        model_ =
          { model
            | graph_views =
                updateGraphView view_uuid (\_ -> Just gv_) model.graph_views
          }
      in
        dragNodeInView view_uuid gv_ (x_, y_) updatedCtx model_
    )
  |> Maybe.withDefault model

updateDrawingData : Uuid -> (DrawingData -> DrawingData) -> Model -> Model
updateDrawingData view_uuid f model =
  { model
    | graph_views =
        AutoDict.update view_uuid
          (Maybe.map (\graph_view ->
            { graph_view
              | drawingData = f graph_view.drawingData
            }
          ))
          model.graph_views
  }

removePhantomLink : Uuid -> NodeId -> NodeId -> Model -> Model
removePhantomLink view_uuid source dest =
  updateDrawingData view_uuid
    (\drawingData ->
      { drawingData
        | link_drawing =
            Dict.remove (source, dest) drawingData.link_drawing
      }
    )

cancelNewNodeCreation : Uuid -> Model -> Model
cancelNewNodeCreation view_uuid model =
  let
    kill : NodeId -> NodeId -> Model -> Model
    kill source dest model_ =
      updateDrawingData view_uuid
        (\drawingData ->
          { drawingData
            | node_drawing =
                Dict.remove dest drawingData.node_drawing
            , link_drawing =
                Dict.remove (source, dest) drawingData.link_drawing
          }
        )
        model_
  in
    case popInteraction (Just view_uuid) model of
      Just (ChoosingDestinationFor source (NewNode dest _), model_) ->
        kill source dest model_
      Just (EditingConnection {source, dest, deleteTargetIfCancelled} _, model_) ->
        if deleteTargetIfCancelled then
          kill source dest model_
        else
          println "🚨 ERROR WHMD(MWOEI" -- how am I in this function, if there's no new node??
          model
      _ ->
        println "🚨 ERROR $DBMWMGYERCC" -- how am I in this function, if neither of these is cancelled??
        model

createNewGraphNode : Uuid -> NodeId -> (Float, Float) -> Model -> Model
createNewGraphNode view_uuid node_id (x, y) model =
  let
    viewWithNode : GraphView -> GraphView
    viewWithNode graph_view = -- ensure that the destination node exists in the graph.
      { graph_view
        | computation =
          let computation = graph_view.computation in
          { computation
            | graph =
                Graph.insert
                  { node =
                      { id = node_id
                      , label =
                          entity node_id NoEffect
                          |> (\e -> { e | x = x, y = y })
                      }
                  , incoming = IntDict.empty
                  , outgoing = IntDict.empty
                  }
                  computation.graph
          }
      }
  in
    { model
      | graph_views =
          AutoDict.update view_uuid
            (Maybe.map (viewWithNode))
            model.graph_views
    }

nilMainProperties : MainUIProperties
nilMainProperties =
  { canEscape = False
  , canDragSplitter = False
  , canAcceptCharacters = False
  , canSelectNewPackage = False
  , canCreateNewPackage = False
  , canLoadTestInput = False
  , canDeleteTestInput = False
  , canCreateTestInput = False
  }

nilViewProperties : GraphViewProperties
nilViewProperties =
  { canSelectConnections = False
  , canSelectEmptySpace = False
  , canSelectNodes = False
  , canSplitNodes = False
  , canDragNodes = False
  , canInspectRefs = False
  , canPan = False
  , canDeletePackage = False
  , canSelectPackage = False
  }

type alias GraphViewPropertySetter = GraphViewProperties
type alias MainPropertySetter = MainUIProperties

setProperties : Model -> Model
setProperties model =
  let
    setLocalProperties : GraphViewDict -> GraphViewDict
    setLocalProperties =
      let
        whenSplittingNode : GraphViewPropertySetter
        whenSplittingNode =
          { nilViewProperties
            | canDragNodes = True
            , canInspectRefs = True
            , canPan = True
          } -- this is a message. the quick brown fox jumps over the lazy dog.
        whenDraggingNode : GraphViewPropertySetter
        whenDraggingNode =
          { nilViewProperties | canPan = True }
        whenDraggingSplitter : GraphViewPropertySetter
        whenDraggingSplitter =
          nilViewProperties
        whenSourceNodeSelected : Uuid -> GraphViewPropertySetter
        whenSourceNodeSelected id =
          { nilViewProperties
            | canSelectEmptySpace = model.mainGraphView == id
            , canSelectNodes = model.mainGraphView == id
            , canSplitNodes = model.mainGraphView == id
            , canDragNodes = model.mainGraphView == id
            , canPan = True
          }
        whenEditingConnection : GraphViewPropertySetter
        whenEditingConnection =
          { nilViewProperties
            | canInspectRefs = True
            , canPan = True
          }
        whenExecuting : GraphViewPropertySetter
        whenExecuting =
          { nilViewProperties
            | canPan = True
            , canDragNodes = True
          }
        whenSimulatingForces : GraphViewPropertySetter
        whenSimulatingForces =
          { nilViewProperties
            | canSelectConnections = True
            , canSelectNodes = True
            , canSplitNodes = True
            , canPan = True
          }
        otherwise : Uuid -> GraphViewPropertySetter
        otherwise id =
          { nilViewProperties
            | canSelectNodes = model.mainGraphView == id
            , canSplitNodes = model.mainGraphView == id
            , canDragNodes = model.mainGraphView == id
            , canSelectConnections = model.mainGraphView == id
            , canInspectRefs = True
            , canPan = True
            , canSelectPackage = List.member id model.computationsExplorer
            , canDeletePackage = List.member id model.computationsExplorer
          }
        whenDeletingPackage : GraphViewPropertySetter
        whenDeletingPackage =
          { nilViewProperties | canPan = True }
      in
        AutoDict.map
          (\k v ->
            { v
              | properties =
                  case peekInteraction (Just k) model.interactionsDict of
                    Just (SplittingNode _ _) ->
                      whenSplittingNode
                    Just (DraggingNode _) ->
                      whenDraggingNode
                    Just (ChoosingDestinationFor _ _) ->
                      whenSourceNodeSelected v.id
                    Just (EditingConnection _ _) ->
                      whenEditingConnection
                    Just (Executing _ _) ->
                      whenExecuting
                    Just (SimulatingForces _ _ _) ->
                      whenSimulatingForces
                    Just (DraggingSplitter _) ->
                      whenDraggingSplitter
                    Just (DeletingPackage _ _) ->
                      whenDeletingPackage
                    Nothing ->
                     otherwise v.id
            }
          )
    setMainProperties : MainUIProperties
    setMainProperties =
      let
        whenSplittingNode : MainPropertySetter
        whenSplittingNode =
          { nilMainProperties
            | canEscape = True
            , canDragSplitter = True
            , canAcceptCharacters = True
          }
        whenDraggingNode : MainPropertySetter
        whenDraggingNode =
          { nilMainProperties | canEscape = True }
        whenDraggingSplitter : MainPropertySetter
        whenDraggingSplitter =
          nilMainProperties
        whenSourceNodeSelected : MainPropertySetter
        whenSourceNodeSelected =
          { nilMainProperties
            | canEscape = True
            , canDragSplitter = True
            , canSelectNewPackage = True
            , canCreateNewPackage = True
          }
        whenEditingConnection : MainPropertySetter
        whenEditingConnection =
          { nilMainProperties
          | canEscape = True
          , canAcceptCharacters = True
          }
        whenExecuting : MainPropertySetter
        whenExecuting =
          { nilMainProperties
          | canEscape = True
          , canDragSplitter = True
          , canLoadTestInput = True
          , canDeleteTestInput = True
          , canCreateTestInput = True
          }
        whenSimulatingForces : MainPropertySetter
        whenSimulatingForces =
          { nilMainProperties
            | canDragSplitter = True
            , canSelectNewPackage = True
            , canCreateNewPackage = True
            , canLoadTestInput = True
            , canDeleteTestInput = True
            , canCreateTestInput = True
          }
        otherwise : MainPropertySetter
        otherwise =
          { nilMainProperties
            | canDragSplitter = True
            , canSelectNewPackage = True
            , canCreateNewPackage = True
            , canLoadTestInput = True
            , canDeleteTestInput = True
            , canCreateTestInput = True
          }
        whenDeletingPackage : MainPropertySetter
        whenDeletingPackage =
          { nilMainProperties | canEscape = True }
      in
        case mostRecentInteraction model of
          Just (_, SplittingNode _ _) ->
            whenSplittingNode
          Just (_, DraggingNode _) ->
            whenDraggingNode
          Just (_, ChoosingDestinationFor _ _) ->
            whenSourceNodeSelected
          Just (_, EditingConnection _ _) ->
            whenEditingConnection
          Just (_, Executing _ _) ->
            whenExecuting
          Just (_, SimulatingForces _ _ _) ->
            whenSimulatingForces
          Just (_, DraggingSplitter _) ->
            whenDraggingSplitter
          Just (_, DeletingPackage _ _) ->
            whenDeletingPackage
          Nothing ->
            otherwise
  in
    { model
      | graph_views = setLocalProperties model.graph_views
      , properties = setMainProperties
    }
    
selectDestination : Uuid -> NodeId -> PossibleDestination -> Model -> Model
selectDestination view_uuid src possible_dest model =
  case possible_dest of
    NewNode phantom_id (x, y) ->
      -- I've clicked on some kind of an empty space.  I'll want to create this node,
      -- and then proceed to edit the connection.
      createNewGraphNode view_uuid phantom_id (x, y) model
      -- editConnection will call setProperties
      |> editConnection (Just view_uuid) (x, y)
          { source = src
          , dest = phantom_id
          , connection = AutoSet.empty transitionToString
          , deleteTargetIfCancelled = True
          }
    ExistingNode dest_id conn ->
      let
        (x, y) =
          AutoDict.get view_uuid model.graph_views
          |> Maybe.andThen (\gv -> Graph.get dest_id gv.computation.graph)
          |> Maybe.map (\ctx -> (ctx.node.label.x, ctx.node.label.y))
          |> Maybe.withDefault (0, 0)
      in
        -- we already have a node selected, and now, an existing
        -- node is being selected as the destination.
        -- editConnection will call setProperties
        editConnection (Just view_uuid) (x, y)
          { source = src
          , dest = dest_id
          , connection = conn
          , deleteTargetIfCancelled = False
          }
          model

deleteLinkFromView : Uuid -> NodeId -> NodeId -> Connection -> Model -> Model
deleteLinkFromView view_uuid source dest conn model =
  let
    emptyConnection : GraphView -> Model
    emptyConnection graph_view =
      let
        alterPackage ag =
          { ag
            | graph =
                Graph.update dest
                  (Maybe.map (\destContext ->
                    { destContext
                      | incoming =
                          IntDict.remove source destContext.incoming
                    }
                  ))
                  ag.graph
          }
      in
        updateGraphInView alterPackage graph_view model
  in
  if AutoSet.isEmpty conn then
    AutoDict.get view_uuid model.graph_views
    |> Maybe.map emptyConnection
    |> Maybe.withDefault model
  else
    { model
      | graph_views =
          AutoDict.update view_uuid
            (Maybe.map (\graph_view ->
              { graph_view
                | drawingData =
                    let drawingData = graph_view.drawingData in
                    { drawingData
                      | link_drawing =
                          Dict.update (source, dest)
                            (Maybe.map (\link -> { link | highlighting = Nothing }))
                            drawingData.link_drawing
                    }
              }
            ))
            model.graph_views
    }

deleteNodeFromView : Uuid -> NodeId -> NodeId -> Model -> Model
deleteNodeFromView view_uuid source dest model =
  let
    model_ =
      updateDrawingData view_uuid
        (\drawingData ->
            { drawingData
              | link_drawing =
                  Dict.remove (source, dest) drawingData.link_drawing
              , node_drawing =
                  Dict.remove dest drawingData.node_drawing
            }
        )
        model
  in
      { model_
        | graph_views =
            AutoDict.update view_uuid
              (Maybe.map (\graph_view ->
                { graph_view
                  | computation =
                    let ag = graph_view.computation in
                    { ag | graph = Graph.remove dest ag.graph }
                }
              ))
              model_.graph_views
      }

startSplit : Uuid -> GraphView -> Graph.NodeContext Entity Connection -> Model -> Model
startSplit uuid graph_view nodeContext model =
  let
    ( main_view, updated_model ) =
      naiveViewFromComputation
        ( 250 , 250 ) Independent True
        graph_view.computation model
    edges_in =
      IntDict.keys nodeContext.incoming
      |> List.map (\to -> (to, nodeContext.node.id))
  in
    pushInteractionForStack (Just uuid)
      ( SplittingNode
          { to_split = nodeContext.node.id
          , left = AutoSet.empty transitionToString
          , right =
              IntDict.foldl
                (\k v acc ->
                    if k /= nodeContext.node.id then
                      AutoSet.union v acc
                    else
                      acc
                )
                (AutoSet.empty transitionToString)
                nodeContext.incoming
          }
          { mainGraph = main_view.id
          }
      )
      updated_model
    |> (\model_ ->
          { model_
            | graph_views =
                updateGraphView main_view.id
                  ( centerAndHighlight ( edges_in {- ++ edges_out -} )
                  >> Just
                  )
                  model_.graph_views
          }
      )
    |> setProperties

toResolutionDict : PackageDict -> ResolutionDict
toResolutionDict packages =
  AutoDict.map (\_ -> .computation) packages

deletePackageFromModel : Uuid -> Model -> Model
deletePackageFromModel uuid model =
  let
    filterTransition : Transition -> Bool
    filterTransition {via} =
      case via of
        ViaGraphReference ref ->
          ref /= uuid
        _ ->
          True
    filterConnection : Connection -> Connection
    filterConnection conn =
      AutoSet.filter filterTransition conn
  in
  { model
    | packages =
        AutoDict.remove uuid model.packages
        -- and now get rid of this package from the connections.
        -- I'm NOT putting this in.
        -- I may change my mind later.
        -- But it causes some pretty widespread disruption, AND
        -- a bunch of invalid AutomatonGraphs…
        -- So for now: just no.
        -- And I will have some invalid Graph references instead.
{-
        |> AutoDict.map
          (\_ pkg ->
            { pkg
              | computation =
                  let ag = pkg.computation in
                  { ag
                    | graph =
                        Graph.mapEdges filterConnection  ag.graph
                  }
            }
          )
-}
  }

updatePackageFromView : Uuid -> Model -> Model
updatePackageFromView gv_uuid model =
  let
    resolutionDict =
      toResolutionDict model.packages
  in
  AutoDict.get gv_uuid model.graph_views
  |> Maybe.andThen (\graph_view ->
    graph_view.graphPackage
    |> Maybe.map (\pkg_uuid ->
      { model
        | packages =
            AutoDict.update pkg_uuid
              (Maybe.map (\pkg ->
                { pkg
                  | computation = graph_view.computation
                  -- run the tests
                  , tests =
                      AutoDict.map
                        (\_ entry ->
                          { entry
                            | result =
                                DFA.load entry.input resolutionDict graph_view.computation
                                |> DFA.run
                                |> List.head
                                |> Maybe.andThen .finalResult
                                |> Maybe.withDefault (InternalError "no result received from test execution")
                          }
                        )
                        pkg.tests
                }
              ))
              model.packages
      }
    )
  )
  |> Maybe.withDefault model -- do nothing; there is no GV, or no link, or no package.

commitOrConfirm : Model -> (Model, Cmd Msg)
commitOrConfirm model =
  -- What am I confirming?
  let
    {- Take an AutomatonGraph and push it into tho Model. -}
    commit_change : GraphView -> AutomatonGraph -> Model -> Model
    commit_change graph_view ag model_ =
      let
        updated_view =
          { graph_view
            | computation = ag
            , undoBuffer = graph_view.computation :: graph_view.undoBuffer
            , redoBuffer = [] -- when we make a new change, the redo-buffer disappears; we're not storing a tree!
          }
        updated_model =
          upsertGraphView updated_view.id (fitGraphViewToGraph updated_view) model_
      in
        refreshComputationsList updated_model
    
    updateExistingNode : NodeId -> NodeId -> Connection -> GraphView -> Model -> Model
    updateExistingNode src dest conn gv model_ =
      GraphEditor.updateLink_graphchange src dest conn gv.computation
      |> \newGraph -> commit_change gv newGraph model_

    confirmPhantomNode : NodeId -> NodeId -> Connection -> GraphView -> Model -> Model
    confirmPhantomNode src dest conn gv model_ =
      GraphEditor.updateLink_graphchange src dest conn gv.computation
      |> \newGraph ->
          commit_change
            { gv
              | computation =
                  let ag = gv.computation in
                  { ag | graph = Graph.remove dest ag.graph }
            }
            newGraph model_

    removeLink : NodeId -> NodeId -> GraphView -> Model -> Model
    removeLink src dest gv model_ =
      GraphEditor.removeLink_graphchange src dest gv.computation
      |> \newGraph -> commit_change gv newGraph model_

  in
    case popMostRecentInteraction model of
      Just (Just gv_uuid, SplittingNode { to_split, left, right } {mainGraph}, model_) ->
        ( if AutoSet.isEmpty left || AutoSet.isEmpty right then
            removeViews [ mainGraph ] model_
          else
            AutoDict.get gv_uuid model_.graph_views
            |> Maybe.andThen
              (\gv ->
                Maybe.combineSecond
                  ( gv
                  , Graph.get to_split gv.computation.graph
                  )
              )
            |> Maybe.map (\(gv, nodeContext) ->
              splitNode nodeContext left gv
              |> \newGraph ->
                  removeViews [ mainGraph ] model_
                  |> commit_change gv newGraph
            )
            |> Maybe.withDefault model_
            |> setProperties
        , Cmd.none
        )
      Just (Just gv_uuid, EditingConnection ({ source, dest, connection, deleteTargetIfCancelled } as alteration) {mainGraph, referenceList}, model_) ->
          -- create such an automatongraph
          ( AutoDict.get (gv_uuid) model_.graph_views   
            |> Maybe.map
              (\gv ->
                if deleteTargetIfCancelled then
                  -- this one comes from a phantom node.
                  removeViews (mainGraph :: referenceList) model_
                  -- because this comes from a phantom node, ensure that we
                  -- remove the 'dest' from the graph before confirming it as
                  -- the previous graph.
                  |> confirmPhantomNode source dest connection gv
                  |> setProperties
                else if AutoSet.isEmpty connection then
                  removeViews (mainGraph :: referenceList) model_
                  |> removeLink source dest gv
                  |> setProperties
                else
                  removeViews (mainGraph :: referenceList) model_
                  |> updateExistingNode source dest connection gv
                  |> setProperties
              )
            |> Maybe.withDefault model_
          , Cmd.none
          )
      Just (Nothing, DeletingPackage to_delete props, model_) ->
        let
          updated_model =
            removeViews (to_delete :: props.directViews ++ props.indirectViews) model_
            |> deletePackageFromModel to_delete
            |> refreshComputationsList
            |> setProperties
        in
          ( updated_model
          , Ports.deleteFromStorage (Uuid.toString to_delete)
          )
      _ ->
        -- _if_ there are things which are yet to be committed, in
        -- the main, then commit them here.
        AutoDict.get model.mainGraphView model.graph_views
        |> Maybe.map
          (\gv ->
            if List.isEmpty gv.undoBuffer then
              ( model -- …there is nothing for me to do!
              , Cmd.none
              )
            else
              let
                new_gv =
                  { gv
                    | computation =
                        GraphEditor.applyChangesToGraph gv.computation
                    -- clear the undo/redo buffers
                    , undoBuffer = []
                    , redoBuffer = []
                  }
                updated_model =
                  updatePackageFromView new_gv.id model
                  |> refreshComputationsList
                  |> setProperties
              in
                ( updated_model
                , new_gv.graphPackage
                  |> Maybe.andThen (\pkg_uuid -> AutoDict.get pkg_uuid model.packages)
                  |> Maybe.map persistPackage
                  |> Maybe.withDefault Cmd.none
                )
          )
        |> Maybe.withDefault ( model, Cmd.none )

viewsContainingPackage : Uuid -> Model -> List GraphView
viewsContainingPackage package_uuid m =
  AutoDict.values m.graph_views
  |> List.filter (\gv -> gv.graphPackage == Just package_uuid)

packagesAndRefs : Model -> List (Uuid, AutoSet.Set String Uuid)
packagesAndRefs {packages} =
  AutoDict.values packages
  |> List.foldl
    (\pkg acc ->
      ( pkg.packageIdentifier
      , Graph.edges pkg.computation.graph
        |> List.foldl
            (\{label} set ->
              AutoSet.foldl
                (\{via} set_ ->
                  case via of
                    ViaGraphReference ref ->
                      AutoSet.insert ref set_
                    _ ->
                      set_
                )
                set
                label
            )
            (AutoSet.empty Uuid.toString)
      ) :: acc
    )
    []

packagesAffectedBy : Uuid -> Model -> List Uuid
packagesAffectedBy uuid model =
  packagesAndRefs model
  |> List.filterMap
    (\(package_uuid, refs) ->
      if AutoSet.member uuid refs then
        Just package_uuid
      else
        Nothing
    )

beginDeletionInteraction : Uuid -> List Uuid -> GraphPackage -> Model -> Model
beginDeletionInteraction package_uuid affected package model =
  -- Erk; it depends on whether you're okay with the consequences.
  let
    all_packages : List (Uuid, AutoSet.Set String Uuid)
    all_packages = packagesAndRefs model
    indirectlyAffected to_check known_indirect =
      let
        (i, o) =
          List.partition
            (\(_, refs) ->
              AutoSet.size (AutoSet.intersect refs known_indirect) > 0
            )
            to_check
      in
        case i of
          [] ->
            -- there is nothing more that matches; we're done!
            known_indirect
          xs ->
            List.map Tuple.first xs
            |> AutoSet.fromList Uuid.toString
            |> AutoSet.union known_indirect
            |> indirectlyAffected o
    affectedSet =
      AutoSet.fromList Uuid.toString affected
      |> Debug.log "affected"
    indirectSet =
      indirectlyAffected
        -- exclude everything in the affectedSet from consideration.
        (List.filter (\(u, _) -> not (AutoSet.member u affectedSet)) all_packages)
        (affectedSet)
      -- and now remove those which are directly affected.
      |> (\s -> AutoSet.diff s affectedSet)
    indirect =
      AutoSet.toList indirectSet
      |> Debug.log "indirect"
    (mainGraph, model_) =
      solvedViewFromComputation
        GraphEditor.coordinateForces
        (Tuple.mapBoth (\v -> v / 3) (\v -> v / 3) model.uiLayout.dimensions.viewport)
        Independent True package.computation model
    (directViews, model__) =
      affected
      |> List.filterMap (\uuid -> AutoDict.get uuid model.packages)
      |> List.sortBy (.created >> Time.posixToMillis >> (*) -1)
      |> List.foldl
        (\pkg (acc, m) ->
          let
            (v, m_) =
              solvedViewFromComputation
                GraphEditor.coordinateForces
                (Tuple.mapBoth (\d -> d / 7) (\d -> d / 7) m.uiLayout.dimensions.viewport)
                Independent True pkg.computation m
          in
            (v :: acc, m_)
        )
        ([], model_)
    (indirectViews, model___) =
      indirect
      |> List.filterMap (\uuid -> AutoDict.get uuid model.packages)
      |> List.sortBy (.created >> Time.posixToMillis >> (*) -1)
      |> List.foldl
        (\pkg (acc, m) ->
          let
            (v, m_) =
              solvedViewFromComputation
                GraphEditor.coordinateForces
                (Tuple.mapBoth (\d -> d / 7) (\d -> d / 7) m.uiLayout.dimensions.viewport)
                Independent True pkg.computation m
          in
            (v :: acc, m_)
        )
        ([], model__)
    props =
      { affectedPackages = affected
      , indirectlyAffectedPackages = indirect
      , mainGraph = mainGraph.id
      , directViews = directViews |> List.map .id
      , indirectViews = indirectViews |> List.map .id
      }
  in
    pushInteractionForStack Nothing (DeletingPackage package_uuid props) model___

deletePackage : GraphPackage -> Model -> ( Model, Cmd Msg )
deletePackage package model =
  -- find out which packages are affected by this one.
  let
    package_uuid = package.packageIdentifier
    affected : List Uuid
    affected =
      packagesAffectedBy package_uuid model
  in
    case affected of
      [] ->
        -- meh, one thing, nobody cares about it. Toss it!
        let
          without_package =
            { model | packages = AutoDict.remove package_uuid model.packages }
          relevant_views =
            viewsContainingPackage package_uuid without_package
            |> List.map .id
        in
          ( removeViews relevant_views without_package
          , Ports.deleteFromStorage (Uuid.toString package_uuid)
          )
      _ ->
        ( beginDeletionInteraction package_uuid affected package model
        , Cmd.none
        )

-- from the execution result, obtain the edges which were taken.
-- Note that multiple edges may be "taken" between two nodes,
-- in the cases of (1) loops and (2) graph-ref intersections.
executing_edges : ExecutionData -> List (NodeId, NodeId)
executing_edges data =
  let
    trace : TransitionTakenData -> (Graph.NodeContext Entity Connection, Int, List (NodeId, NodeId)) -> (Graph.NodeContext Entity Connection, Int, List (NodeId, NodeId))
    trace {matching} (ctx, recency, acc) =
      IntDict.toList ctx.outgoing
      |> List.find (\(_, conn) -> AutoSet.member matching conn)
      |> Maybe.andThen (Tuple.first >> flip Graph.get data.computation.graph)
      |> Maybe.map
        (\newCtx ->
          ( newCtx
          , recency - 1
          , (ctx.node.id, newCtx.node.id) :: acc
          )
        )
      |> Maybe.withDefault
        ( ctx, recency, acc )
  in
    Graph.get data.computation.root data.computation.graph
    |> Maybe.map (\ctx ->
      List.foldl
        trace
        (ctx, List.length data.transitions, [])
        ( data.transitions
          -- |> Debug.log "Traced transitions"
        )
      |> (\(_, _, acc) -> List.reverse acc)
      -- |> Debug.log "Executed edges"
    )
    |> Maybe.withDefault []

expandStep : Int -> List ExecutionData -> ExecutionProperties -> Model -> Model
expandStep n results props model =
  let
    relevantSteps = -- ordered from most recent to least recent
      List.dropWhile (.step >> (/=) n) results
    selectedStep =
      List.head relevantSteps
    with_graphview =
      Maybe.map (\step ->
        let
          edges = executing_edges step
          (gv_uuid, model__) =
            solvedViewFromComputation
              GraphEditor.spreadOutForces
              (Tuple.mapBoth (\v -> v - 128) (\v -> v - 40) model.uiLayout.dimensions.bottomPanel)
              Independent True
              step.computation
              model
            |>(\({id}, m) ->
                ( id
                , { m
                    | graph_views =
                        AutoDict.update id
                          (Maybe.map <| centerAndHighlight edges)
                          m.graph_views
                  }
                )
              )
        in
          ( gv_uuid , model__ )
      )
      selectedStep
  in
    Maybe.map
      (\(gv_uuid, model__) ->
        replaceInteraction Nothing
            (Executing results
              { props
                | expandedSteps = IntDict.insert n gv_uuid props.expandedSteps
              }
            )
            model__
        |> setProperties
      )
      with_graphview
    |> Maybe.withDefault model

step_execution : GraphView -> (List ExecutionData -> List ExecutionData) -> Test -> Model -> Model
step_execution orig_graphview execution_function test model =
  let
    (previously_executed, previous_props, interactionFunction) =
      case peekInteraction Nothing model.interactionsDict of
        Just ( Executing results props ) ->
          ( results
          , Just props
          , \new_hx new_props ->
              replaceInteraction Nothing (Executing new_hx new_props)
          )
        _ ->
          ( DFA.load test.input (toResolutionDict model.packages) orig_graphview.computation
            |> List.singleton
          , Nothing
          , \new_hx new_props ->
              pushInteractionForStack Nothing (Executing new_hx new_props)
          )
    applied_step : List ExecutionData
    applied_step =
      execution_function previously_executed
    head_step =
      List.head applied_step
    (new_graphview_uuid, model_) =
      Maybe.map
        (\step ->
          let
            edges = executing_edges step
          in
          solvedViewFromComputation
            GraphEditor.spreadOutForces
            model.uiLayout.dimensions.mainEditor
            Independent True
            step.computation
            model
          |>(\({id}, m) ->
              ( id
              , { m
                  | graph_views =
                      AutoDict.update id
                        (Maybe.map <| centerAndHighlight edges)
                        m.graph_views
                }
              )
            )
        ) head_step
      |> Maybe.withDefault (orig_graphview.id, model)
    updated_props =
      Maybe.withDefault
        { expandedSteps = IntDict.empty
        }
        previous_props
  in
    interactionFunction applied_step updated_props model_
    |> (\model__ -> { model__ | mainGraphView = new_graphview_uuid })
    |> setProperties

update_package : Uuid -> PackageMsg -> Model -> ( Model, Cmd Msg )
update_package pkg_uuid msg model =
  let
    change_input s key p =
      -- it's okay to have a test-uuid with no corresponding test.
      -- When we write to it, the data will appear.
      if String.isEmpty s then
        delete_test key p
      else
        { p | tests =
          AutoDict.update key
            ( Maybe.map
                (\test ->
                  { test
                    | input = s
                    , result =
                        DFA.load s (toResolutionDict model.packages) p.computation
                        |> DFA.run
                        |> List.head
                        |> Maybe.andThen .finalResult
                        |> Maybe.withDefault (InternalError "no result")
                  }
                )
              >> Maybe.orElse
                (Just { input = s
                  , expectation = ExpectAccepted
                  , result =
                      DFA.load s (toResolutionDict model.packages) p.computation
                      |> DFA.run
                      |> List.head
                      |> Maybe.andThen .finalResult
                      |> Maybe.withDefault (InternalError "no result")
                  })
            )
            p.tests
        }
    flip_acceptance key p =
      -- it's okay to have a test-uuid with no corresponding test.
      -- When we write to it, the data will appear.
      { p | tests =
        AutoDict.update key
          (Maybe.map
            (\test ->
              { test
                | expectation =
                    case test.expectation of
                      ExpectAccepted -> ExpectRejected
                      ExpectRejected -> ExpectAccepted
              }
            )
          )
          p.tests
      }
    delete_test test_uuid p =
      -- it's okay to have a test-uuid with no corresponding test.
      -- When we write to it, the data will appear.
      { p | tests = AutoDict.remove test_uuid p.tests }
    select_test test_uuid p =
      { p | currentTestKey = test_uuid }
    create_test test_uuid p =
      { p | currentTestKey = test_uuid }
    change_description : String -> { a | computation : { b | description : Maybe String } } -> { a | computation : { b | description : Maybe String } }
    change_description d p =
      if String.isEmpty d then
        { p | computation = let computation = p.computation in { computation | description = Nothing } }
      else
        { p | computation = let computation = p.computation in { computation | description = Just d } }
    update_model p model_ =
      { model_
        | packages = AutoDict.insert pkg_uuid p model_.packages
      }
  in
  case msg of
    SelectPackage ->
      ( AutoDict.get pkg_uuid model.packages
        |> Maybe.map
          (\pkg ->
            removeViews [ model.mainGraphView ] model
            |> solvedViewFromComputation GraphEditor.coordinateForces
                model.uiLayout.dimensions.mainEditor MainEditor False pkg.computation
            |> linkToPackage pkg.packageIdentifier
            |> Tuple.second
            |> setProperties
          )
        |> Maybe.withDefault model
      , Cmd.none
      )

    DeletePackage ->
      AutoDict.get pkg_uuid model.packages
      |> Maybe.map (flip deletePackage model)
      |> Maybe.withDefault ( model, Cmd.none )

    CreateNewTestInput ->
      AutoDict.get pkg_uuid model.packages
      |> Maybe.map
        (\pkg ->
          let
            (test_uuid, newSeed) = Random.step Uuid.generator model.randomSeed
            p = create_test test_uuid pkg
            updated_model =
              update_model p model
              |> (\m -> { m | randomSeed = newSeed })
          in
            case popInteraction Nothing updated_model of
              Just ( Executing _ _ , model_ ) ->
                -- stop executing, if I was executing.
                -- I've got a different(?) test selected now!
                ( model_
                , persistPackage p
                )
              _ ->
                ( updated_model
                , persistPackage p
                )
        )
      |> Maybe.withDefault ( model, Cmd.none )

    SelectTest test_uuid ->
      AutoDict.get pkg_uuid model.packages
      |> Maybe.map
        (\pkg ->
          let
            p = select_test test_uuid pkg
            updated_model =
              update_model p model
          in
            case popInteraction Nothing updated_model of
              Just ( Executing _ _ , model_ ) ->
                -- stop executing, if I was executing.
                -- I've got a different(?) test selected now!
                ( model_
                , persistPackage p
                )
              _ ->
                ( updated_model
                , persistPackage p
                )
        )
      |> Maybe.withDefault ( model, Cmd.none )

    DeleteTestInput test_uuid ->
      AutoDict.get pkg_uuid model.packages
      |> Maybe.map
        (\pkg ->
          let
            p = delete_test test_uuid pkg
            updated_model =
              update_model p model
          in
            ( updated_model
            , persistPackage p
            )
        )
      |> Maybe.withDefault ( model, Cmd.none )

    UpdateTestInput s ->
      AutoDict.get pkg_uuid model.packages
      |> Maybe.map
        (\pkg ->
          let
            p = change_input s pkg.currentTestKey pkg
            updated_model =
              update_model p model
          in
            ( updated_model
            , persistPackage p
            )
        )
      |> Maybe.withDefault ( model, Cmd.none )

    UpdateComputationDescription s ->
      AutoDict.get pkg_uuid model.packages
      |> Maybe.map
        (\pkg ->
          let
            p = change_description s pkg
            updated_model =
              update_model p model
              |>(\m ->
                  { m
                    | graph_views =
                        viewsContainingPackage pkg_uuid m
                        |> List.foldl
                          (\gv -> AutoDict.insert gv.id (change_description s gv))
                          m.graph_views
                    , packages =
                        AutoDict.insert pkg_uuid p m.packages
                  }
                )
          in
            ( updated_model
            , persistPackage p
            )
        )
      |> Maybe.withDefault ( model, Cmd.none )

    FlipAcceptanceCondition ->
      AutoDict.get pkg_uuid model.packages
      |> Maybe.map
        (\pkg ->
          let
            p = flip_acceptance pkg.currentTestKey pkg
            updated_model =
              update_model p model
          in
            ( updated_model
            , persistPackage p
            )
        )
      |> Maybe.withDefault ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg {- |> (\v -> if v == ForceDirectedMsg FDG.Tick then v else Debug.log "MESSAGE" v) -} of
    GraphViewMsg uuid gv_msg ->
      update_graphview uuid gv_msg model

    PackageMsg uuid pkg_msg ->
      update_package uuid pkg_msg model

    DragSplitter shouldStop coord ->
      ( case popInteraction (Nothing) model of
          Just (DraggingSplitter movement, model_) ->
            if shouldStop then -- do not, in fact, drag the splitter.
              model_
              |> updateMainEditorDimensions
              |> setProperties
            else -- such a drag.
              { model
                | uiLayout =
                    dragSplitter coord movement model.uiConstants model.uiLayout
              }
              |> updateMainEditorDimensions
              |> setProperties
          _ ->
            model
      , Cmd.none
      )

    QuickInput input ->
      ( case mostRecentInteraction model of
          Just (key_uuid, EditingConnection alteration props) ->
            handleConnectionEditorInput key_uuid input alteration props model
          Just (key_uuid, SplittingNode data props) ->
            String.toList input
            |> List.head
            |> Maybe.map
                (\ch -> nodeSplitSwitch props key_uuid (ViaCharacter ch) data model)
            |> Maybe.withDefault model
          _ ->
            model
      , Cmd.none
      )

    ToggleConnectionTransition via ->
        ( case mostRecentInteraction model of
            Just (key_uuid, EditingConnection ({connection} as alteration) props) ->
              replaceInteraction key_uuid 
                ( EditingConnection
                    { alteration | connection = toggleConnectionTransition via connection }
                    props
                )
                model
            Just (key_uuid, SplittingNode data props) ->
              nodeSplitSwitch props key_uuid via data model
            _ ->
              model
        , Cmd.none
        )

    SelectNavigation ComputationsIcon ->
      ( selectNavIcon ComputationsIcon model
        |> refreshComputationsList
        |> setProperties
      , Cmd.none
      )

    SelectNavigation TestsIcon ->
      ( selectNavIcon TestsIcon model
        |> setProperties
      , Cmd.none
      )

    -- SelectNavigation _ ->
    --   Debug.todo "SelectNavigation _ not implemented."

    SelectTool item ->
      ( { model
          | uiLayout =
              let uiLayout = model.uiLayout in
              { uiLayout
                | selected =
                    let selected = uiLayout.selected in
                    { selected | bottomPanel = item }
              }
        }
      , Cmd.none
      )

    OnResize dims ->
      ( resizeViewport dims model
      , Cmd.none
      )

    ToggleAreaVisibility where_ ->
      ( { model
          | uiLayout = toggleAreaVisibility where_ model.uiLayout
        }
        |> updateMainEditorDimensions
        |> setProperties
      , Cmd.none
      )
    StartDraggingSplitter movement ->
      ( pushInteractionForStack (Nothing) (DraggingSplitter movement) model
        |> setProperties
      , Cmd.none
      )

    Escape ->
      ( if model.properties.canEscape then
          case popMostRecentInteraction model of
            Nothing -> -- hmm, let's look at the local interactions of the main editor window.
              -- huh!  Looks like there's nothing to do!  So why was I called??
              -- Ideally, I shouldn't even spawn an event if there's nothing to do.
              model
            Just (Just uuid, ChoosingDestinationFor _ (NewNode _ _), _) ->
              cancelNewNodeCreation uuid model -- this will pop, and also handle graph/UI changes too.
              |> setProperties
            Just (Just uuid, ChoosingDestinationFor source (ExistingNode dest _), _) ->
              removePhantomLink uuid source dest model
            Just (Just uuid, EditingConnection alteration {referenceList, mainGraph}, model_) ->
              handleConnectionRemoval uuid alteration (mainGraph :: referenceList) model_ 
              |> setProperties
            Just (Just _, SplittingNode _ props, model_) ->
              removeViews [ props.mainGraph ] model_
              |> setProperties
            Just (Nothing, DeletingPackage _ {mainGraph, directViews, indirectViews}, model_) ->
              removeViews (mainGraph :: directViews ++ indirectViews) model_
              |> setProperties
            Just (_, _, model_) ->
              setProperties model_ -- yay, I could pop from the global
        else
          model
      , Cmd.none
      )

    EditConnection coordinates uuid src dest connection ->
      ( editConnection (Just uuid) coordinates
          { source = src
          , dest = dest
          , connection = connection
          , deleteTargetIfCancelled = False
          }
          model
      , Cmd.none
      )

    Undo ->
      ( { model
          | graph_views =
              AutoDict.update model.mainGraphView
                (Maybe.map (\graph_view ->
                    case (mostRecentInteraction model, graph_view.undoBuffer) of
                      (_, []) ->
                        graph_view
                      (Just _, _) ->
                        graph_view -- do not permit undo/redo while I'm performing any operation.
                      (Nothing, h::t) ->
                        { graph_view
                          | undoBuffer = t
                          , redoBuffer = graph_view.computation :: graph_view.redoBuffer
                          , computation = h
                        }
                ))
                model.graph_views
        }
        |> setProperties
      , Cmd.none
      )

    Redo ->
      ( { model
          | graph_views =
              AutoDict.update model.mainGraphView
                (Maybe.map (\graph_view ->
                    case (mostRecentInteraction model, graph_view.redoBuffer) of
                      (_, []) ->
                        graph_view
                      (Just _, _) ->
                        graph_view -- do not permit undo/redo while I'm performing any operation.
                      (Nothing, h::t) ->
                        { graph_view
                          | redoBuffer = t
                          , undoBuffer = graph_view.computation :: graph_view.redoBuffer
                          , computation = h
                        }
                ))
                model.graph_views
        }
        |> setProperties
      , Cmd.none
      )

    Confirm ->
      commitOrConfirm model

    CreateNewPackage ->
      ( model
      , Task.perform TimeValueForPackage Time.now
      )

    TimeValueForPackage posix ->
      let
        (testUuid, mainUuid, model_) = getUuid2 model
        pkg =
          createNewPackage
            testUuid
            mainUuid
            posix
            ( { graph =
                  Graph.fromNodesAndEdges
                    [ Graph.Node 0 (entity 0 NoEffect |> (\e -> { e | x = 0, y = 0 }))
                    ]
                    []
              , description = Nothing
              , root = 0
              }
            )
        updated_model =
          { model_ | packages = AutoDict.insert mainUuid pkg model.packages }
        ( _, with_graph_view ) =
          solvedViewFromComputation
            GraphEditor.coordinateForces
            model.uiLayout.dimensions.mainEditor
            MainEditor
            False
            pkg.computation
            updated_model
      in
        ( with_graph_view
          |> refreshComputationsList
          |> setProperties
        , persistPackage pkg
        )

    Step ->
      AutoDict.get model.mainGraphView model.graph_views
      |> Maybe.andThen
        (\gv ->
          Maybe.combineSecond
            ( gv
            , AutoDict.get model.selectedPackage model.packages
              |> Maybe.andThen (\pkg -> AutoDict.get pkg.currentTestKey pkg.tests)
            )
        )
      |> Maybe.map
        (\(gv, test) ->
          ( step_execution gv
              (\prevList ->
                case prevList of
                  h::t -> DFA.step h :: h :: t
                  [] -> []
              )
              test model
          , Cmd.none
          )
        )
      |> Maybe.withDefaultLazy
        (\() ->
          Debug.log "[update→Step] Could not find the view?" model.mainGraphView |> \_ ->
          println "[update→Step] Or could not find the package from that view?"
          ( model, Cmd.none )
        )

    Run ->
      AutoDict.get model.mainGraphView model.graph_views
      |> Maybe.andThen
        (\gv ->
          Maybe.combineSecond
            ( gv
            , AutoDict.get model.selectedPackage model.packages
              |> Maybe.andThen (\pkg -> AutoDict.get pkg.currentTestKey pkg.tests)
            )
        )
      |> Maybe.map
        (\(gv, test) ->
          ( step_execution gv
              (\input ->
                case input of
                  h::t -> DFA.run h ++ (h::t)
                  [] -> []
              )
              test model
          , Cmd.none
          )
        )
      |> Maybe.withDefaultLazy
        (\() ->
          Debug.log "[update→Run] Could not find the view?" model.mainGraphView |> \_ ->
          println "[update→Run] Or could not find the package from that view?"
          ( model, Cmd.none )
        )

    StepBack ->
      AutoDict.get model.mainGraphView model.graph_views
      |> Maybe.andThen
        (\gv ->
          Maybe.combineSecond
            ( gv
            , AutoDict.get model.selectedPackage model.packages
              |> Maybe.andThen (\pkg -> AutoDict.get pkg.currentTestKey pkg.tests)
            )
        )
      |> Maybe.map
        (\(gv, test) ->
          ( step_execution gv
              (\input ->
                case input of
                  [h] -> [h] -- can't step back from the initial state.
                  _::t -> t
                  [] -> []
              )
              test model
          , Cmd.none
          )
        )
      |> Maybe.withDefaultLazy
        (\() ->
          Debug.log "[update→StepBack] Could not find the view?" model.mainGraphView |> \_ ->
          println "[update→StepBack] Or could not find the package from that view?"
          ( model, Cmd.none )
        )

    ResetComputation ->
      case popInteraction Nothing model of
        Just ( Executing _ _ , model_ ) ->
          ( setProperties model_, Cmd.none )
        _ ->
          ( model, Cmd.none )

    ToggleDebugStep n ->
      ( case peekInteraction Nothing model.interactionsDict of
          Just ( Executing results props ) ->
            case IntDict.get n props.expandedSteps of
              Just id ->
                -- get rid of this view.
                { model
                  | graph_views = AutoDict.remove id model.graph_views
                }
                -- and now get rid of the expanded step
                |> replaceInteraction Nothing
                    (Executing results
                      { props
                        | expandedSteps =
                            IntDict.remove n props.expandedSteps
                      }
                    )
                |> setProperties
              Nothing ->
                model
          _ ->
            model
      , Cmd.none
      )

    CrashWithMessage err ->
      Debug.todo err

isPassingTest : Test -> Maybe Bool
isPassingTest test =
  case test.result of
    InternalError _ -> Nothing
    Accepted -> Just <| test.expectation == ExpectAccepted
    Rejected -> Just <| test.expectation == ExpectRejected
    NoMatchingTransitions -> Just <| test.expectation == ExpectRejected

isFailingTest : Test -> Maybe Bool
isFailingTest = isPassingTest >> Maybe.map not









{-
  **************************************
  Interaction → interactive capabilities
  **************************************
-}

{-| Given host coordinates, translate them to guest-viewport coordinates -}
translateHostCoordinates : (Float, Float) -> GraphView -> (Float, Float)
translateHostCoordinates (x, y) graph_view =
  let
    ( x_host, y_host ) = graph_view.host_coordinates
    ( w_host, h_host ) = graph_view.host_dimensions
    ( x_guest, y_guest ) = graph_view.guest_coordinates
    ( w_guest, h_guest ) = graph_view.guest_dimensions
    ( pan_x, pan_y ) = graph_view.pan
    translate_dimension coord coord_host coord_guest dim_host dim_guest pan =
      if coord <= coord_host then
        coord_guest + pan -- on the (left/top) edge
      else if coord >= coord_host + dim_host then
        coord_guest + dim_guest + pan -- on the (right/bottom) edge
      else
        -- this is in the actual area
        let
          ratio = dim_guest / dim_host
        in
          (coord - coord_host) * ratio + coord_guest + pan
    translate_x =
      translate_dimension x x_host x_guest w_host w_guest pan_x
    translate_y =
      translate_dimension y y_host y_guest h_host h_guest pan_y
  in
    ( translate_x, translate_y )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    panSubscription =
      pan
        ( D.decodeValue
            ( D.map3 (\uuid pan_x pan_y -> Pan pan_x pan_y |> GraphViewMsg uuid)
                (D.field "uuid" Uuid.decoder)
                (D.at ["pan", "pan_x"] D.int)
                (D.at ["pan", "pan_y"] D.int)
            )
          >> Result.withDefault (CrashWithMessage "Invalid pan message from JavaScript")
        )
    stopPanSubscription =
      stopPan
        ( D.decodeValue
            ( D.map (\uuid -> GraphViewMsg uuid StopPan) Uuid.decoder )
          >> Result.withDefault (CrashWithMessage "Invalid stop-pan message from JavaScript")
        )
    keyboardSubscription =
      BE.onKeyDown
        ( D.map2
            (\key ctrlPressed -> ( key, ctrlPressed ))
            (D.field "key" D.string)
            (D.field "ctrlKey" D.bool)
          |> D.andThen
            (\(key, ctrlPressed) ->
              case String.toLower key of
                "escape" ->
                  if model.properties.canEscape then                        
                    case mostRecentInteraction model of
                      Just (_, EditingConnection _ _) ->
                        if ctrlPressed then
                          D.succeed Escape
                        else
                          D.fail "Need to Ctrl-escape out of editing a connection"
                      Just (_, SplittingNode _ _) ->
                        if ctrlPressed then
                          D.succeed Escape
                        else
                          D.fail "Need to Ctrl-escape out of splitting a node"
                      _ ->
                        D.succeed Escape
                  else
                    D.fail "Escape not permitted at this point"
                "enter" ->
                  case mostRecentInteraction model of
                    Just (_, EditingConnection _ _) ->
                      if ctrlPressed then
                        D.succeed Confirm
                      else
                        D.fail "Need to Ctrl-enter to confirm editing a connection"
                    Just (_, SplittingNode _ _) ->
                      if ctrlPressed then
                        D.succeed Confirm
                      else
                        D.fail "Need to enter to confirm splitting a node"
                    _ ->
                      D.succeed Confirm
                "z" ->
                  if ctrlPressed then
                    D.succeed Undo
                  else
                    D.fail "Need to Ctrl-z to undo"
                "y" ->
                  if ctrlPressed then
                    D.succeed Redo
                  else
                    D.fail "Need to Ctrl-y to redo"
                _ ->
                  D.fail "Untrapped"
            )
        )
    resizeSubscription =
      BE.onResize (\w h -> OnResize (toFloat w, toFloat h) {- |> Debug.log "Raw resize values" -})
    splitterSubscriptions =
      case peekInteraction Nothing model.interactionsDict of
        Just (DraggingSplitter LeftRight) ->
          let
            -- navigatorbarwidth + splitterwidth/2
            offset = -(48 + 8 / 2)
          in
            [ BE.onMouseMove (D.map ((+) offset >> DragSplitter False) (D.field "clientX" D.float))
            , BE.onMouseUp (D.map ((+) offset >> DragSplitter True) (D.field "clientX" D.float))
            ]
        Just (DraggingSplitter UpDown) ->
          [ BE.onMouseMove (D.map (DragSplitter False) (D.field "clientY" D.float))
          , BE.onMouseUp (D.map (DragSplitter True) (D.field "clientY" D.float))
          ]
        _ ->
          []
    nodeMoveSubscriptions =
      let
        createNodeMoveSubscription uuid =
          AutoDict.get uuid model.graph_views
          |> Maybe.map
            (\graph_view ->
                BE.onMouseMove
                  ( D.map2
                      (\x y ->
                        MoveNode
                          (graph_view |> translateHostCoordinates (x, y))
                        |> GraphViewMsg uuid
                      )
                      (D.field "clientX" D.float)
                      (D.field "clientY" D.float)
                  )
            )
          |> Maybe.withDefault Sub.none
      in
        AutoDict.toList model.interactionsDict
        |> List.filterMap
          (\(maybeUuid, (_, stack)) ->
            case (maybeUuid, stack) of
              (Just uuid, ChoosingDestinationFor _ _ :: _) ->
                Just <| createNodeMoveSubscription uuid
              (Just uuid, DraggingNode _ :: _) ->
                Just <| Sub.batch 
                  [ createNodeMoveSubscription uuid
                  , BE.onMouseUp (D.succeed (GraphViewMsg uuid <| StopDraggingNode))
                  ]
              _ ->
                Nothing
          )
  in
    Sub.batch
      ( resizeSubscription ::
        panSubscription ::
        stopPanSubscription ::
        keyboardSubscription ::
        nodeMoveSubscriptions ++
        splitterSubscriptions
      )













{- ***********************************************************
   VIEW FUNCTIONS
   ***********************************************************
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
          ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣄⠀⠀⠀⠀⠀⠀⣠⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
          ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠚⠻⠿⡇⠀⠀⠀⠀⢸⠿⠟⠓⠀⠀⠀⠀⠀⠀⠀⠀⠀
          ⠀⠀⠀⠀⠀⠀⣠⣴⣾⣿⣶⣦⡀⢀⣤⣤⡀⢀⣴⣶⣿⣷⣦⣄⠀⠀⠀⠀⠀⠀
          ⠀⠀⠀⠀⠀⣴⣿⣿⣿⣿⣿⣿⡇⢸⣿⣿⡇⢸⣿⣿⣿⣿⣿⣿⣦⠀⠀⠀⠀⠀
          ⠀⠀⠀⠀⠘⠋⣉⡉⠙⠛⢿⣿⡇⢸⣿⣿⡇⢸⣿⡿⠛⠋⢉⣉⠙⠃⠀⠀⠀⠀
          ⠀⠀⢀⣤⣾⡛⠛⠛⠻⢷⣤⡙⠃⢸⣿⣿⡇⠘⢋⣤⣾⡟⠛⠛⠛⠷⣤⡀⠀⠀
          ⠀⢀⣾⣿⣿⡇⠀⠀⠀⠀⠙⣷⠀⠘⠛⠛⠃⠀⣾⣿⣿⣿⠀⠀⠀⠀⠈⢷⡀⠀
          ⠀⢸⡇⠈⠉⠀⠀⠀⠀⠀⠀⢸⡆⢰⣿⣿⡆⢰⡇⠀⠉⠁⠀⠀⠀⠀⠀⢸⡇⠀
          ⠀⠸⣧⠀⠀⠀⠀⠀⠀⠀⢀⡾⠀⠀⠉⠉⠀⠀⢷⡀⠀⠀⠀⠀⠀⠀⠀⣼⠇⠀
          ⠀⠀⠙⢷⣄⣀⠀⠀⣀⣤⡾⠁⠀⠀⠀⠀⠀⠀⠈⢷⣤⣀⠀⠀⣀⣠⡾⠋⠀⠀
          ⠀⠀⠀⠀⠉⠛⠛⠛⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠛⠛⠛⠉⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
-}













debugViewDimensions : Bool
debugViewDimensions = True

debugElement : String -> String -> Html a
debugElement otherClass s =
  if debugViewDimensions then
    div
      [ HA.class "debug dimensions-parent" ]
      [ div
        [ HA.class <| "dimensions " ++ otherClass ]
        [ text s ]
      ]
  else
    div
      []
      []

debugDimensions : Dimensions -> Html a
debugDimensions (w, h) =
  debugElement "width height" (String.fromFloat w ++ "×" ++ String.fromFloat h)

debugHeight : Float -> Html a
debugHeight h =
  debugElement "height" (String.fromFloat h)

debugWidth : Float -> Html a
debugWidth w =
  debugElement "width" (String.fromFloat w)

viewNavigatorsArea : Model -> Html Msg
viewNavigatorsArea model =
  div
    [ HA.class "sidebar-container" ]
    [ if not model.uiLayout.open.sideBar then
        div [] []
      else
        div
          [ HA.class "navigation-bar" ]
          [ button
              [ HA.classList
                  [ ("navigation-icon computation-icon", True)
                  , ("active", model.uiLayout.selected.sideBar == ComputationsIcon)
                  ]
              , HA.title "Computations"
              , (model.uiLayout.selected.sideBar /= ComputationsIcon)
                |> thenPermitInteraction (HE.onClick (SelectNavigation ComputationsIcon))
              ]
              [ text "📁"]
          , button
              [ HA.classList
                  [ ("navigation-icon test-icon", True)
                  , ("active", model.uiLayout.selected.sideBar == TestsIcon)
                  ]
              , HA.title "Tests"
              , (model.uiLayout.selected.sideBar /= TestsIcon)
                |> thenPermitInteraction (HE.onClick (SelectNavigation TestsIcon))
              ]
              [ text "🧪"
              , AutoDict.get model.selectedPackage model.packages
                |> Maybe.map
                  (\pkg ->
                    let
                      (pass, fail, error) =
                        AutoDict.toList pkg.tests
                        |> List.foldl
                          (\(_, test) (p, f, e) ->
                            case isPassingTest test of
                              Just True ->
                                (p + 1, f, e)
                              Just False ->
                                (p, f + 1, e)
                              Nothing ->
                                (p, f, e + 1)
                          )
                          (0, 0, 0)
                      (testClass, number, testTitle) =
                        case (pass, fail, error) of
                          (0, 0, 0) ->
                            ( "nothing" -- --dracula-foreground
                            , "…"
                            , "No tests exist"
                            )
                          (_, 0, 0) -> -- no failures, no errors, and only passes.
                            ( "all-pass" -- --dracula-green
                            , "💯"
                            , "All tests passed!"
                            )
                          (0, _, 0) -> -- no passes, no errors, only failures.
                            ( "all-fail" -- --dracula-red
                            , "😵‍💫"
                            , "All tests failed!"
                            )
                          (_, _, 0) -> -- some passes, some failures, no errors
                            ( "some-fail" -- --dracula-pink
                            , String.fromInt fail
                            , if fail == 1 then
                                "1 test failed"
                              else
                                String.fromInt fail ++ " tests failed"
                            )
                          _ -> -- internal error exists!
                            ( "internal-error" -- --dracula-yellow
                            , "❓"
                            , "Some tests did not complete due to an internal error! This indicates a problem with the computation system. Please report it to the developer."
                            )
                    in
                      div
                        [ HA.class ("quick-status " ++ testClass)
                        , HA.title testTitle
                        ]
                        [ text number ]
                  )
                |> Maybe.withDefault (text "")
              ]
          ]
    , if not model.uiLayout.open.sideBar then
        text ""
      else
        div
          [ HA.class "sidebar"
          , HA.css
              [ Css.width <| Css.px <|
                  if model.uiLayout.open.sideBar then
                    Tuple.first model.uiLayout.dimensions.sideBar
                  else
                    0
              ]
          ]
          [ debugDimensions model.uiLayout.dimensions.sideBar
          , case model.uiLayout.selected.sideBar of
              ComputationsIcon ->
                viewComputationsSidebar model
              TestsIcon ->
                AutoDict.get model.selectedPackage model.packages
                |> Maybe.map (flip viewTestsSidebar model)
                |> Maybe.withDefault
                  (div [ HA.class "error graph-not-loaded" ] [ text "⚠ No graph is loaded in the editor" ])
          ]
    ]

viewTestsSidebar : GraphPackage -> Model -> Html Msg
viewTestsSidebar pkg {properties} =
  let
    (expectAccept, expectReject) =
      AutoDict.toList pkg.tests
      |> List.partition (Tuple.second >> .expectation >> (==) ExpectAccepted)
    displayTests headingHtml tx =
      case tx of
        [] ->
          text ""
        _ ->
          div
            []
            [ headingHtml
            , ul
                [ HA.class "input-list" ]
                ( List.sortBy (Tuple.second >> .input) tx
                  |> List.map
                    (\(key, test) ->
                      li
                        [ HA.classList
                            [ ("test-input", True)
                            , ("selected", key == pkg.currentTestKey)
                            , ("disabled", not properties.canLoadTestInput)
                            ]
                        , HA.title "Edit"
                        ]
                        [ viewTestItemInPanel (key, test) ]
                    )
                )
            ]
    viewTestItemInPanel : (Uuid, Test) -> Html Msg
    viewTestItemInPanel (key, test) =
      let
        testStatus = isFailingTest test
      in
        div 
          [ HA.classList
              [ ("failing", testStatus == Just True)
              , ("passing", testStatus == Just False)
              , ("error", testStatus == Nothing)
              ]
          , properties.canLoadTestInput
            |> thenPermitInteraction (HE.onClick (PackageMsg pkg.packageIdentifier <| SelectTest key))
          ]
          [ span
              [ HA.classList
                  [ ("test-input-container", True)
                  , ("disabled", not properties.canLoadTestInput)
                  ]
              , case test.result of
                  InternalError s -> HA.title s
                  _ -> HA.hidden False -- … pretty irrelevant. Which is what I want.
              ]
              [ text test.input ]
          , if properties.canDeleteTestInput && not (key == pkg.currentTestKey) then
              div
                [ HA.classList
                    [ ("button", True)
                    , ("disabled", not properties.canDeleteTestInput)
                    ]
                , HA.title "Delete test"
                , HE.onClick (PackageMsg pkg.packageIdentifier <| DeleteTestInput key)
                ]
                [ text "🚮" ]
            else
              text ""
          ]
  in
    div
      [ HA.class "sidebar-content tests-explorer" ]
      [ h1
          [ HA.class "sidebar-title" ]
          [ text "Test Inputs "
          , if properties.canCreateTestInput then
              button
                [ HA.class "add-button"
                , HA.title "Add new test"
                , HE.onClick (PackageMsg pkg.packageIdentifier CreateNewTestInput)
                ]
                [ text "➕" ]
            else
              text ""
          ]
      , div
          [ HA.class "package-description" ]
          [ pkg.computation.description
            |> Maybe.map text
            |> Maybe.withDefault (text "")
          ]
      , div
          [ HA.class "test-inputs expect-accepted" ]
          [ displayTests
              ( div
                  [ HA.class "title" ]
                  [ span [] [ text "Should be " ]
                  , span [ HA.class "emphasis" ] [ text "accepted" ]
                  ]
              )
              expectAccept
          ]
      , div
          [ HA.class "test-inputs expect-rejected" ]
          [ displayTests
              ( div
                  [ HA.class "title" ]
                  [ span [] [ text "Should be " ]
                  , span [ HA.class "emphasis" ] [ text "rejected" ]
                  ]
              )
              expectReject
          ]




    ]

viewComputationsSidebar : Model -> Html Msg
viewComputationsSidebar model =
  div
    [ HA.class "sidebar-content computations" ]
    [ h1
        [ HA.class "sidebar-title" ]
        [ text "Computations "
        , if model.properties.canCreateNewPackage then
            button
              [ HA.class "add-button"
              , HA.title "Create new computation"
              , HE.onClick CreateNewPackage
              ]
              [ text "➕" ]
          else
            text ""
        ]
    , let
        mainPkgUuid =
          AutoDict.get model.mainGraphView model.graph_views
          |> Maybe.andThen .graphPackage
      in
      div
        [ HA.class "computations-explorer" ]
        ( List.filterMap
            (\pkg_uuid ->
                AutoDict.get model.mainGraphView model.graph_views
                |> Maybe.map
                  (\graph_view ->
                    div
                      [ HA.class "package"
                      , graph_view.properties.canSelectPackage
                        |> thenPermitInteraction (HE.onClick (PackageMsg pkg_uuid SelectPackage))
                      ]
                      [ viewGraph graph_view
                      , div
                          [ HA.class "description" ]
                          [ graph_view.computation.description
                            |> Maybe.withDefault "(no description)"
                            |> text
                          ]
                      , if graph_view.graphPackage == mainPkgUuid || not graph_view.properties.canDeletePackage then
                          text ""
                        else
                          div
                            [ HA.class "delete-button"
                            , HA.title "Delete"
                            , HE.onClick (PackageMsg pkg_uuid DeletePackage)
                            ]
                            [ text "🚮" ]
                      ]
                  )
            )
            model.computationsExplorer
        )
    ]

viewCollapsedAreaButton : SplitterMovement -> Html Msg
viewCollapsedAreaButton movement =
  let
    (movementClass, targetArea) =
      case movement of
        LeftRight ->
          ( "leftright", NavigatorsArea )
        UpDown ->
          ( "updown", ToolsArea )
    collapseIcon =
      svg
        [ Svg.Styled.Attributes.class ("collapse-icon " ++ movementClass)
        , Svg.Styled.Attributes.viewBox "4 4 10 8"
        ]
        [ Svg.Styled.path
            [ Svg.Styled.Attributes.d "M10 12L6 8l4-4" ]
            []
        ]
  in
    button
      [ HA.class <| "collapse-button " ++ movementClass ++ " collapsed"
      , HA.title "Expand"
      , HE.onClick (ToggleAreaVisibility targetArea)
      ]
      [ collapseIcon ]

-- unconditionally display a splitter
viewSplitter : Int -> SplitterMovement -> Model -> Bool -> Html Msg
viewSplitter zIdx movement model areaOpen =
  let
    (movementClass, targetArea) =
      case movement of
        LeftRight ->
          ( "leftright", NavigatorsArea )
        UpDown ->
          ( "updown", ToolsArea )
    collapseIcon =
      svg
        [ Svg.Styled.Attributes.class ("collapse-icon " ++ movementClass)
        , Svg.Styled.Attributes.viewBox "4 4 10 8"
        ]
        [ Svg.Styled.path
            [ Svg.Styled.Attributes.d "M10 12L6 8l4-4" ]
            []
        ]
    topOfStack =
      peekInteraction Nothing model.interactionsDict
  in
    div
      [ HA.classList
          [ ("splitter-separator " ++ movementClass, True)
          , ("dragging", topOfStack == Just (DraggingSplitter movement))
          , ( "draggable", model.properties.canDragSplitter && areaOpen)
          ]
      , HA.css
          [ Css.zIndex (Css.int zIdx)
          ]
      , HE.onMouseDown (StartDraggingSplitter movement)
      ]
      [ div
          [ HA.class <| "separator-handle " ++ movementClass ]
          [ button
              [ HA.class <| "collapse-button " ++ movementClass
              , HA.title "Collapse"
              , HE.onClick (ToggleAreaVisibility targetArea)
              ]
              [ collapseIcon ]
          ]
      ]

viewTestingTool : GraphPackage -> Test -> Model -> Html Msg
viewTestingTool pkg test model =
  let
    html_remaining_data =
      List.map (\ch ->
        div
          [ HA.class "character" ]
          [ text <| String.fromChar ch ]
      )
    html_transitions_taken =
      List.map (\{matching} ->
        div
          [ HA.class "transition" ]
          [ text <| acceptConditionToString matching.via ]
      )
    contextChars = 3
    html_summary_step h =
      div
        [ HA.class "execution-step-inner" ]
        [ div
            [ HA.class "summary" ]
            [ if List.isEmpty h.transitions then
                -- If there are no transitions, then the div will collapse weirdly
                -- and that will throw off the calculations for the vertical position
                -- of the list-item bullet, and of course it will, because that is
                -- the obvious thing to happen, isn't it?
                -- Wow. CSS, eh? CSS.
                -- So, this exists to counteract that 100% obvious flex-behaviour.
                text ""
              else
                div
                  [ HA.class "transitions-taken" ]
                  ((if List.length h.transitions > contextChars then
                      div
                        [ HA.class "ellipsis" ]
                        [ text "…" ]
                    else
                      text ""
                  ) ::
                  ( html_transitions_taken <| List.drop (List.length h.transitions - contextChars) h.transitions )
                  )
            , if List.isEmpty h.remainingData then
                text ""
              else
                div
                  [ HA.class "remaining-data" ]
                  ((html_remaining_data <| List.take contextChars h.remainingData) ++
                    ( if List.length h.remainingData > contextChars then
                        [ div
                            [ HA.class "ellipsis" ]
                            [ text "…" ]
                        ]
                      else
                        []
                    )
                  )
            ]
        ]
    html_expanded_step gv h =
      div
        [ HA.class "execution-step-inner" ]
        [ div
            [ HA.class "summary" ]
            [ case h.transitions of
                [] ->
                  text ""
                _ ->
                  div
                    [ HA.class "transitions-taken" ]
                    ( html_transitions_taken h.transitions )              
            , case h.remainingData of
                [] ->
                  text ""
                _ ->
                  div
                    [ HA.class "remaining-data" ]
                    (html_remaining_data h.remainingData)
            ]
        , div
            [ HA.class "step-graph" ]
            [ viewGraph gv ]
        ]
    html_execution_step props h =
      li
        [ HA.class "execution-step"
        , HE.onClick (ToggleDebugStep h.step)
        ]
        [ case IntDict.get h.step props.expandedSteps of
            Just gv_uuid ->
              AutoDict.get gv_uuid model.graph_views
              |> Maybe.map (flip html_expanded_step h)
              |> Maybe.withDefaultLazy (\() -> html_summary_step h)
            Nothing ->
              html_summary_step h
        ]
    html_execution_steps props results =
      ul
        [ HA.class "execution-steps" ]
        ( List.map (html_execution_step props) results )
  in
  div
    [ HA.class "tool-content testing" ]
    [ case peekInteraction Nothing model.interactionsDict of
        Just (Executing ((h::t) as results) props) ->
          div
            [ HA.class "step-through" ]
            ( case h.finalResult of
                Just result ->
                  -- final; show it.
                  [ div
                      [ HA.class "tools-strip" ]
                      [ button
                          [ HA.class "tool-icon reset"
                          , HA.title "Reset"
                          , HE.onClick ResetComputation
                          ]
                          [ text "🔁" ]
                      , button
                          [ HA.class "tool-icon step-back"
                          , HA.title "Step back"
                          , HE.onClick StepBack
                          ]
                          [ text "↩️" ]
                      ]
                  , case result of
                      Accepted ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result accepted" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ span
                                      [ HA.class "emphasis" ]
                                      [ text "Accepted" ]
                                  ]
                              ]
                          , html_execution_steps props results
                          ]
                      Rejected ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result rejected" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ span
                                      [ HA.class "emphasis" ]
                                      [ text "Rejected" ]
                                  ]
                              ]
                          , html_execution_steps props results
                          ]
                      NoMatchingTransitions ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result rejected" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ span
                                      [ HA.class "emphasis" ]
                                      [ text "Failed" ]
                                  , text ".  The computation did not account for "
                                  , span [ HA.class "emphasis" ] [ text "all" ]
                                  , text " of the input."
                                  ]
                              , div
                                  [ HA.class "summary-detail" ]
                                  [ text "Remaining input: "
                                  , div
                                      [ HA.class "remaining-data" ]
                                      ( html_remaining_data h.remainingData )
                                  ]
                              ]
                          , html_execution_steps props results
                          ]
                      InternalError s ->
                        div
                          [ HA.class "progress-area" ]
                          [ div
                              [ HA.class "final-result error" ]
                              [ div
                                  [ HA.class "summary-sentence" ]
                                  [ text <| "💀 Internal Error!  " ++ s ]
                              ]
                          , html_execution_steps props results
                          ]
                  ]
                Nothing ->
                  -- non-final; we're continuing.
                  [ div
                      [ HA.class "tools-strip" ]
                      [ button
                          [ HA.class "tool-icon continue-stepping"
                          , HA.title "Continue"
                          , HE.onClick Step
                          ]
                          [ text "⏯️" ]
                      , button
                          [ HA.class "tool-icon stop-stepping"
                          , HA.title "Run to end"
                          , HE.onClick Run
                          ]
                          [ text "▶️" ]
                      , button
                          [ HA.class "tool-icon stop-stepping"
                          , HA.title "Stop / Reset"
                          , HE.onClick ResetComputation
                          ]
                          [ text "⏹️" ]
                      , button
                          [ HA.class "tool-icon step-back"
                          , HA.title "Step back"
                          , HE.onClick StepBack
                          ]
                          [ text "↩️" ]
                      ]
                  , div
                      [ HA.class "progress-area" ]
                      [ html_execution_steps props results ]
                  ]
            )
        _ ->
          -- edit panel.  We are not executing.
          div
            [ HA.class "edit-input" ]
            [ div
                [ HA.class "tools-strip" ]
                [ button
                    [ HA.class "tool-icon start-stepping"
                    , HA.title "Step through"
                    , HE.onClick Step
                    ]
                    [ text "⏯️" ]
                , button
                    [ HA.class "tool-icon run"
                    , HA.title "Run"
                    , HE.onClick Run
                    ]
                    [ text "▶️" ]
                ]
            , div
                [ HA.class "edit-panel" ]
                [ div
                    [ HA.class "acceptance-condition" ]
                    [ text "When this input is received, the computation should "
                    , span
                        [ HA.classList
                            [ ("expect-accept", test.expectation == ExpectAccepted)
                            , ("expect-reject", test.expectation == ExpectRejected)
                            ]
                        , HE.onClick (PackageMsg pkg.packageIdentifier FlipAcceptanceCondition)
                        ]
                        [ text
                            ( case test.expectation of
                                ExpectAccepted -> "accept"
                                ExpectRejected -> "reject"
                            )
                        ]
                    , text " it."
                    ]
                , textarea
                    [ HA.class "input-textarea"
                    , HA.value test.input
                    , HE.onInput (UpdateTestInput >> PackageMsg pkg.packageIdentifier)
                    , HA.placeholder "Enter your test input here"
                    ]
                    []
                ]
            ]
    ]

viewMetadataTool : GraphPackage -> Html Msg
viewMetadataTool package =
  let
    len = package.computation.description |> Maybe.withDefault "" |> String.length |> toFloat
    idx = round <| -0.02779853 + 0.06685532 * len + 0.004036796 * len * len
    color = List.getAt idx colorScale.css.best_to_worst |> Maybe.withDefault colorScale.css.worst
  in
  div
    [ HA.class "tool-content metadata" ]
    [ div
        [ HA.class "edit-description" ]
        [ div
            [ HA.class "edit-panel" ]
            [ textarea
                [ HA.class "input-textarea"
                , HA.css [ Css.color color ]
                , HA.value (Maybe.withDefault "" package.computation.description)
                , HE.onInput (UpdateComputationDescription >> PackageMsg package.packageIdentifier)
                , HA.placeholder "What kind of thing does this package recognize?"
                ]
                []
            ]
        ]
    ]

viewToolsArea : Model -> Html Msg
viewToolsArea model =
  div
    [ HA.class "tools-container" ]
    [ div
        [ HA.class "tools-bar" ]
        [ button
            [ HA.classList
                [ ("tool-icon", True)
                , ("active", model.uiLayout.selected.bottomPanel == TestingToolIcon)
                ]
            , HA.title "Testing"
            , (model.uiLayout.selected.bottomPanel /= TestingToolIcon)
              |> thenPermitInteraction (HE.onClick (SelectTool TestingToolIcon))
            ]
            [ text "🔬"]
        , button
            [ HA.classList
                [ ("tool-icon", True)
                , ("active", model.uiLayout.selected.bottomPanel == MetadataToolIcon)
                ]
            , HA.title "Tests"
            , (model.uiLayout.selected.bottomPanel /= MetadataToolIcon)
              |> thenPermitInteraction (HE.onClick (SelectTool MetadataToolIcon))
            ]
            [ text "📝" ]
        ]
    , div
        [ HA.class "tools-area"
        , HA.css
            [ Css.height <| Css.px <|
                if model.uiLayout.open.bottomPanel then
                  Tuple.second model.uiLayout.dimensions.bottomPanel
                else
                  0
            ]
        ]
        [ debugDimensions model.uiLayout.dimensions.bottomPanel
        , case model.uiLayout.selected.bottomPanel of
            MetadataToolIcon ->
              AutoDict.get model.selectedPackage model.packages
              |> Maybe.map (viewMetadataTool)
              |> Maybe.withDefault
                ( div
                    [ HA.class "metadata-content no-package" ]
                    [ text "Internal error: no graph-view.  This is a bug!" ]
                )
            TestingToolIcon ->
              AutoDict.get model.selectedPackage model.packages
              |> Maybe.map (\pkg ->
                ( pkg
                , AutoDict.get pkg.currentTestKey pkg.tests
                  |> Maybe.withDefault
                    { input = ""
                    , expectation = ExpectAccepted
                    , result = Rejected
                    }
                )
              )
              |> Maybe.map (\(pkg, test) ->
                viewTestingTool pkg test model
              )
              |> Maybe.withDefault
                ( div
                    [ HA.class "tool-content no-tests" ]
                    [ text "There are no tests for this computation." ]
                )
        ]
    ]

viewMainInterface : Model -> Html Msg
viewMainInterface model =
  div
    [ HA.class "editor-frame" ]
    [ viewNavigatorsArea model
    , if model.uiLayout.open.sideBar then
        viewSplitter
          5 LeftRight
          model
          (model.uiLayout.open.sideBar)
      else
        div [] []
    , div
        [ HA.class "editor-and-tools-panel" ]
        [ div
            [ HA.class "editor-main"
            , HA.css
                [ Css.maxWidth <| px <| Tuple.first model.uiLayout.dimensions.mainEditor
                , Css.maxHeight <| px <| Tuple.second model.uiLayout.dimensions.mainEditor
                ]
            ]
            [ debugDimensions model.uiLayout.dimensions.mainEditor
            , AutoDict.get model.mainGraphView model.graph_views
              |> Maybe.map (GraphEditor.viewGraph)
              |> Maybe.withDefault
                (div [ HA.class "error graph-not-loaded" ] [ text "⚠ Graph to load was not found!" ]) -- erk! say what now?!
            ]
        , viewSplitter
            4 UpDown
            model
            model.uiLayout.open.bottomPanel
        , viewToolsArea model
        ]
    -- we have these down here so that they WILL sit on top of the panning-region bars for the
    -- main area.
    , if not model.uiLayout.open.sideBar then
        viewCollapsedAreaButton LeftRight
      else
        text ""
    , if not model.uiLayout.open.bottomPanel then
        viewCollapsedAreaButton UpDown
      else
        text ""
    ]

htmlForTransition : Model -> Transition -> Maybe (Html Msg)
htmlForTransition {packages} {via, isFinal} =
  let
    classes =
      [ ("set-item", True)
      , ("final", isFinal)
      , ("non-final", not isFinal)
      ]
  in
    case via of
      ViaCharacter c ->
        Just
          ( span
              [ HA.classList ( ("character", True) :: classes )
              ]
              [ text <| String.fromChar c ]
          )
      ViaGraphReference pkg_uuid ->
        AutoDict.get pkg_uuid packages
        |> Maybe.map (\pkg ->
          span
            [ HA.classList ( ("graph-reference", True) :: classes ) ]
            [ div
                [ HA.class "package-badge"
                , HA.title "This unique badge identifies this specific computation."
                ]
                [ TypedSvg.svg
                    [ TypedSvg.Attributes.viewBox 0 0 30 18 ]
                    [ GraphEditor.viewGraphReference pkg_uuid 4 0 0 ]
                  |> Html.Styled.fromUnstyled
                ]
            , case pkg.computation.description of
                Just s ->
                  div
                    [ HA.class "description" ]
                    [ text s ]
                Nothing ->
                  text ""
            ]
            -- [ text <| Maybe.withDefault "(no description)" pkg.description ]
        )


viewConnectionEditor : Model -> Connection -> ConnectionEditorProperties -> Html Msg
viewConnectionEditor model connection editorData =
  let
    (terminal, nonTerminal) =
      AutoSet.partition (.isFinal) connection
  in
  div
    [ HA.class "modal" ]
    [ div
        [ HA.class "connection-editor" ]
        [ div
            [ HA.class "connection-editor-top" ]
            [ div
                [ HA.class "top-left" ]
                [ div
                    [ HA.class "set-builder" ]
                    [ div -- set visualization area
                        [ HA.class "set-visualization" ]
                        [ div
                            [ HA.class "set-bracket" ]
                            [ text "{"
                            ]
                        , div -- terminal items group
                            [ HA.class "terminals" ]
                            ( AutoSet.toList terminal
                              |> List.filterMap (htmlForTransition model)
                            )
                        , div
                            [ HA.class "set-separator" ]
                            []
                        , div -- normal (non-terminal) items group
                            [ HA.class "non-terminals" ]
                            ( AutoSet.toList nonTerminal
                              |> List.filterMap (htmlForTransition model)
                            )
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "}"
                            ]
                        ]
                    ]
                , div -- quick input
                    [ HA.class "quick-input" ]
                    [ div
                        [ HA.class "quick-input-bar"
                        , HE.onInput (QuickInput)
                        ]
                        [ input
                            [ HA.class "input-field"
                            , HA.placeholder "Type here…"
                            , HA.id "quick-input"
                            , HA.autocomplete False
                            , HA.attribute "autocorrect" "off"
                            , HA.attribute "spellcheck" "off"
                            , HA.value <|
                                case editorData.editingMode of
                                  CharacterInput -> ""
                                  GraphReferenceSearch s -> s
                            ]
                            []
                        -- , button
                        --     [ HA.class "action-button" ]
                        --     [ span [] [ text "🎨 Images" ] ]
                        -- , button
                        --     [ HA.class "action-button secondary" ]
                        --     [ span [] [ text "🔍 Browse All" ] ]
                        ]
                    , div
                        [ HA.class "instructions" ]
                        ( case editorData.editingMode of
                            CharacterInput ->
                              [ div
                                  []
                                  [ span [] [ text "Typing a character once adds it; typing it again promotes it to " ]
                                  , span [ HA.class "terminal-style" ] [ text "terminal" ]
                                  , span [] [ text " status; typing it a third time removes it.  Use the mouse to scroll and select computations." ]
                                  ]
                              , div
                                  []
                                  [ span [] [ text "Typing «`» enters computation-search mode." ]
                                  ]
                              ]
                            GraphReferenceSearch _ ->
                              [ div
                                  []
                                  [ span [] [ text "Only computations with a matching description will be displayed.  Use the mouse to scroll and select computations.  Type «`» to switch back to character mode." ]
                                  ]
                              ]
                        )
                    ]
                ]
            , div
                [ HA.class "top-right" ]
                [ AutoDict.get editorData.mainGraph model.graph_views
                  |> Maybe.map viewGraph
                  |> Maybe.withDefault (text "")
                ]
            ]
        , div -- image palette
            [ HA.class "image-palette" ]
            [ div
                [ HA.class "palette-grid" ]
                -- good width for a 4x? grid: 450-455px
                ( List.filterMap
                    (\view_uuid ->
                        AutoDict.get view_uuid model.graph_views
                        |> Maybe.andThen
                          (\gv ->
                            Maybe.combineSecond
                              ( gv
                              , Maybe.andThen
                                  (\pkg_uuid -> AutoDict.get pkg_uuid model.packages)
                                  gv.graphPackage
                              )
                          )
                        |> Maybe.map
                          (\(graph_view, pkg) ->
                            let
                              via = ViaGraphReference pkg.packageIdentifier
                              inTerminals = AutoSet.member (Transition True via) terminal
                              inNonTerminals = AutoSet.member (Transition False via) nonTerminal
                            in
                              -- case graph_view. graph_view.package.description of
                              --   Nothing -> -- then display anyway.
                              --   Just k ->
                              div
                                [ HA.classList
                                    [ ("palette-item", True)
                                    , ("terminal", inTerminals)
                                    , ("non-terminal", inNonTerminals)
                                    ]
                                , HE.onClick (ToggleConnectionTransition via)
                                ]
                                [ viewGraph graph_view
                                , div
                                    [ HA.class "description" ]
                                    [ graph_view.computation.description
                                      |> Maybe.withDefault "(no description)"
                                      |> text
                                    ]
                                , if inTerminals || inNonTerminals then
                                    div
                                      [ HA.class "package-badge"
                                      , HA.title "This unique badge identifies this specific computation."
                                      ]
                                      [ TypedSvg.svg
                                          [ TypedSvg.Attributes.viewBox 0 0 30 18 ]
                                          [ GraphEditor.viewGraphReference pkg.packageIdentifier 4 0 0 ]
                                        |> Html.Styled.fromUnstyled
                                      ]
                                  else
                                    text ""
                                ]
                        )
                    )
                    editorData.shownList
                )
            , div
                []
                []
            ]
        ]
    ]

viewNodeSplitInterface : Model -> NodeSplitData -> SplitNodeInterfaceProperties -> Html Msg
viewNodeSplitInterface model {left, right} interfaceData =
  div
    [ HA.class "modal" ]
    [ div
        [ HA.class "node-split-interface" ]
        [ div
            [ HA.class "node-split-top" ]
            [ div
                [ HA.class "top-left" ]
                [ div
                    [ HA.class "set-builder" ]
                    [ div -- set visualization area
                        [ HA.class "set-visualization" ]
                        [ div
                            [ HA.class "set-bracket" ]
                            [ text "{"
                            ]
                        , div -- terminal items group
                            [ HA.class "left" ]
                            ( AutoSet.toList left
                              |> List.filterMap (\t -> Maybe.combineSecond (t, htmlForTransition model t))
                              |> List.map (\({via}, e) -> div [ HE.onClick (ToggleConnectionTransition via) ] [ e ])
                            )
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "}"
                            ]
                        , div
                            [ HA.class "set-separator" ]
                            []
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "{"
                            ]
                        , div -- normal (non-terminal) items group
                            [ HA.class "right" ]
                            ( AutoSet.toList right
                              |> List.filterMap (\t -> Maybe.combineSecond (t, htmlForTransition model t))
                              |> List.map (\({via}, e) -> div [ HE.onClick (ToggleConnectionTransition via) ] [ e ])
                            )
                        , div
                            [ HA.class "set-bracket" ]
                            [ text "}"
                            ]
                        ]
                    ]
                , div -- quick input
                    [ HA.class "quick-input" ]
                    [ div
                        [ HA.class "quick-input-bar"
                        , HE.onInput (QuickInput)
                        ]
                        [ input
                            [ HA.class "input-field"
                            , HA.placeholder "Type here…"
                            , HA.id "quick-input"
                            , HA.autocomplete False
                            , HA.attribute "autocorrect" "off"
                            , HA.attribute "spellcheck" "off"
                            , HA.value ""
                            ]
                            []
                        ]
                    , div
                        [ HA.class "instructions" ]
                            [ div
                                []
                                [ span [] [ text "Typing a character will switch it from right to left, or vice-versa.  You can also click on items above, including computations, to switch their sides." ]
                                ]
                            ]
                    ]
                ]
            , div
                [ HA.class "top-right" ]
                [ AutoDict.get interfaceData.mainGraph model.graph_views
                  |> Maybe.map viewGraph
                  |> Maybe.withDefault (text "")
                ]
            ]
        ]
    ]

viewPackageDeletionWarning : DeletingPackageProperties -> Model -> Html Msg
viewPackageDeletionWarning props model =
  let
    viewGraphItem uuid =
      AutoDict.get uuid model.graph_views
      |> Maybe.map
        (\graph_view ->
          div
            [ HA.class "graph-item" ]
            [ viewGraph graph_view
            , div
                [ HA.class "description" ]
                [ graph_view.computation.description
                  |> Maybe.withDefault "(no description)"
                  |> text
                ]
            ]
        )
  in
  div
    [ HA.class "modal" ]
    [ div
        [ HA.class "deletion-warning" ]
        [ div
            [ HA.class "title" ]
            [ text "Are you sure you want to delete this?"
            ]
        , div
            [ HA.class "graph-to-delete" ]
            [ viewGraphItem props.mainGraph
              |> Maybe.withDefault (text "")
            ]
        , div
            [ HA.class "details" ]
            [ div
              [ HA.class "panel left" ]
              [ div
                  [ HA.class "info" ]
                  [ text "Deletion will directly affect" ]
              , div
                  [ HA.class "affected" ]
                  ( List.filterMap viewGraphItem props.directViews )
              ]
            , div
              [ HA.class "panel right" ]
              [ div
                  [ HA.class "info" ]
                  [ text "Deletion will indirectly affect" ]
              , div
                  [ HA.class "affected" ]
                  ( List.filterMap viewGraphItem props.indirectViews )
              ]
            ]
        , div
            [ HA.class "buttons" ]
            [ button
                [ HE.onClick Confirm
                , HA.class "button danger"
                ]
                [ text "Delete this graph"
                ]
            , button
                [ HE.onClick Escape
                , HA.class "button"
                ]
                [ text "Keep this graph"
                ]
            ]
        ]
    ]

view : Model -> Html Msg
view model =
  div
    []
    [ case mostRecentInteraction model of
        Just (Just _, EditingConnection {connection} props) ->
          viewConnectionEditor model connection props
        Just (Just _, SplittingNode data props) ->
          viewNodeSplitInterface model data props
        Just (Nothing, DeletingPackage _ props) ->
          viewPackageDeletionWarning props model
        _ ->
          viewMainInterface model
    , div
        [ HA.class "debug gv-size" ]
        [ text <| String.fromInt <| AutoDict.size model.graph_views ]
    , div
        [ HA.class "debug interactionsdict" ]
        [ htmlInteractionsDictSummary model.interactionsDict ]
    ]

htmlInteractionsDictSummary : AutoDict.Dict String (Maybe Uuid) (Int, List InteractionState) -> Html a
htmlInteractionsDictSummary dict =
  AutoDict.toList dict
  |> List.sortBy (Tuple.second >> Tuple.first)
  |> List.map (\(key, (_, stack)) ->
    let
      k =
        case key of
          Nothing -> "⯁"
          Just uuid -> truncate_uuid uuid
    in
        k ++ " → " ++ String.fromInt (List.length stack)
  )
  |>
    (\strings ->
      Html.Styled.ol
        []
        ( List.map (\s -> Html.Styled.li [] [ text s ]) strings )
    )