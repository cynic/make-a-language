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
import Basics.Extra as Basics
import Automata.Debugging
import IntDict
import Set
import Graph exposing (NodeId)
import Force
import Automata.Debugging exposing (debugAutomatonGraph)
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes
import Automata.Debugging exposing (println)
import UserInterface exposing (..)
import GraphEditor
import Dict exposing (Dict)
import Jsonify exposing (..)
import Graph exposing (Graph)
import Css exposing (px)
import Basics.Extra exposing (maxSafeInteger, minSafeInteger)
import GraphEditor exposing (path_between)
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

{-
Quality / technology requirements:

1. Use CSS3, HTML5, and classes.  The file `style.css` can be modified with the
   correct styles.
2. Html.Styled should be used to maintain type safety, unless that is impossible
   or impractical.  If impossible or impractical, a comment should be left to
   explain why that is the case.
-}

type alias Msg = Main_Msg
type alias Model = Main_Model

canExecute : Model -> Uuid -> Bool
canExecute { graph_views } uuid =
  AutoDict.get uuid graph_views
  |> Maybe.map (.package >> .undoBuffer >> List.isEmpty)
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

nodeDrawingForPackage : GraphPackage -> Bool -> Uuid -> Model -> Dict NodeId NodeDrawingData
nodeDrawingForPackage package isFrozen graphView_uuid model =
  let
    disconnectedNodes =
      GraphEditor.identifyDisconnectedNodes package.userGraph
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
    Graph.nodes package.userGraph.graph
    |> List.map
      (\node ->
        let
          nodeContext =
            Graph.get node.id package.userGraph.graph
        in
          ( node.id
          , { exclusiveAttributes =
                case peekInteraction (Just graphView_uuid) model of
                  Just (DraggingNode node_id) ->
                    maybe_fromBool
                      (node.id == node_id)
                      DrawSelected
                  Just (Executing result) ->
                    maybe_fromBool
                      ( GraphEditor.executionData result
                        |> Maybe.map (.currentNode >> (==) node.id)
                        |> Maybe.withDefault False
                      )
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
            , isRoot = node.id == package.userGraph.root
            , canSplit =
                if isFrozen then
                  False
                else
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
identifyCardinality from to {package} =
  Graph.get to package.userGraph.graph
  |> Maybe.map
    (\toContext -> identifyCardinalityViaContext from toContext)
  |> Maybe.withDefault Unidirectional

linkDrawingForPackage : GraphPackage -> Dict (NodeId, NodeId) LinkDrawingData
linkDrawingForPackage package =
  Graph.edges package.userGraph.graph
  |> List.filterMap
      (\edge ->
        Maybe.map2
          (\f t -> { sourceNode = f, destNode = t, label = edge.label })
          (Graph.get edge.from package.userGraph.graph)
          (Graph.get edge.to package.userGraph.graph)
      )
  |> List.map
      (\{sourceNode, destNode, label} ->
        let
          cardinality : Cardinality
          cardinality =
            identifyCardinalityViaContext sourceNode.node.id destNode
        in
          ( (sourceNode.node.id, destNode.node.id)
          , { cardinality = cardinality
            , executionData = Nothing
            , label = label
            , pathBetween =
                GraphEditor.path_between
                  sourceNode.node.label
                  destNode.node.label
                  cardinality
            , highlighting = Nothing
            }
          )
    )
  |> Dict.fromList

{-| Probably not the function you want. Look at `solvedViewFromPackage` and
    `naiveViewFromPackage` instead.
-}
viewFromPackage : GraphEditor.ComputeGraphResult -> (Float, Float) -> InterfaceLocation -> Bool -> GraphPackage -> Model -> (GraphView, Model)
viewFromPackage computed (w, h) location createFrozen pkg model =
  let
    (id, model_) = getUuid model
    -- now that we have the positions, calculate the dimensions of
    -- the viewport.
    -- first, let's get the aspect ratio.  That will let us figure out
    -- the height of the viewport "automatically" once we have the right width.
    solved_pkg =
      { pkg
        | userGraph =
          computed.solvedGraph
          |> debugAutomatonGraphXY ("[viewFromPkg " ++ truncate_uuid id ++ "] solved_pkg")
      }
    guest_viewport =
      calculateGuestDimensionsForHost
        (w, h)
        createFrozen
        solved_pkg.userGraph.graph
    properties = defaultViewProperties createFrozen solved_pkg
    graph_view : GraphView
    graph_view =
      { id = id
      , isFrozen = createFrozen
      , package = solved_pkg
      , interfaceLocation = location
      , simulation = computed.simulation
      , host_dimensions = (w, h)
      , host_coordinates = (-1, -1)
      , panBuffer = GraphEditor.panBufferAmount (w, h)
      , guest_dimensions = guest_viewport.dimensions
      , guest_coordinates = guest_viewport.coordinates
      , guest_inner_coordinates = guest_viewport.inner_coordinates
      , guest_inner_dimensions = guest_viewport.inner_dimensions
      , forces = computed.forces
      , specificForces = IntDict.empty
      , zoom = 1.0
      , pan = ( 0, 0)
      , disconnectedNodes = Set.empty
      , properties = properties
      , drawingData =
          { link_drawing = linkDrawingForPackage solved_pkg
          , node_drawing = nodeDrawingForPackage solved_pkg createFrozen id model
          }
      }
  in
    ( graph_view
    , { model_
        | graph_views =
            AutoDict.insert id graph_view model_.graph_views
        , mainGraphView =
            if location == MainEditor then
              id
            else
              model.mainGraphView
      }
    )

{-| Creates a new GraphView from a provided GraphPackage, accepting the provided layout
    uncritically, and adds the GraphView to the `graph_views` dictionary in the `Model`.
-}
naiveViewFromPackage : (Float, Float) -> InterfaceLocation -> Bool -> GraphPackage -> Model -> (GraphView, Model)
naiveViewFromPackage (w, h) location createFrozen pkg model =
  viewFromPackage
    { solvedGraph = pkg.userGraph
    , simulation = Force.simulation []
    , forces = []
    }
    (w, h) location createFrozen pkg model

{-| Creates a new GraphView from a provided GraphPackage, solves the forces for the relevant
    AutomatonGraph, and adds the GraphView to the `graph_views` dictionary in the `Model`.
-}
solvedViewFromPackage : (Float, Float) -> InterfaceLocation -> Bool -> GraphPackage -> Model -> (GraphView, Model)
solvedViewFromPackage (w, h) location createFrozen pkg model =
  viewFromPackage
    (GraphEditor.computeGraphFully (w, h) pkg.userGraph)
    (w, h) location createFrozen pkg model

centerAndHighlight : NodeId -> NodeId -> GraphView -> GraphView
centerAndHighlight src dest graph_view =
  Maybe.map2
    (\srcContext destContext ->
      let
        bbox a b =
          { min = { x = min a.x b.x, y = min a.y b.y }
          , max = { x = max a.x b.x, y = max a.y b.y }
          }
        bounds =
          bbox srcContext.node.label destContext.node.label
        inner_padding = 60
        adjusted =
          { bounds
            | min = { x = bounds.min.x - inner_padding, y = bounds.min.y - inner_padding }
            , max = { x = bounds.max.x + inner_padding, y = bounds.max.y + inner_padding }
          }
      in
        { graph_view
          | drawingData =
              let drawingData = graph_view.drawingData in
                { drawingData
                  | link_drawing =
                      Dict.update (src, dest)
                        (Maybe.map (\link ->
                          { link | highlighting = Just Highlight }
                        ))
                        drawingData.link_drawing
                }
          , guest_coordinates =
              ( adjusted.min.x, adjusted.min.y )
          , guest_dimensions =
              ( adjusted.max.x - adjusted.min.x, adjusted.max.y - adjusted.min.y )
          , guest_inner_coordinates =
              ( bounds.min.x, bounds.min.y )
          , guest_inner_dimensions =
              ( bounds.max.x - bounds.min.x, bounds.max.y - bounds.min.y )
        }
    )
    (Graph.get src graph_view.package.userGraph.graph)
    (Graph.get dest graph_view.package.userGraph.graph)
  |> Maybe.withDefault graph_view

-- MAIN

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }

createNewPackage : Uuid.Uuid -> Time.Posix -> AutomatonGraph -> GraphPackage
createNewPackage testUuid currentTime g = -- `dimensions` is the width & height of the panel
  { userGraph = g
  -- , dimensions = dimensions
  , description = Nothing
  , created = currentTime
  , currentTestKey = testUuid
  , tests = AutoDict.empty Uuid.toString
  , undoBuffer = []
  , redoBuffer = []
  }

init : E.Value -> (Model, Cmd Msg)
init flags =
  let
    decoded =
      D.decodeValue decodeFlags flags
      |> Result.mapError (\err -> Debug.log "OH NO! FLAGS WAS NOT PARSED CORRECTLY!!" err)
      |> Result.withDefault (Flags 80 60 0 [] (Time.millisToPosix 0) [])
    
    (packages, mainPackage, initialSeed) =
      let
        seed0 = Random.initialSeed decoded.initialSeed decoded.extendedSeeds
        allPackagesList =
          decoded.packages
          -- |> List.map (\v -> Automata.Debugging.debugAutomatonGraph "Loaded" v.model.userGraph |> \_ -> v)
          |> List.map (\v -> ( v.userGraph.graphIdentifier, v ))
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
                  decoded.startTime
                  ( { graph =
                        Graph.fromNodesAndEdges
                          [ Graph.Node 0 (entity 0 NoEffect |> (\e -> { e | x = 0, y = 0 }))
                          ]
                          []
                    , graphIdentifier = mainUuid
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
    state : UIState
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
      , mainGraphView = mainPackage.userGraph.graphIdentifier -- this is—temporarily—the wrong value!
      , packages = packages
      , uiState = state
      , uiConstants = constants
      , randomSeed = initialSeed
      -- , mouseCoords = (0, 0)
      , interactionsDict = AutoDict.empty (Maybe.map Uuid.toString >> Maybe.withDefault "")
      , properties =
          defaultMainProperties
      , computationsExplorer = []
      }
    model =
      -- I _know_ that this will succeed, because I've added this
      -- exact one
      let
        ( v, model_with_viewDict) =
          solvedViewFromPackage
            state.dimensions.mainEditor
            MainEditor
            False
            mainPackage
            model_excl_views
      in
        { model_with_viewDict
          | mainGraphView =
              v.id
              -- |> debugLog_ "mainGraphView UUID" truncate_uuid
        }
  in
    ( selectComputationsIcon model
    , Cmd.none
    )

-- UPDATE

{-| Note: EVERYWHERE that I use persistPackage, I should ALSO
    update the `packages` dictionary!

    (yes, I should one day figure out an automagic way to do this…
     maybe. but it's in so few places right now that hey, YAGNI?)
-}
persistPackage : GraphPackage -> Cmd Msg
persistPackage =
  Ports.saveToStorage << encodeGraphPackage

-- updateGraphInView : AutomatonGraph -> GraphView -> GraphView
-- updateGraphInView g v =
--   let
--     (w, h)  = v.dimensions
--     forceGraph = toForceGraph (g {- |> Debug.log "Received by ForceDirectedGraph" -} )
--     forces = viewportForce (w, h) :: basicForces forceGraph (round h)
--     pkg = v.package
--   in
--     { v -- make sure we are referencing the correct Model!
--       | simulation = Force.simulation forces
--       , package = { pkg | userGraph = forceGraph }
--       , forces = forces
--       , specificForces = IntDict.empty
--     }

clearUndoBuffers : GraphPackage -> GraphPackage
clearUndoBuffers pkg =
  { pkg
    | undoBuffer = []
    , redoBuffer = []
  }

updateTestResultsFor : ResolutionDict -> GraphPackage -> GraphPackage
updateTestResultsFor resolutionDict pkg =
  { pkg
    | tests =
        AutoDict.map
          (\_ entry ->
            { entry
              | result =
                  DFA.load entry.input resolutionDict pkg.userGraph
                  |> DFA.run
            }
          )
          pkg.tests
  }

-- updateViewAfterConfirmation : AutomatonGraph -> ResolutionDict -> GraphView -> GraphView
-- updateViewAfterConfirmation g resolutionDict v =
--   updateGraphInView g v
--   |>  ( \v_ ->
--         { v_
--           | package =
--               v.package
--               |> updateTestResultsFor resolutionDict
--               |> clearUndoBuffers
--         }
--       )

-- applyChangesToGraph : AutomatonGraph -> AutomatonGraph
-- applyChangesToGraph g =
--   debugAutomatonGraph "Initial from user" g |> \_ ->
--   { g
--     | graph =
--         -- first, actually remove all disconnected nodes.
--         identifyDisconnectedNodes g
--         |> Set.foldl Graph.remove g.graph
--   }
--   |> debugAutomatonGraph "After removing disconnected"
--   |> (DFA.fromAutomatonGraph >> DFA.toAutomatonGraph g.graphIdentifier)

-- confirmChanges : AutomatonGraph -> ResolutionDict -> GraphView -> GraphView
-- confirmChanges g resolutionDict graphView =
--   updateViewAfterConfirmation
--     (applyChangesToGraph g)
--     resolutionDict
--     graphView

-- resizeGraphView : (Float, Float) -> GraphView -> GraphView
-- resizeGraphView dim v =
--   let
--     -- the center of the viewport may change.
--     forces = GraphEditor.forces v.package.userGraph (Tuple.mapBoth round round dim)
--   in
--   { v
--     | dimensions = dim
--     , forces = forces
--     , simulation =
--         Force.simulation
--           ( forces ++
--             List.concat (IntDict.values v.specificForces)
--           )
--   }

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

    UpdateCurrentPackage updated ->
      if updated.userGraph.graphIdentifier /= model.package.userGraph.graphIdentifier then
        -- I don't care; ignore this!
        model
      else
        { model
          | currentPackage = updated
          , packages =
              AutoDict.insert
                updated.userGraph.graphIdentifier
                updated
                model.packages
        }
    
    CreateNewNodeAt ( x, y ) ->
      case model.interactionsDict of
        Just (ModifyingGraph _ { source, transitions }) ->
          { model
            | interactionsDict =
                Just <| ModifyingGraph ChooseCharacter <| GraphModification source (NewNode ( x, y )) transitions
          }
        _ ->
          model

    Escape ->
      let
        escapery =
          case model.interactionsDict of
            Just (ModifyingGraph via { source, dest, transitions }) ->
              case dest of
                NoDestination ->
                  -- I must be escaping from something earlier.
                  { model | interactionsDict = Nothing }
                _ ->
                  { model
                    | interactionsDict =
                        Just <| ModifyingGraph via <| GraphModification source NoDestination transitions
                  }
            Just (Splitting _) ->
              { model | interactionsDict = Nothing }
            Just (AlteringConnection _ _) ->
              { model | interactionsDict = Nothing }
            Nothing ->
              model
            Just (Dragging _) ->
              -- stop dragging.
              { model | interactionsDict = Nothing }
            Just (Executing _ _) ->
              model -- nothing to escape.
              -- let
              --   pkg = model.package
              -- in
              --   { model
              --     | interactionsDict = Nothing
              --     , currentPackage =
              --         { pkg
              --           | userGraph = original
              --         }
              --   }
      in
      -- ooh!  What are we "escaping" from, though?
        escapery

    Confirm ->
      -- What am I confirming?
      let
        commit_change : AutomatonGraph -> Model -> Model
        commit_change updatedGraph model_ =
          let
            basic =
              basicForces updatedGraph (round <| Tuple.second model_.dimensions)
            viewport =
              viewportForces model_.dimensions updatedGraph.graph
            pkg = model_.package
          in
            { model_
            | interactionsDict = Nothing
            , currentPackage =
                { pkg
                  | userGraph = updatedGraph
                  , undoBuffer = model.package.userGraph :: model_.package.undoBuffer
                  , redoBuffer = [] -- when we make a new change, the redo-buffer disappears; we're not storing a tree!
                }
                    -- NOTE ⬇ WELL! This isn't a typo!
            , basicForces = basic
            , viewportForces = viewport
            , simulation = Force.simulation (basic ++ viewport)
            , disconnectedNodes =
                identifyDisconnectedNodes updatedGraph
            }
        
        createNewNode : NodeId -> Connection -> Float -> Float -> Model
        createNewNode src conn x y =
          newnode_graphchange src x y conn model.package.userGraph
          |> \newGraph -> commit_change newGraph model

        updateExistingNode src dest conn =
          updateLink_graphchange src dest conn model.package.userGraph
          |> \newGraph -> commit_change newGraph model

        removeLink : NodeId -> NodeId -> Model
        removeLink src dest =
          removeLink_graphchange src dest model.package.userGraph
          |> \newGraph -> commit_change newGraph model

      in
        case model.interactionsDict of
          Just (ModifyingGraph _ { source, dest, transitions }) ->
            case dest of
              ( NewNode ( x, y ) ) ->
                -- create a totally new node, never before seen!
                createNewNode source transitions x y
              ( ExistingNode destination ) ->
                if AutoSet.isEmpty transitions then
                  removeLink source destination
                else
                  updateExistingNode source destination transitions
              ( NoDestination ) ->
                -- ??? Nothing for me to do!  The user is just pressing Enter because… uh… eh, who knows?
                model
          Just (AlteringConnection _ { source, dest, transitions }) ->
                if AutoSet.isEmpty transitions then
                  removeLink source dest
                else
                  updateExistingNode source dest transitions
          Just (Splitting { to_split, left, right }) ->
            if AutoSet.isEmpty left || AutoSet.isEmpty right then
              { model | interactionsDict = Nothing }
            else
              Graph.get to_split model.package.userGraph.graph
              |> Maybe.map (\node ->
                splitNode node left right model
                |> \g -> commit_change g model
              )
              |> Maybe.withDefault model
          Nothing -> -- I'm not in an active operation. But do I have changes to confirm?
            case model.package.undoBuffer of
              [] -> -- no changes are proposed, so…
                model -- …there is nothing for me to do!
              _ ->
                confirmChanges model
          Just (Dragging _) ->
            model -- confirmation does nothing for this (visual) operation.
          Just (Executing _ _) ->
            model -- confirmation does nothing for execution

    ToggleSelectedTransition acceptCondition ->
      let
        alterTransitions : Connection -> Connection
        alterTransitions conn =
          AutoSet.foldl
            (\t (seen, state) ->
              if t.via == acceptCondition then
                -- I've found the right transition.  Now, update or remove or…?
                if t.isFinal then
                  ( True
                  , state -- skip it; i.e., remove it.
                  )
                else
                  ( True
                  , AutoSet.insert
                      ( { t
                        | isFinal = True -- make it final.
                        }
                      )
                      state
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
      in
        case model.interactionsDict of
          Just (ModifyingGraph via ({ transitions } as mod)) ->
            { model
              | interactionsDict =
                  Just <| ModifyingGraph via <|
                    { mod
                      | transitions = alterTransitions transitions
                    }
            }
          Just (AlteringConnection via ({ transitions } as mod)) ->
            { model
              | interactionsDict =
                  Just <| AlteringConnection
                    via
                    { mod
                      | transitions = alterTransitions transitions
                    }
            }
          Just (Splitting { to_split, left, right }) ->
            let
              tr finality =
                Transition
                  finality
                  acceptCondition
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
              { model
                | interactionsDict =
                    Just <| Splitting <| Split to_split newLeft newRight
              }
          _ ->
            model

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

    SwitchVia newChosen ->
      case model.interactionsDict of
        Just (ModifyingGraph _ d) ->
          { model | interactionsDict = Just <| ModifyingGraph newChosen d }
        Just (AlteringConnection _ d) ->
          { model | interactionsDict = Just <| AlteringConnection newChosen d }
        _ -> model

    SwitchToNextComputation ->
      case model.interactionsDict of
        Just (ModifyingGraph (ChooseGraphReference idx) d) ->
          { model | interactionsDict = Just <| ModifyingGraph (ChooseGraphReference <| min (AutoDict.size model.packages - 1) (idx + 1)) d }
        Just (AlteringConnection (ChooseGraphReference idx) d) ->
          { model | interactionsDict = Just <| AlteringConnection (ChooseGraphReference <| min (AutoDict.size model.packages - 1) (idx + 1)) d }
        _ ->
          model

    SwitchToPreviousComputation ->
      case model.interactionsDict of
        Just (ModifyingGraph (ChooseGraphReference idx) d) ->
          { model | interactionsDict = Just <| ModifyingGraph (ChooseGraphReference <| max 0 (idx - 1)) d }
        Just (AlteringConnection (ChooseGraphReference idx) d) ->
          { model | interactionsDict = Just <| AlteringConnection (ChooseGraphReference <| max 0 (idx - 1)) d }
        _ ->
          model

    StartSplit nodeId ->
      Graph.get nodeId model.package.userGraph.graph
      |> Maybe.map
        (\node ->
          { model
            | interactionsDict =
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
                    | userGraph =
                        executionResultAutomatonGraph executionResult
                        |> Maybe.withDefault pkg.userGraph
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
                    | userGraph =
                        executionResultAutomatonGraph executionResult
                        |> Maybe.withDefault pkg.userGraph
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
                    | userGraph = original
                  }
            }
          _ ->
            model -- 'stop' isn't valid in any other context.
-}

{-| When either the sidebar or the bottom-panel have changed dimensions, this should be
    called to figure out what changes need to be made to any of the other dimensions.
-}
recalculate_uistate : UIState -> UIState
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

{-| Accepts host dimensions, view properties, and a graph, and calculates the
    appropriate guest coordinates & guest dimensions (i.e. viewport).
-}
calculateGuestDimensionsForHost : (Float, Float) -> Bool -> Graph.Graph Entity Connection -> GuestDimensions
calculateGuestDimensionsForHost (w, h) isFrozen graph =
  let
    aspectRatio : Float
    aspectRatio =
      w / h
      -- |> Debug.log "Viewport aspect-ratio"
    theoretical_max = 150.0 * toFloat (Graph.size graph)
    -- Now find out: where is the bounding box for the nodes?
    ( (min_x_raw, max_x_raw), (min_y_raw, max_y_raw) ) =    
      Graph.fold
        (\ctx ((xmin, xmax), (ymin, ymax)) ->
          ( ( min ctx.node.label.x xmin
            , max ctx.node.label.x xmax
            )
          , ( min ctx.node.label.y ymin
            , max ctx.node.label.y ymax
            )
          )
        )
        ((theoretical_max, -1000), (theoretical_max, -1000))
        graph
      -- |> Debug.log "Raw Bounds"
    inner_pad : Float -- 85-105 in SVG-coordinates seems to be a "good" amount of space
    inner_pad =
      if isFrozen then
        -- we don't need any buffer.
        -- So, just put in a "buffer" for the node radius.
        if max_x_raw - min_x_raw < 60 then
          60 -- this is the minimum amount. It is enough to show a single node.
        else
          10
      else
        -- when we edit (e.g. make new nodes etc), we want some free space around to put
        -- those nodes.  That is what this is for.
        95
    -- the forces on the graph place the root at (0, 0).
    -- they also pull all the other nodes to the right and to the
    -- y-axis center.
    -- So I can take the number of nodes and multiply by, say, 150 and
    -- I shouldn't be too far off from a maximum.
    ( (min_x, max_x), (min_y, max_y) ) =
      ( (min_x_raw - inner_pad, max_x_raw + inner_pad)
      , (min_y_raw - inner_pad, max_y_raw + inner_pad))
      -- |> Debug.log "After-padding Bounds"
    -- from this, I can figure out the appropriate coordinates
    center_y =
      (min_y + max_y) / 2
      -- |> Debug.log "center-y"
    autoHeight =
      (max_x - min_x) / aspectRatio
      -- |> Debug.log "Auto-height (via aspect-ratio)"
    -- now, we want a center within that autoHeight.
    guestCoordinates =
      ( min_x, center_y - autoHeight / 2 )
      -- |> Debug.log "Top-left (X,Y) of SVG viewport"
    guestDimensions =
      ( max_x - min_x, autoHeight )
      -- |> Debug.log "(Width, Height) of SVG viewport" 
    pad_inner_x =
      0.15 * (max_x_raw - min_x_raw)
      -- |> Debug.log "Inner-rectangle X padding (for panning)"
    pad_inner_y =
      0.15 * (max_y_raw - min_y_raw)
      -- |> Debug.log "Inner-rectangle Y padding (for panning)"
    x_inner =
      min_x_raw + pad_inner_x
      -- |> Debug.log "Inner-rectangle X (for panning)"
    y_inner =
      min_y_raw + pad_inner_y
      -- |> Debug.log "Inner-rectangle Y (for panning)"
    width_inner =
      (max_x_raw - pad_inner_x * 2) - min_x_raw
      -- |> Debug.log "Inner-rectangle width (for panning)"
    height_inner =
      (max_y_raw - pad_inner_y * 2) - min_y_raw
      -- |> Debug.log "Inner-rectangle height (for panning)"
  in
    { dimensions = guestDimensions
    , coordinates = guestCoordinates
    , inner_coordinates = ( x_inner, y_inner )
    , inner_dimensions = ( width_inner, height_inner )
    }

sidebarGraphDimensions : UIState -> ( Float, Float )
sidebarGraphDimensions uiState =
  let
    ( w, _ ) = uiState.dimensions.sideBar
  in
  ( w - 20 , 9/16 * ( w - 20 ) )


updateMainEditorDimensions : Model -> Model
updateMainEditorDimensions ({uiState} as model) =
  let
    updateMain : GraphView -> GraphView
    updateMain graph_view =
      let
        guest_viewport =
          calculateGuestDimensionsForHost
            uiState.dimensions.mainEditor
            graph_view.isFrozen
            graph_view.package.userGraph.graph
      in
        { graph_view
          | host_dimensions = uiState.dimensions.mainEditor
          , panBuffer = GraphEditor.panBufferAmount uiState.dimensions.mainEditor
          , guest_dimensions = guest_viewport.dimensions
          , guest_coordinates = guest_viewport.coordinates
          , host_coordinates =
              if uiState.open.sideBar then
                (Tuple.first uiState.dimensions.sideBar + 8 + 48, 0)
              else
                (0, 0)
        }
    updateSidebar : GraphView -> GraphView
    updateSidebar graph_view =
      let
        sidebarDimensions =
          sidebarGraphDimensions uiState
        guest_viewport =
          calculateGuestDimensionsForHost
            sidebarDimensions
            True
            graph_view.package.userGraph.graph
      in
        { graph_view
          | host_dimensions = sidebarDimensions
          , panBuffer = 0
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
                    if uiState.open.sideBar then
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
dragSplitter : Float -> SplitterMovement -> UIConstants -> UIState -> UIState
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
toggleAreaVisibility : AreaUITarget -> UIState -> UIState
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
resizeViewport (w, h) ({uiState, uiConstants} as model) =
  let
    dim = uiState.dimensions
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
    
    state : UIState
    state =
      { uiState
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
      | uiState = state
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

peekInteraction : Maybe Uuid -> Model -> Maybe InteractionState
peekInteraction uuid model =
  AutoDict.get uuid model.interactionsDict
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

panGraphView : Float -> Float -> GraphView -> GraphView
panGraphView x y graph_view =
  let
    ( host_x, host_y ) = graph_view.host_coordinates
    ( host_width, host_height ) = graph_view.host_dimensions
    ( guest_x, guest_y ) = graph_view.guest_coordinates
    ( guest_width, guest_height) = graph_view.guest_dimensions
    ( guest_inner_x, guest_inner_y ) = graph_view.guest_inner_coordinates
    ( guest_inner_width, guest_inner_height ) = graph_view.guest_inner_dimensions
    ( pan_x, pan_y ) = graph_view.pan
    calc_for_dim v current_pan host_coord host_dim guest_coord guest_dim guest_inner_coord guest_inner_dim =
      -- left and top are the "negative" directions.
      if v <= host_coord + graph_view.panBuffer && guest_coord - current_pan - 1 < guest_inner_coord then
        current_pan - 1
      -- right and bottom are the "positive" directions
      else if v >= host_coord + host_dim - graph_view.panBuffer && guest_coord - current_pan + 1 + guest_dim > guest_inner_coord + guest_inner_dim then
        current_pan + 1
      else
        current_pan
  in
    if graph_view.properties.canPan then
      { graph_view
        | pan =
            ( calc_for_dim x pan_x host_x host_width  guest_x guest_width  guest_inner_x guest_inner_width
            , calc_for_dim y pan_y host_y host_height guest_y guest_height guest_inner_y guest_inner_height
            )
      }
    else
      graph_view

packagesToGraphViews : (Float, Float) -> Model -> (List GraphView, Model)
packagesToGraphViews (w, h) model =
  List.foldl
    (\uuid (acc, model_) ->
      let
        ( v, model__) =
          solvedViewFromPackage (w, h) Sidebar True uuid model_
      in
        (v :: acc, model__)
    )
    ( [], model )
    (AutoDict.values model.packages)

selectComputationsIcon : Model -> Model
selectComputationsIcon model =
  let
    (computation_views, updated_model) =
      packagesToGraphViews (sidebarGraphDimensions model.uiState) model
  in
    { updated_model
      | uiState =
          let uiState = model.uiState in
          { uiState
            | selected =
              let selected = uiState.selected in
              { selected | sideBar = ComputationsIcon }
          }
      , computationsExplorer =
          List.map .id computation_views
    }

update_ui : UIMsg -> Model -> ( Model, Cmd Msg )
update_ui ui_msg model =
  case ui_msg of
    ToggleAreaVisibility where_ ->
      ( { model
          | uiState = toggleAreaVisibility where_ model.uiState
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
    StartDraggingNode graphView_uuid nodeId ->
      ( pushInteractionForStack (Just graphView_uuid) (DraggingNode nodeId) model
        |> setProperties
      , Cmd.none
      )
    DragSplitter shouldStop coord ->
      ( case popInteraction (Nothing) model of
          Just (DraggingSplitter movement, model_) ->
            if shouldStop then
              model_
              |> updateMainEditorDimensions
              |> setProperties
            else
              { model
                | uiState =
                    dragSplitter coord movement model.uiConstants model.uiState
              }
              |> updateMainEditorDimensions
              |> setProperties
          _ ->
            model
      , Cmd.none
      )
    SelectNavigation ComputationsIcon ->
      ( selectComputationsIcon model
      , Cmd.none
      )

    SelectNavigation _ ->
      Debug.todo "SelectNavigation _ not implemented."

    SelectTool item ->
      ( { model
          | uiState =
              let uiState = model.uiState in
              { uiState
                | selected =
                    let selected = uiState.selected in
                    { selected | bottomPanel = item }
              }
        }
      , Cmd.none
      )

    OnResize dims ->
      ( resizeViewport dims model
      , Cmd.none
      )

    ConsiderPan uuid rectangles ->
      ( model
      , E.object
          [ ("uuid", Uuid.encode uuid)
          , ("rectangles"
            , E.list
                (\{w,h,x,y} ->
                  E.object
                    [ ("w", E.float w)
                    , ("h", E.float h)
                    , ("x", E.float x)
                    , ("y", E.float y)
                    ]
                )
                rectangles
            )
          ]
        |> considerPan
      )

    Pan uuid x y ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (panGraphView x y))
                model.graph_views
        }
      , -- this fulfils a more responsive sort of "protocol": if JS says "Pan!", and there is
        -- either nothing to pan or nothing pannable, then tell it to stop panning.
        AutoDict.get uuid model.graph_views
        |> Maybe.map (\{properties} -> if properties.canPan then Cmd.none else stopPan (Uuid.toString uuid))
        |> Maybe.withDefaultLazy (\() -> stopPan (Uuid.toString uuid))
      )

    StopPan uuid ->
      ( model
      , stopPan (Uuid.toString uuid)
      )

    ResetPan uuid ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (\graph_view ->
                  { graph_view | pan = (0, 0) }
                ))
                model.graph_views
        }
      , stopPan (Uuid.toString uuid)
      )

    RequestCoordinates uuid ->
      ( model
      , requestCoordinates (Uuid.toString uuid)
      )

    ReceiveCoordinates uuid (x, y) ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (\graph_view ->
                  { graph_view | host_coordinates = (x, y) }
                ))
                model.graph_views
        }
      , Cmd.none
      )

    SelectPackage uuid ->
      ( AutoDict.get uuid model.packages
        |> Maybe.map
          (\pkg ->
            solvedViewFromPackage model.uiState.dimensions.mainEditor MainEditor False pkg model
            |> Tuple.second
            |> setProperties
          )
        |> Maybe.withDefault model
      , Cmd.none
      )

    SelectNode view_uuid node_id ->
      ( case peekInteraction (Just view_uuid) model of
          Just (ChoosingDestinationFor src etc) ->
            selectDestination view_uuid src etc model
          _ -> -- includes 'Nothing'
            -- this is the initial selection.
            selectSourceNode model view_uuid node_id
            |> setProperties
      , Cmd.none
      )

    SelectSpace view_uuid ->
      ( case peekInteraction (Just view_uuid) model of
          Just (ChoosingDestinationFor src etc) ->
            selectDestination view_uuid src etc model
          _ ->
            model
      , Cmd.none
      )
    
    MovePhantomNode view_uuid (x, y) ->
      ( movePhantomNode view_uuid (x, y) model
        |> setProperties
      , Cmd.none
      )

    ConnectionEditorInput input ->
      ( case mostRecentInteraction model of
          Just (key_uuid, EditingConnection alteration props) ->
            handleConnectionEditorInput key_uuid input alteration props model
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
            _ ->
              model
        , Cmd.none
        )

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

-- toggleSelectedCondition : AcceptVia -> Model -> Model
-- toggleSelectedCondition acceptCondition model =
--   case model.interactionsDict of
--     Just (ModifyingGraph via ({ transitions } as mod)) ->
--       { model
--         | interactionsDict =
--             Just <| ModifyingGraph via <|
--               { mod
--                 | transitions = alterTransitions transitions
--               }
--       }
--     Just (AlteringConnection via ({ transitions } as mod)) ->
--       { model
--         | interactionsDict =
--             Just <| AlteringConnection
--               via
--               { mod
--                 | transitions = alterTransitions transitions
--               }
--       }
--     _ ->
--       model

filterConnectionEditorGraphs : String -> List Uuid -> Model -> List Uuid
filterConnectionEditorGraphs s referenceList model =
  let
    v = String.toLower s
  in
  List.filterMap (\uuid -> AutoDict.get uuid model.graph_views) referenceList
  |> List.filter
    (\gv ->
      case gv.package.description of
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
            pkg =
              let package = gv.package in
              { package
                | userGraph =
                    case Graph.get dest package.userGraph.graph of
                      Nothing ->
                        GraphEditor.newnode_graphchange source x y connection package.userGraph
                      Just _ ->
                        GraphEditor.updateLink_graphchange source dest connection package.userGraph
              }
            ( main_view, updated_model ) =
              naiveViewFromPackage
                ( 250
                , 250 --* 9/16
                )
                Independent
                True
                pkg
                model_
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
            case peekInteraction view_uuid model_ of
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
                    (centerAndHighlight source dest >> Just)
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
  |> Maybe.andThen (.package >> .userGraph >> .graph >> Graph.get node_id)
  |> Maybe.map
    (\nodeContext ->
      let
        linkDrawingData : LinkDrawingData
        linkDrawingData =
          { cardinality = Recursive
          , pathBetween =
              path_between
                nodeContext.node.label
                nodeContext.node.label
                Recursive
          , executionData = Nothing
          , label = AutoSet.empty transitionToString
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
      unusedNodeId graph_view.package.userGraph
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
      , pathBetween =
          path_between -- calculate the link path
            sourceNodeContext.node.label
            { x = x_, y = y_ }
            Unidirectional
      , executionData = Nothing
      , label = AutoSet.empty transitionToString
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

phantomLinkDrawingForExisting : Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> LinkDrawingData
phantomLinkDrawingForExisting sourceNodeContext existingNodeContext =
  let
    cardinality =
      identifyCardinalityViaContext sourceNodeContext.node.id existingNodeContext
    connection =
      IntDict.get sourceNodeContext.node.id existingNodeContext.incoming
      |> Maybe.withDefaultLazy (\() -> AutoSet.empty transitionToString)
  in
    { cardinality = cardinality
    , pathBetween =
        path_between -- calculate the link path
          sourceNodeContext.node.label
          existingNodeContext.node.label
          cardinality
    , executionData = Nothing
    , label = connection
    , highlighting = Just Phantom
    }

switchFromPhantomToExisting : Uuid -> NodeId -> Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromPhantomToExisting view_uuid phantom_id sourceNodeContext existingNodeContext model =
  let
    linkData = -- this is the new calculated link-path
      phantomLinkDrawingForExisting sourceNodeContext existingNodeContext
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
            ( ExistingNode existingNodeContext.node.id linkData.label )
        )

switchFromExistingToExisting : Uuid -> Bool -> NodeId -> Graph.NodeContext Entity Connection -> Graph.NodeContext Entity Connection -> Model -> Model
switchFromExistingToExisting view_uuid old_conn_is_empty old_existing sourceNodeContext nearbyNodeContext model =
  let
    linkData = -- this is the new calculated link-path
      phantomLinkDrawingForExisting sourceNodeContext nearbyNodeContext
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
            ( ExistingNode nearbyNodeContext.node.id linkData.label )
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
  let
    -- what is the destination of this link?
    -- Sounds like a simple question: it's the phantom node, of course.  But when the
    -- phantom node gets too close to a REAL node, then it should switch to that node;
    -- and when it is far enough away, then it should switch back.
    nearby_node_lockOnDistance : Float
    nearby_node_lockOnDistance = 20

    ( xPan, yPan ) =
      graph_view.pan
    nearby_node_func : ((Graph.Node Entity -> Bool) -> List (Graph.Node Entity) -> b) -> Float -> (Float, Float) -> GraphView -> b
    nearby_node_func f distance mouseCoords { package } =
      -- a good distance value is nodeRadius + 9 = 7 + 9 = 16, for "locking on".
      let
        userGraph = package.userGraph |> debugAutomatonGraph "userGraph in movePhantomNode"
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

    nearby_node : Float -> (Float, Float) -> GraphView -> Maybe (Graph.Node Entity)
    nearby_node =
      nearby_node_func List.find

    -- nearby_nodes : Float -> (Float, Float) -> GraphView -> List (Graph.Node Entity)
    -- nearby_nodes =
    --   nearby_node_func List.filter

  in
    -- first, let's find this thing.
    case peekInteraction (Just view_uuid) model of
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
              (Graph.get source graph_view.package.userGraph.graph)
              (Graph.get nearbyNode.id graph_view.package.userGraph.graph)
            |> Maybe.withDefault model
          Nothing ->
            -- great, there is no nearby node; just update the (x, y).
            Maybe.map
              (\sourceNodeContext ->
                updatePhantomMovement view_uuid phantom_id (x_, y_) sourceNodeContext model
              )
              (Graph.get source graph_view.package.userGraph.graph)
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
                (Graph.get source graph_view.package.userGraph.graph)
                (Graph.get nearbyNode.id graph_view.package.userGraph.graph)
              |> Maybe.withDefault model -- no changes made.
          Nothing ->
            -- there is no nearby node; move from the existing node to a phantom node. 
            Maybe.map
              (\sourceNodeContext -> switchFromExistingToPhantom view_uuid (AutoSet.isEmpty conn) existing_node graph_view (x_, y_) sourceNodeContext model)
              (Graph.get source graph_view.package.userGraph.graph)
            |> Maybe.withDefault model
      _ ->
        model

movePhantomNode : Uuid -> (Float, Float) -> Model -> Model
movePhantomNode view_uuid (x_, y_) model =
  AutoDict.get view_uuid model.graph_views
  |> Maybe.map (\gv -> movePhantomNodeInView view_uuid gv (x_, y_) model)
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
        | package =
            let package = graph_view.package in
            { package
              | userGraph =
                let userGraph = package.userGraph in
                { userGraph
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
                        userGraph.graph
                }
            }
      }
  in
    { model
      | graph_views =
          AutoDict.update view_uuid
            (Maybe.map (viewWithNode))
            model.graph_views
    }

defaultViewProperties : Bool -> GraphViewPropertySetter
defaultViewProperties isFrozen package =
  { canSelectConnections = not isFrozen
  , canSelectEmptySpace = False
  , canSelectNodes = not isFrozen
  , canSplitNodes = not isFrozen
  , canDragNodes = not isFrozen
  , canInspectRefs = True
  , canPan = True
  , canChooseInPackageList = package.undoBuffer == []
  }

defaultMainProperties : MainUIProperties
defaultMainProperties =
  { canEscape = False
  , canDragSplitter = True
  , canAcceptCharacters = False
  , dragDirection = Nothing
  }

type alias GraphViewPropertySetter = GraphPackage -> GraphViewProperties
type alias MainPropertySetter = MainUIProperties

setProperties : Model -> Model
setProperties model =
  let
    setLocalProperties f a = (Tuple.first f) a
    setMainProperties f = (Tuple.second f)
    default : (Bool -> GraphViewPropertySetter, MainPropertySetter)
    default = ( defaultViewProperties, defaultMainProperties )
    whenSplittingNode : Bool -> (GraphViewPropertySetter, MainPropertySetter)
    whenSplittingNode isFrozen =
      ( \package ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = not isFrozen
          , canInspectRefs = True
          , canPan = True
          , canChooseInPackageList = package.undoBuffer == []
          }
      , { canEscape = True
        , canDragSplitter = True
        , canAcceptCharacters = False
        , dragDirection = Nothing
        }
      )
    whenDraggingNode : (GraphViewPropertySetter, MainPropertySetter)
    whenDraggingNode =
      ( \package ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = False
          , canInspectRefs = False
          , canPan = True
          , canChooseInPackageList = package.undoBuffer == []
          }
      , { canEscape = True
        , canDragSplitter = False
        , canAcceptCharacters = False
        , dragDirection = Nothing
        }
      )
    whenDraggingSplitter : SplitterMovement -> (GraphViewPropertySetter, MainPropertySetter)
    whenDraggingSplitter movement =
      ( \package ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = False
          , canInspectRefs = False
          , canPan = False
          , canChooseInPackageList = package.undoBuffer == []
          }
      , { canEscape = False
        , canDragSplitter = False
        , canAcceptCharacters = False
        , dragDirection = Just movement
        }
      )
    whenSourceNodeSelected : (GraphViewPropertySetter, MainPropertySetter)
    whenSourceNodeSelected =
      ( \package ->
          { canSelectConnections = False
          , canSelectEmptySpace = True
          , canSelectNodes = True
          , canSplitNodes = True
          , canDragNodes = True
          , canInspectRefs = False
          , canPan = True
          , canChooseInPackageList = package.undoBuffer == []
          }
      , { canEscape = True
        , canDragSplitter = True
        , canAcceptCharacters = False
        , dragDirection = Nothing
        }
      )
    whenEditingConnection : (GraphViewPropertySetter, MainPropertySetter)
    whenEditingConnection =
      ( \_ ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = False
          , canInspectRefs = True
          , canPan = True
          , canChooseInPackageList = False
          }
      , { canEscape = True
        , canDragSplitter = False
        , canAcceptCharacters = True
        , dragDirection = Nothing
        }
      )
    whenExecuting : (GraphViewPropertySetter, MainPropertySetter)
    whenExecuting =
      ( \package ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = False
          , canInspectRefs = True
          , canPan = True
          , canChooseInPackageList = package.undoBuffer == []
          }
      , { canEscape = True
        , canDragSplitter = True
        , canAcceptCharacters = False
        , dragDirection = Nothing
        }
      )
    mainProperties_base =
      case peekInteraction (Nothing) model of
        Just (DraggingSplitter movement) ->
          setMainProperties (whenDraggingSplitter movement)
        _ ->
          setMainProperties default
  in
    { model
      | graph_views =
          AutoDict.map
            (\k v ->
              { v
                | properties =
                    case peekInteraction (Just k) model of
                      Nothing ->
                        setLocalProperties default v.isFrozen v.package
                      Just (SplittingNode _) ->
                        setLocalProperties (whenSplittingNode v.isFrozen) v.package
                      Just (DraggingNode _) ->
                        setLocalProperties whenDraggingNode v.package
                      Just (ChoosingDestinationFor _ _) ->
                        setLocalProperties whenSourceNodeSelected v.package
                      Just (Executing _) ->
                        setLocalProperties whenExecuting v.package
                      Just (EditingConnection _ _) ->
                        setLocalProperties whenEditingConnection v.package
                      x ->
                        Debug.todo <| "Received a local interaction for " ++ Debug.toString k ++ " that should never have been received… " ++ Debug.toString x
              }
            )
            model.graph_views
      , properties =
          case mostRecentInteraction model of
            Just (_, SplittingNode _) ->
              { mainProperties_base | canEscape = True }
            Just (_, DraggingNode _) ->
              mainProperties_base
            Just (_, ChoosingDestinationFor _ _) ->
              { mainProperties_base | canEscape = True }
            Just (_, EditingConnection _ _) ->
              { mainProperties_base | canEscape = True, canAcceptCharacters = True }
            Just (_, Executing _) ->
              { mainProperties_base | canEscape = True }
            Just (_, DraggingSplitter _) ->
              mainProperties_base
            Nothing ->
              mainProperties_base
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
          |> Maybe.andThen (\gv -> Graph.get dest_id gv.package.userGraph.graph)
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

deleteLinkFromView : Uuid -> NodeId -> NodeId -> Model -> Model
deleteLinkFromView view_uuid source dest model =
  let
    conn =
      AutoDict.get view_uuid model.graph_views
      |> Maybe.andThen (\gv -> Graph.get dest gv.package.userGraph.graph)
      |> Maybe.andThen (\{incoming} -> IntDict.get source incoming)
      |> Maybe.withDefault (AutoSet.empty transitionToString)
  in
    if AutoSet.isEmpty conn then
      { model
        | graph_views =
            AutoDict.update view_uuid
              (Maybe.map (\graph_view ->
                { graph_view
                  | drawingData =
                      let drawingData = graph_view.drawingData in
                      { drawingData
                        | link_drawing = Dict.remove (source, dest) drawingData.link_drawing
                      }
                  , package =
                      let pkg = graph_view.package in
                      { pkg
                        | userGraph =
                          let ag = pkg.userGraph in
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
                      }
                }
              ))
              model.graph_views
      }
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
                  | package =
                      let pkg = graph_view.package in
                      { pkg
                        | userGraph =
                          let ag = pkg.userGraph in
                          { ag
                            | graph = Graph.remove dest ag.graph
                          }
                      }
                }
              ))
              model_.graph_views
      }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg {- |> (\v -> if v == ForceDirectedMsg FDG.Tick then v else Debug.log "MESSAGE" v) -} of
    UIMsg ui_msg ->
      update_ui ui_msg model

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
            Just (Just uuid, EditingConnection {source, dest, deleteTargetIfCancelled} {referenceList, mainGraph}, model_) ->
              -- clear out the reference-list.
              let
                model_without_editor_views =
                  { model_
                    | graph_views =
                        List.foldl AutoDict.remove model_.graph_views (mainGraph :: referenceList)
                  }
              in


              if deleteTargetIfCancelled then
                deleteNodeFromView uuid source dest model_without_editor_views
                |> setProperties
              else
                -- does deletion link deletion IF appropriate.
                deleteLinkFromView uuid source dest model_without_editor_views
                |> setProperties
            Just (_, _, model_) ->
              model_ -- yay, I could pop from the global
              |> setProperties
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

    Undo uuid ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (\graph_view ->
                    case (mostRecentInteraction model, graph_view.package.undoBuffer) of
                      (_, []) ->
                        graph_view
                      (Just _, _) ->
                        graph_view -- do not permit undo/redo while I'm performing any operation.
                      (Nothing, h::t) ->
                        let
                          pkg = graph_view.package
                        in
                          { graph_view
                            | package =
                                { pkg
                                  | undoBuffer = t
                                  , redoBuffer = graph_view.package.userGraph :: graph_view.package.redoBuffer
                                  , userGraph = h
                                }
                            , disconnectedNodes = GraphEditor.identifyDisconnectedNodes h
                          }
                ))
                model.graph_views
        }
      , Cmd.none
      )

    Redo uuid ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (\graph_view ->
                    case (mostRecentInteraction model, graph_view.package.redoBuffer) of
                      (_, []) ->
                        graph_view
                      (Just _, _) ->
                        graph_view -- do not permit undo/redo while I'm performing any operation.
                      (Nothing, h::t) ->
                        let
                          pkg = graph_view.package
                        in
                          { graph_view
                            | package =
                                { pkg
                                  | redoBuffer = t
                                  , undoBuffer = graph_view.package.userGraph :: graph_view.package.undoBuffer
                                  , userGraph = h
                                }
                            , disconnectedNodes = GraphEditor.identifyDisconnectedNodes h
                          }
                ))
                model.graph_views
        }
      , Cmd.none
      )

    CrashWithMessage err ->
      Debug.todo err

    -- GraphViewMsg uuid submsg ->
    --   AutoDict.get uuid model.graph_views
    --   |> Maybe.map (updateGraphView submsg)
    --   |> Maybe.withDefault ( model, Cmd.none )

    -- UpdateTestPanelContent newInput expectation ->
    --   let
    --     pkg = model.fdg_model.package
    --     resolutionDict =
    --       AutoDict.map (\_ -> .userGraph) model.fdg_model.packages
    --     updatedTests =
    --       case newInput of
    --         "" ->
    --           AutoDict.remove pkg.currentTestKey pkg.tests
    --         _ ->
    --           AutoDict.insert
    --             pkg.currentTestKey
    --               ( Test
    --                   newInput
    --                   expectation
    --                   ( DFA.load newInput resolutionDict pkg.userGraph
    --                     |> DFA.run
    --                   )
    --               )
    --             pkg.tests
    --     updatedPackage =
    --       { pkg | tests = updatedTests }
    --   in
    --     ( { model
    --         | fdg_model =
    --             FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
    --       }
    --     , persistPackage updatedPackage
    --     )

    -- UpdateDescriptionPanelContent content ->
    --   let
    --     currentPackage = model.fdg_model.package
    --     updatedPackage =
    --       { currentPackage
    --         | description =
    --             if content == "" then
    --               Nothing
    --             else
    --               Just content
    --       }
    --   in
    --     ( { model
    --         | fdg_model = FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
    --       }
    --     , persistPackage updatedPackage
    --     )

    -- StepThroughExecution ->
    --   let
    --     inputString =
    --       AutoDict.get model.fdg_model.package.currentTestKey model.fdg_model.package.tests
    --       |> Maybe.map .input
    --       |> Maybe.withDefault ""
    --     newFdModel =
    --       if model.executionStage /= StepThrough then
    --         -- this is the first click of stepping, so do a load first.
    --         DFA.load
    --           inputString
    --           (AutoDict.map (\_ -> .userGraph) model.fdg_model.packages) -- resolution-dict
    --           model.fdg_model.package.userGraph
    --       else
    --         FDG.update FDG.Step model.fdg_model
    --   in
    --   ( { model 
    --     | executionStage =
    --         case newFdModel.interactionsDict of
    --           Just (Executing _ (CanContinue _)) ->
    --             StepThrough
    --           Just (Executing _ _) ->
    --             ExecutionComplete
    --           _ ->
    --             model.executionStage
    --     , fdg_model =
    --         newFdModel
    --     }
    --   , Cmd.none
    --   )

    -- SelectBottomPanel p ->
    --   ( { model | selectedBottomPanel = p }
    --   , Cmd.none
    --   )

    -- CreateNewPackage ->
    --     ( { model | fdg_model = FDG.reInitialize model.fdg_model }
    --     , Cmd.none
    --     )

    -- SelectPackage uuid ->
    --   case AutoDict.get uuid model.fdg_model.packages of
    --     Nothing ->
    --       ( model, Cmd.none )
    --     Just pkg ->
    --       let
    --         fdg_model = model.fdg_model
    --         newFdgModel =
    --           { fdg_model | currentPackage = { pkg | dimensions = fdg_model.dimensions } }
    --           |> FDG.update (FDG.ViewportUpdated model.fdg_model.dimensions)
    --           |> FDG.update FDG.Reheat
    --       in
    --       ( { model
    --           | fdg_model = newFdgModel
    --         }
    --       , Cmd.none
    --       )

    -- DeletePackage uuid ->
    --   let
    --     fdg_model = model.fdg_model
    --     newPackages = AutoDict.remove uuid model.fdg_model.packages
    --     newFdgModel = { fdg_model | packages = newPackages }
    --   in
    --     ( if uuid == fdg_model.package.userGraph.graphIdentifier then
    --         { model | fdg_model = FDG.reInitialize newFdgModel }
    --       else
    --         { model | fdg_model = newFdgModel }
    --     , Ports.deleteFromStorage (Uuid.toString uuid)
    --     )

    -- CreateNewTest ->
    --   let
    --     fdg_model = model.fdg_model
    --     currentPackage = fdg_model.package
    --     (uuid, newSeed) = Random.step Uuid.generator fdg_model.uuidSeed
    --     newFdgModel =
    --       { fdg_model
    --         | currentPackage = { currentPackage | currentTestKey = uuid }
    --         , uuidSeed = newSeed
    --       }
    --   in
    --     ( { model | fdg_model = newFdgModel }
    --     , Cmd.none
    --     )

    -- SelectTest key ->
    --   let
    --     currentPackage = model.fdg_model.package
    --     updatedPackage = { currentPackage | currentTestKey = key }
    --   in
    --     ( { model
    --         | fdg_model = FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
    --       }
    --     , Cmd.none -- no need to persist this though
    --     )

    -- DeleteTest key ->
    --   let
    --     currentPackage = model.fdg_model.package
    --     updatedPackage =
    --       { currentPackage | tests = AutoDict.remove key currentPackage.tests }
    --   in
    --     ( { model
    --         | fdg_model = FDG.update (FDG.UpdateCurrentPackage updatedPackage) model.fdg_model
    --       }
    --     , persistPackage updatedPackage
    --     )

-- isAccepted : ExecutionResult -> Maybe Bool
-- isAccepted result =
--   case result of
--     InternalError -> Nothing
--     CanContinue _ -> Nothing
--     EndOfInput (Accepted _) -> Just True
--     _ -> Just False

-- isPassingTest : Test -> Maybe Bool
-- isPassingTest test =
--   isAccepted test.result
--   |> Maybe.map
--     (\v ->
--       (test.expectation == ExpectAccepted && v) || (test.expectation == Automata.Data.ExpectRejected && not v)
--     )

-- isFailingTest : Test -> Maybe Bool
-- isFailingTest = isPassingTest >> Maybe.map not

-- viewIconBar : Model -> Html Msg
-- viewIconBar model =
--   let
--     (pass, fail, error) =
--       AutoDict.toList model.fdg_model.package.tests
--       |> List.foldl
--         (\(_, {expectation, result}) (p, f, e) ->
--           case isAccepted result of
--             Just True ->
--               if expectation == ExpectAccepted then
--                 (p + 1, f, e)
--               else
--                 (p, f + 1, e)
--             Just False ->
--               if expectation == ExpectRejected then
--                 (p + 1, f, e)
--               else
--                 (p, f + 1, e)
--             Nothing -> (p, f, e + 1)
--         )
--         (0, 0, 0)
--     (testBackgroundColor, number, testTitle) =
--       case (pass, fail, error) of
--         (0, 0, 0) ->
--           ( Css.rgb 0xf8 0xf8 0xf2 -- --dracula-foreground
--           , "…"
--           , "No tests exist"
--           )
--         (_, 0, 0) -> -- no failures, no errors, and only passes.
--           ( Css.rgb 0x8c 0xf1 0x8c -- --dracula-green
--           , "💯"
--           , "All tests passed!"
--           )
--         (0, _, 0) -> -- no passes, no errors, only failures.
--           ( Css.rgb 0xff 0x55 0x55 -- --dracula-red
--           , "😵‍💫"
--           , "All tests failed!"
--           )
--         (_, _, 0) -> -- some passes, some failures, no errors
--           ( Css.rgb 0xf1 0x8c 0x8c -- --dracula-pink
--           , String.fromInt fail
--           , if fail == 1 then
--               "1 test failed"
--             else
--               String.fromInt fail ++ " tests failed"
--           )
--         _ -> -- internal error exists!
--           ( Css.rgb 0xf1 0xfa 0x8c -- --dracula-yellow
--           , "❓"
--           , "Some tests did not complete due to an internal error! This indicates a problem with the computation system. Please report it to the developer."
--           )
--     testsExtra =
--       [ div
--           [ HA.class "test-panel__number"
--           , HA.css
--               [ Css.backgroundColor <| testBackgroundColor
--               , Css.color <| Css.rgb 0x28 0x2a 0x36 -- --dracula-background
--               , Css.borderRadius (Css.pct 50)
--               -- , Css.borderColor <| Css.rgb 0x44 0x47 0x5a -- --dracula-current-line
--               -- , Css.borderWidth (Css.px 1)
--               -- , Css.borderStyle Css.solid
--               , Css.position Css.absolute
--               , Css.left (Css.pct 50)
--               , Css.top (Css.pct 50)
--               -- , Css.fontSize (Css.pct 60)
--               , Css.padding (Css.px 2)
--               -- , Css.opacity (Css.num 0.8)
--               , Css.userSelect Css.none
--               , Css.width (Css.em 1.2)
--               , Css.height (Css.em 1.2)
--               , Css.lineHeight (Css.em 1.2)
--               , Css.textAlign Css.center
--               , Css.fontWeight Css.bold
--               -- , let
--               --     o = 0.5
--               --     blur = 1
--               --   in
--               --     Css.textShadowMany
--               --     [ { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px o, offsetY = Css.px o, color = Just <| testBackgroundColor }
--               --     , { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px -o, offsetY = Css.px -o, color = Just <| testBackgroundColor }
--               --     , { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px -o, offsetY = Css.px o, color = Just <| testBackgroundColor }
--               --     , { defaultTextShadow | blurRadius = Just <| Css.px blur, offsetX = Css.px o, offsetY = Css.px -o, color = Just <| testBackgroundColor }
--               --     ]
--               ]
--           , HA.title testTitle
--           ]
--           [ text number ]
--       ]
--   in
--     div 
--       [ HA.class "icon-bar" ]
--       [ viewIcon ComputationsIcon "📁" [] model
--       , viewIcon TestsIcon "🧪" testsExtra model
--       , viewIcon SearchIcon "🔍" [] model
--       , viewIcon GitIcon "🌿" [] model
--       , viewIcon ExtensionsIcon "🧩" [] model
--       ]








-- viewTestItemInPanel : (Uuid, Test) -> Html Msg
-- viewTestItemInPanel (key, test) =
--   let
--     testStatus = isFailingTest test
--   in
--     div 
--       [ HA.classList
--           [ ("left-panel__testItem", True)
--           , ("left-panel__testItem--failing", testStatus == Just True)
--           , ("left-panel__testItem--passing", testStatus == Just False)
--           , ("left-panel__testItem--error", testStatus == Nothing)
--           ]
--       , onClick (SelectTest key)
--       ]
--       [ span
--           [ HA.css
--             [ Css.width (Css.pct 100)
--             -- , Css.whiteSpace Css.nowrap
--             , Css.overflow Css.hidden
--             , Css.textOverflow Css.ellipsis
--             , Css.whiteSpace Css.pre
--             , Css.fontFamilyMany
--                 [ "Fira Code", "Inconsolata", "DejaVu Sans Mono"
--                 , "Liberation Mono", "Ubuntu Mono", "Cascadia Code"
--                 , "Cascadia Mono", "Consolas", "Lucida Console"
--                 , "Courier New" ]
--                 Css.monospace
--             ]
--           ]
--           [ text test.input ]
--       , div
--           [ HA.css
--               [ Css.position Css.absolute
--               , Css.right (Css.px 5)
--               , Css.top (Css.px 5)
--               ]
--           , HA.class "button"
--           , HA.title "Delete test"
--           , onClick (DeleteTest key)
--           ]
--           [ text "🚮" ]
--       ]

-- viewLeftTestPanel : Model -> Html Msg
-- viewLeftTestPanel model =
--   let
--     (expectAccept, expectReject) =
--       model.fdg_model.package.tests
--       |> AutoDict.toList
--       |> List.partition (Tuple.second >> .expectation >> (==) ExpectAccepted)
--     displayTests headingHtml tx =
--       case tx of
--         [] ->
--           text ""
--         _ ->
--           div
--             []
--             [ h4 [] headingHtml
--             , ul []
--               ( List.sortBy (Tuple.second >> .input) tx
--                 |> List.map
--                   (\(key, test) ->
--                     li
--                       []
--                       [ viewTestItemInPanel (key, test) ]
--                   )
--               )
--             ]
--   in
--     div
--       [ HA.css
--           [ Css.backgroundColor <| Css.rgb 0x28 0x2a 0x36 -- --dracula-background
--           , Css.color <| Css.rgb 0xf8 0xf8 0xf2 -- --dracula-foreground
--           ]
--       ]
--       [ h2
--           []
--           [ text "Tests "
--           , div
--               [ HA.class "button button--primary"
--               , onClick CreateNewTest
--               , HA.title "Create new test"
--               ]
--               [ text "➕" ]
--           ]
--       , model.fdg_model.package.description
--         |> Maybe.map
--           (\desc ->
--             div
--               [ HA.css
--                   [ Css.fontStyle Css.italic
--                   , Css.fontSize (Css.rem 0.8)
--                   , Css.whiteSpace Css.preWrap
--                   ]
--               ]
--               [ text "“"
--               , text desc
--               , text "”"
--               ]
--           )
--         |> Maybe.withDefault (text "")
--       , displayTests
--           [ span [] [ text "Should be " ]
--           , span [ HA.css [ Css.color <| Css.rgb 0x50 0xfa 0x7b ] ] [ text "accepted" ]
--           ]
--           expectAccept
--       , displayTests
--           [ span [] [ text "Should be " ]
--           , span [ HA.css [ Css.color <| Css.rgb 0xff 0x79 0xc6 ] ] [ text "rejected" ]
--           ]
--         expectReject
--       ]


-- viewTestPanelButtons : Model -> List (Html Msg)
-- viewTestPanelButtons model =
--   [ div
--     [ HA.class (getActionButtonClass model.executionStage RunExecution)
--     , onClick RunExecution
--     , HA.disabled (model.executionStage == ExecutionComplete || not (FDG.canExecute model.fdg_model))
--     , HA.title "Run"
--     ]
--     [ text "▶️" ]
--   , div
--     [ HA.class (getActionButtonClass model.executionStage ResetExecution)
--     , onClick ResetExecution
--     , HA.disabled (model.executionStage == Ready || model.executionStage == NotReady)
--     , HA.title "Reset"
--     ]
--     [ text "⏹️" ]
--   , div
--     [ HA.class (getActionButtonClass model.executionStage StepThroughExecution)
--     , onClick StepThroughExecution
--     , HA.disabled (model.executionStage == ExecutionComplete || not (FDG.canExecute model.fdg_model))
--     , HA.title "Step-through"
--     ]
--     [ text "⏭️" ]
--   , div
--     [ HA.class (getActionButtonClass model.executionStage (DeleteTest model.fdg_model.package.currentTestKey))
--     , HA.css
--         [ Css.marginTop (Css.px 15)
--         ]
--     , onClick <| DeleteTest model.fdg_model.package.currentTestKey
--     , HA.disabled (model.executionStage == StepThrough)
--     , HA.title "Delete test"
--     ]
--     [ text "🚮" ]
--   ]

-- viewDescriptionPanelButtons : Model -> List (Html Msg)
-- viewDescriptionPanelButtons _ =
--   []

-- viewBottomPanelHeader : Model -> Html Msg
-- viewBottomPanelHeader model =
--   let
--     buttons =
--       ( case model.selectedBottomPanel of
--           AddTestPanel ->
--             viewTestPanelButtons model
--           EditDescriptionPanel ->
--             viewDescriptionPanelButtons model
--       )
--   in
--   div 
--     [ HA.classList
--         [ ("bottom-panel__header", True)
--         , ("open", not <| List.isEmpty buttons)
--         ]
--     ]
--     [ div 
--       [ HA.class "bottom-panel__actions" ]
--       buttons
--     ]

-- executionText : Model -> Html Msg
-- executionText { fdg_model } =
--   div
--     []
--     [ case fdg_model.interactionsDict of
--         Just (Executing _ result) ->
--           let
--             maybeDatum =
--               FDG.executionData result
--             showProgress : ExecutionData -> Html Msg
--             showProgress datum =
--               p
--                 [ HA.class "computation-progress" ]
--                 [ span
--                     [ HA.class "computation-progress-processed" ]
--                     ( datum.transitions
--                       -- |> Debug.log "Execution-data transitions"
--                       |> List.map
--                         (\{matching} ->
--                           viewInputProcessing matching.via
--                             ( if matching.isFinal then
--                                 ["final"]
--                               else
--                                 ["non-final"]
--                             )
--                         )
--                     )
--                 , span
--                     [ HA.class "computation-progress-to-do" ]
--                     ( datum.remainingData
--                     -- |> Debug.log "Execution-data remaining"
--                     |> List.map ViaCharacter
--                     |> List.map (\via -> viewInputProcessing via [])
--                     )
--                 ]
--           in
--             p
--               [ HA.class "output-line" ]
--               [ text <|
--                   case result of
--                     EndOfInput (Accepted _) ->
--                         "✅ Success! 🎉"
--                     EndOfInput (Rejected _) ->
--                         "❌ Rejected 😔.  The computation did not end with an accepting transition."
--                     EndOfComputation _ ->
--                         "❌ Rejected 😔.  The input was longer than the computation."
--                     CanContinue _ ->
--                         "🟢 Continuing execution."
--                     x ->
--                         "🦗 Bug!  " ++ Debug.toString x ++ ".  You should never see this message.  I need to figure out what just happened here…"
--               , Maybe.map
--                   showProgress
--                   maybeDatum
--                 |> Maybe.withDefault (text "")
--               ]
--         _ ->
--           p [] [ text "🦗 Bug! K%UGFCR" ] -- eheh?! I should never be here!
--     ]

-- viewInputProcessing : AcceptVia -> List String -> Html msg
-- viewInputProcessing via classes =
--   span
--     [ HA.classList <| List.map (\v -> (v, True)) classes ]
--     [ text <| acceptConditionToString via ]

-- viewAddTestPanelContent : Model -> Html Msg
-- viewAddTestPanelContent model =
--   let
--     test =
--       AutoDict.get model.fdg_model.package.currentTestKey model.fdg_model.package.tests
--     testInput =
--       Maybe.map .input test 
--       |> Maybe.withDefault ""
--     testExpected =
--       Maybe.map .expectation test
--       |> Maybe.withDefault ExpectAccepted
--     testExpectationElement =
--       case testInput of
--         "" ->
--           text ""
--         _ ->
--           div
--             [ HA.css
--                 [ Css.whiteSpace Css.preWrap
--                 , Css.padding4 (Css.px 0) (Css.px 0) (Css.px 15) (Css.px 15)
--                 , Css.userSelect Css.none
--                 ]
--             ]
--             [ span
--                 []
--                 [ text "When this input is received, the computation should " ]
--             -- , span
--             --     [ HA.class "button"
--             --     , HA.css
--             --         [ Css.backgroundColor <| Css.rgb 0x62 0x72 0xa4 -- --dracula-comment
--             --         ]
--             --     , onClick <| UpdateTestPanelContent testInput <|
--             --         if testExpected == ExpectAccepted then ExpectRejected else ExpectAccepted
--             --     ]
--             --     [ text "⇄"
--             --     ]
--             -- , text " "
--             , span
--                 [ HA.classList
--                     [ ("test_panel__accept-text", testExpected == ExpectAccepted)
--                     , ("test_panel__reject-text", testExpected == ExpectRejected)
--                     ]
--                 , HA.css
--                     [ Css.fontWeight Css.bold
--                     , Css.textDecorationLine Css.underline
--                     , Css.textDecorationStyle Css.dashed
--                     , Css.cursor Css.pointer
--                     , Css.color <|
--                         if testExpected == ExpectAccepted then
--                           Css.rgb 0x50 0xfa 0x7b -- --dracula-green
--                         else
--                           Css.rgb 0xff 0x79 0xc6 -- --dracula-pink
--                     ]
--                 , onClick <| UpdateTestPanelContent testInput <|
--                     if testExpected == ExpectAccepted then ExpectRejected else ExpectAccepted
--                 ]
--                 [ text
--                     ( case testExpected of
--                         ExpectAccepted -> "accept"
--                         ExpectRejected -> "reject"
--                     )
--                 ]
--             , span
--                 [ HA.css
--                     [ Css.whiteSpace Css.preWrap
--                     ]
--                 ]
--                 [ text " it." ]
--             ]
--     testContentArea =
--       textarea 
--         [ HA.class "right-bottom-panel__textarea"
--         , HA.value testInput
--         , onInput (\v -> UpdateTestPanelContent v testExpected)
--         , HA.placeholder "Enter your test input here"
--         , HA.disabled <| Maybe.Extra.isJust model.fdg_model.interactionsDict
--         ]
--         []
--   in
--     case model.executionStage of
--       Ready ->
--         div
--           [ HA.class "bottom-panel__content"]
--           [ testExpectationElement
--           , testContentArea
--           ]

--       NotReady ->
--         div 
--           [ HA.class "notready-output" ]
--           [ p [ HA.class "output-line" ] [ text "All changes must be committed or undone before you can execute." ] ]
      
--       ExecutionComplete ->
--         div 
--           [ HA.class "execution-output" ]
--           [ executionText model ]
--       StepThrough ->
--         div 
--           [ HA.class "debug-output" ]
--           [ executionText model ]

-- viewEditDescriptionPanelContent : Model -> Html Msg
-- viewEditDescriptionPanelContent model =
--   textarea 
--     [ HA.class "right-bottom-panel__textarea"
--     , HA.value (model.fdg_model.package.description |> Maybe.withDefault "")
--     , onInput UpdateDescriptionPanelContent
--     , HA.placeholder "What does this computation do?"
--     , HA.disabled <| Maybe.Extra.isJust model.fdg_model.interactionsDict
--     ]
--     []

-- viewBottomPanelContent : Model -> Html Msg
-- viewBottomPanelContent model =
--   div 
--     [ HA.class "bottom-panel__content" ]
--     [  div 
--       [ HA.class "bottom-panel__titlebar" ]
--       [ div
--         [ HA.class "bottom-panel__title" ]
--         [ text (getBottomPanelTitle model.selectedBottomPanel model.executionStage) ]
--       , div
--         [ HA.class "bottom-panel__tab-buttons" ]
--         [ div
--           [ HA.classList
--               [ ("button", True)
--               , ("tab-button", True)
--               , ("tab-button--selected", model.selectedBottomPanel == AddTestPanel)
--               ]
--           , onClick <| SelectBottomPanel AddTestPanel
--           , HA.title "Add test"
--           ]
--           [ text "🧪" ]
--         , div
--           [ HA.classList
--               [ ("button", True)
--               , ("tab-button", True)
--               , ("tab-button--selected", model.selectedBottomPanel == EditDescriptionPanel)
--               ]
--           , onClick <| SelectBottomPanel EditDescriptionPanel
--           , HA.title "Describe computation"
--           ]
--           [ text "🗃️" ]
--         ]
--       ]
--     , case model.selectedBottomPanel of
--         AddTestPanel ->
--           viewAddTestPanelContent model
--         EditDescriptionPanel ->
--           viewEditDescriptionPanelContent model
--     ]

-- getBottomPanelTitle : BottomPanel -> ExecutionStage -> String
-- getBottomPanelTitle panel state =
--   case panel of
--     AddTestPanel ->
--       case state of
--         Ready -> "Testing // Ready"
--         ExecutionComplete -> "Testing // Execution complete" -- show execution output
--         StepThrough -> "Testing // Step-through"
--         NotReady -> "Testing // Not ready"
--     EditDescriptionPanel ->
--       "Describe computation"

-- getActionButtonClass : ExecutionStage -> Msg -> String
-- getActionButtonClass currentState buttonAction =
--   let
--     baseClass = "button action-button"
--     activeClass = case (currentState, buttonAction) of
--       (ExecutionComplete, RunExecution) -> " action-button--active"
--       (StepThrough, StepThroughExecution) -> " action-button--active"
--       _ -> ""
--   in
--   baseClass ++ activeClass

-- viewStatusBar : Model -> Html Msg
-- viewStatusBar model =
--   div 
--     [ HA.class "status-bar" ]
--     [ span [] [ text (getStatusMessage model.executionStage) ]
--     , div 
--       [ HA.class "status-bar__section" ]
--       [ div
--         [ HA.classList
--             [ ("button", True)
--             , ("status-bar__button", True)
--             , ("status-bar__button--active", model.rightBottomPanelOpen)
--             ]
--         , onClick ToggleBottomPanel
--         ]
--         [ text "Terminal" ]
--       ]
--     , span 
--       [ HA.class "status-bar__section--right" ]
--       [ text ("Viewport: " ++ String.fromFloat (Tuple.first model.mainPanelDimensions) ++ " × " ++ String.fromFloat (Tuple.second model.mainPanelDimensions)) ]
--     ]

-- getStatusMessage : ExecutionStage -> String
-- getStatusMessage state =
--   case state of
--     Ready -> "Ready"
--     ExecutionComplete -> "Running..."
--     StepThrough -> "Debug Mode"
--     NotReady -> "Uncommitted"

{-
  **************************************
  Interaction → interactive capabilities
  **************************************
-}

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
            ( D.map3 (\uuid x y -> Pan uuid x y |> UIMsg)
                (D.field "uuid" Uuid.decoder)
                (D.field "x" D.float)
                (D.field "y" D.float)
            )
          >> Result.withDefault (CrashWithMessage "Invalid pan message from JavaScript")
        )
    keyboardSubscription =
      BE.onKeyDown
        ( D.map2
            (\key ctrlPressed -> ( key, ctrlPressed ))
            (D.field "key" D.string)
            (D.field "ctrlKey" D.bool)
          |> D.andThen
            (\v ->
              case v of
                ( "Escape", False ) ->
                  if model.properties.canEscape then
                    D.succeed Escape
                  else
                    D.fail "Esacpe not permitted at this point"
                _ ->
                  D.fail "Untrapped"
            )
        )

    -- keyboardSubscription =
    --   Browser.Events.onKeyDown
    --     ( D.map2
    --         (\key ctrlPressed -> ( key, ctrlPressed ))
    --         (D.field "key" D.string)
    --         (D.field "ctrlKey" D.bool)
    --       |> D.andThen
    --         (\v ->
    --           case v of
    --             ( "1", True ) ->
    --               -- Debug.log "yup" v |> \_ ->
    --               D.succeed ResetView
    --             ( "Enter", False ) ->
    --               D.succeed Confirm
    --             ( "Escape", False ) ->
    --               D.succeed Escape
    --             ( "Tab", False) ->
    --               D.succeed Reheat
    --             ( "z", True) ->
    --               D.succeed Undo
    --             ( "Z", True) ->
    --               D.succeed Undo
    --             ( "y", True) ->
    --               D.succeed Redo
    --             ( "Y", True) ->
    --               D.succeed Redo
    --             ( ch, _ ) ->
    --               case String.toList ch of
    --                 [char] ->
    --                   D.succeed (KeyPressed char)
    --                 _ ->
    --                   D.fail "Not a character key"
    --               -- in
    --               -- case model.interactionsDict of
    --               --   Just (ModifyingGraph ChooseCharacter { dest }) ->
    --               --     case dest of
    --               --       NoDestination ->
    --               --         D.fail "Not a recognized key combination"
    --               --       _ ->
    --               --         decodeChar
    --               --   Just (ModifyingGraph (ChooseGraphReference _) _) ->
    --               --     D.fail "Not choosing characters"
    --               --   Just (AlteringConnection ChooseCharacter _) ->
    --               --     decodeChar
    --               --   Just (AlteringConnection (ChooseGraphReference _) _) ->
    --               --     D.fail "Not choosing characters"
    --               --   Just (Splitting _) ->
    --               --     decodeChar
    --               --   _ ->
    --               --     D.fail "Not a recognized key combination"
    --         )
    --     )
    --   else
    --     Sub.none
    coordinateSubscription =
      receiveCoordinates
        ( D.decodeValue
            (D.map3 (\uuid x y -> ReceiveCoordinates uuid (x, y) |> UIMsg)
              (D.field "uuid" Uuid.decoder)
              (D.field "x" D.float)
              (D.field "y" D.float)
            )
          >> Result.withDefault (CrashWithMessage "Invalid coordinates message from JavaScript")
          -- >> Debug.log "Received coordinates"
        )
    resizeSubscription =
      BE.onResize (\w h -> UIMsg <| OnResize (toFloat w, toFloat h) {- |> Debug.log "Raw resize values" -})
    splitterSubscriptions =
      case peekInteraction Nothing model of
        Just (DraggingSplitter LeftRight) ->
          let
            -- navigatorbarwidth + splitterwidth/2
            offset = -(48 + 8 / 2)
          in
            [ BE.onMouseMove (D.map ((+) offset >> DragSplitter False >> UIMsg) (D.field "clientX" D.float))
            , BE.onMouseUp (D.map ((+) offset >> DragSplitter True >> UIMsg) (D.field "clientX" D.float))
            ]
        Just (DraggingSplitter UpDown) ->
          [ BE.onMouseMove (D.map (DragSplitter False >> UIMsg) (D.field "clientY" D.float))
          , BE.onMouseUp (D.map (DragSplitter True >> UIMsg) (D.field "clientY" D.float))
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
                      MovePhantomNode
                        uuid
                        (graph_view |> translateHostCoordinates (x, y))
                      |> UIMsg
                    )
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                )
            )
      in
        AutoDict.toList model.interactionsDict
        |> List.filterMap
          (\(maybeUuid, (_, stack)) ->
            case (maybeUuid, stack) of
              (Just uuid, ChoosingDestinationFor _ _ :: _) ->
                createNodeMoveSubscription uuid
              _ ->
                Nothing
          )
  in
    Sub.batch
      ( resizeSubscription ::
        panSubscription ::
        coordinateSubscription ::
        keyboardSubscription ::
        nodeMoveSubscriptions ++
        splitterSubscriptions
      )
  -- Sub.batch
  --   [ -- , keyboardSubscription
  --   -- , panSubscription
  --     currentTimeSubscription
  --   , Browser.Events.onAnimationFrame (always Tick)
  --   , BE.onResize (\w h -> OnResize (toFloat w, toFloat h) {- |> Debug.log "Raw resize values" -})
  --   , if model.isDraggingHorizontalSplitter || model.isDraggingVerticalSplitter then
  --       Sub.batch
  --         [ BE.onMouseMove (D.map2 MouseMove (D.field "clientX" D.float) (D.field "clientY" D.float))
  --         , BE.onMouseUp (D.succeed StopDragging)
  --         ]
  --     else
        -- Sub.none
    -- ]













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

-- viewPackageItem : Model -> GraphPackage -> Html Msg
-- viewPackageItem model package =
--   let
--     displaySvg =
--       FDG.viewComputationThumbnail (model.leftPanelWidth - 15) model.fdg_model package
--     canSelect =
--       -- we can select this EITHER if there are no pending changes, OR
--       -- if this is the currently-loaded package (i.e. to "reset"/"refresh" it)
--       model.fdg_model.package.undoBuffer == [] ||
--       package.userGraph.graphIdentifier == model.fdg_model.package.userGraph.graphIdentifier
--   in
--   div 
--     [ HA.classList
--         [ ("left-panel__packageItem", True)
--         , ("left-panel__packageItem--disabled", not canSelect)
--         ]
--     , HA.css
--         [ Css.position Css.relative
--         , Css.borderRadius (Css.px 5)
--         , Css.borderWidth (Css.px 1)
--         , Css.borderColor (Css.rgb 0x28 0x2a 0x36) -- --dracula-background
--         , Css.borderStyle Css.solid
--         , Css.userSelect Css.none
--         , if canSelect then
--             Css.cursor Css.pointer
--           else
--             Css.cursor Css.notAllowed
--         ]
--     , if canSelect then
--         onClick (SelectPackage package.userGraph.graphIdentifier)
--       else
--         HA.title "Apply or cancel the pending changes before selecting another package."
--     ]
--     [ Html.Styled.fromUnstyled <| Html.map ForceDirectedMsg displaySvg
--     , div
--         [ HA.css
--             [ Css.position Css.absolute
--             , Css.right (Css.px 5)
--             , Css.top (Css.px 0)
--             ]
--         , HA.classList
--             [ ("button", canSelect)
--             , ("button--disabled", not canSelect)
--             ]
--         , HA.title "Delete computation"
--         , if canSelect then
--             onClick (DeletePackage package.userGraph.graphIdentifier)
--           else
--             HA.title "Apply or cancel the pending changes before deleting a package."
--         ]
--         [ text "🚮" ]
--     ]


viewNavigatorsArea : Model -> Html Msg
viewNavigatorsArea model =
  div
    [ HA.class "sidebar-container" ]
    [ if not model.uiState.open.sideBar then
        div [] []
      else
        div
          [ HA.class "navigation-bar" ]
          [ button
              [ HA.classList
                  [ ("navigation-icon", True)
                  , ("active", model.uiState.selected.sideBar == ComputationsIcon)
                  ]
              , HA.title "Computations"
              , (model.uiState.selected.sideBar /= ComputationsIcon)
                |> thenPermitInteraction (HE.onClick (UIMsg <| SelectNavigation ComputationsIcon))
              ]
              [ text "📁"]
          , button
              [ HA.classList
                  [ ("navigation-icon", True)
                  , ("active", model.uiState.selected.sideBar == TestsIcon)
                  ]
              , HA.title "Tests"
              , (model.uiState.selected.sideBar /= TestsIcon)
                |> thenPermitInteraction (HE.onClick (UIMsg <| SelectNavigation TestsIcon))
              ]
              [ text "🧪" ]
          ]
    , if not model.uiState.open.sideBar then
        div [] []
      else
        div
          [ HA.class "sidebar"
          , HA.css
              [ Css.width <| Css.px <|
                  if model.uiState.open.sideBar then
                    Tuple.first model.uiState.dimensions.sideBar
                  else
                    0
              ]
          ]
          [ debugDimensions model.uiState.dimensions.sideBar
          , case model.uiState.selected.sideBar of
              ComputationsIcon ->
                viewComputationsSidebar model
              TestsIcon ->
                div
                    [ HA.class "sidebar-content" ]
                    [ text "Hello world B" ]
          ]
    ]

viewComputationsSidebar : Model -> Html Msg
viewComputationsSidebar model =
  div
    [ HA.class "sidebar-content computations" ]
    [ h1
        [ HA.class "sidebar-title" ]
        [ text "Computations "
        , button
            [ HA.class "add-button"
            , HA.title "Create new computation"
            ]
            [ text "➕" ]
        ]
    , div
        [ HA.class "computations-explorer" ]
        ( List.filterMap
            (\uuid ->
              let
                isMain = Uuid.toString uuid == Uuid.toString model.mainGraphView
              in
                AutoDict.get uuid model.graph_views
                |> Maybe.map
                  (\graph_view ->
                    div
                      [ HA.class "package"
                      , HE.onClick (UIMsg <| SelectPackage graph_view.package.userGraph.graphIdentifier)
                      ]
                      [ viewGraph graph_view
                      , div
                          [ HA.class "description" ]
                          [ graph_view.package.description
                            |> Maybe.withDefault "(no description)"
                            |> text
                          ]
  --         , HA.title "Delete computation"
  --         , if canSelect then
  --             onClick (DeletePackage package.userGraph.graphIdentifier)
  --           else
  --             HA.title "Apply or cancel the pending changes before deleting a package."
  --         ]
  --         [ text "🚮" ]
                      , if isMain then
                          text ""
                        else
                          div
                            [ HA.class "delete-button"
                            , HA.title "Delete"
                            -- , HE.onClick (DeletePackage graph_view.package.userGraph.graphIdentifier)
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
      , HE.onClick (UIMsg <| ToggleAreaVisibility targetArea)
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
  in
    div
      [ HA.classList
          [ ("splitter-separator " ++ movementClass, True)
          , ("dragging", model.properties.dragDirection == Just movement)
          , ( "draggable", model.properties.canDragSplitter && areaOpen)
          ]
      , HA.css
          [ Css.zIndex (Css.int zIdx)
          ]
      , HE.onMouseDown (UIMsg <| StartDraggingSplitter movement)
      ]
      [ div
          [ HA.class <| "separator-handle " ++ movementClass ]
          [ button
              [ HA.class <| "collapse-button " ++ movementClass
              , HA.title "Collapse"
              , HE.onClick (UIMsg <| ToggleAreaVisibility targetArea)
              ]
              [ collapseIcon ]
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
                , ("active", model.uiState.selected.bottomPanel == TestingToolIcon)
                ]
            , HA.title "Testing"
            , (model.uiState.selected.bottomPanel /= TestingToolIcon)
              |> thenPermitInteraction (HE.onClick (UIMsg <| SelectTool TestingToolIcon))
            ]
            [ text "🔬"]
        , button
            [ HA.classList
                [ ("tool-icon", True)
                , ("active", model.uiState.selected.bottomPanel == MetadataToolIcon)
                ]
            , HA.title "Tests"
            , (model.uiState.selected.bottomPanel /= MetadataToolIcon)
              |> thenPermitInteraction (HE.onClick (UIMsg <| SelectTool MetadataToolIcon))
            ]
            [ text "📝" ]
        ]
    , div
        [ HA.class "tools-area"
        , HA.css
            [ Css.height <| Css.px <|
                if model.uiState.open.bottomPanel then
                  Tuple.second model.uiState.dimensions.bottomPanel
                else
                  0
            ]
        ]
        [ debugDimensions model.uiState.dimensions.bottomPanel
        , case model.uiState.selected.bottomPanel of
            MetadataToolIcon ->
              div
                [ HA.class "tool-content" ]
                [ text "Hello world A" ]
            TestingToolIcon ->
              div
                [ HA.class "tool-content" ]
                [ text "Hello world B" ]
        ]
    ]

viewMainInterface : Model -> Html Msg
viewMainInterface model =
  div
    [ HA.class "editor-frame" ]
    [ viewNavigatorsArea model
    , if model.uiState.open.sideBar then
        viewSplitter
          5 LeftRight
          model
          (model.uiState.open.sideBar)
      else
        div [] []
    , div
        [ HA.class "editor-and-tools-panel" ]
        [ div
            [ HA.class "editor-main"
            , HA.css
                [ Css.maxWidth <| px <| Tuple.first model.uiState.dimensions.mainEditor
                , Css.maxHeight <| px <| Tuple.second model.uiState.dimensions.mainEditor
                ]
            ]
            [ debugDimensions model.uiState.dimensions.mainEditor
            , AutoDict.get model.mainGraphView model.graph_views
              |> Maybe.map (GraphEditor.viewGraph)
              |> Maybe.withDefault
                (div [ HA.class "error graph-not-loaded" ] [ text "⚠ Graph to load was not found!" ]) -- erk! say what now?!
            ]
        , viewSplitter
            4 UpDown
            model
            model.uiState.open.bottomPanel
        , viewToolsArea model
        ]
    -- we have these down here so that they WILL sit on top of the panning-region bars for the
    -- main area.
    , if not model.uiState.open.sideBar then
        viewCollapsedAreaButton LeftRight
      else
        text ""
    , if not model.uiState.open.bottomPanel then
        viewCollapsedAreaButton UpDown
      else
        text ""
    ]

viewConnectionEditor : Model -> Uuid -> ConnectionAlteration -> ConnectionEditorProperties -> Html Msg
viewConnectionEditor model uuid {source, dest, connection} editorData =
  let
    (terminal, nonTerminal) =
      AutoSet.partition (.isFinal) connection
    htmlForTransition {via} =
      case via of
        ViaCharacter c ->
          Just
            ( span
                [ HA.class "set-item character" ]
                [ text <| String.fromChar c ]
            )
        ViaGraphReference pkg_uuid ->
          AutoDict.get pkg_uuid model.packages
          |> Maybe.map (\pkg ->
            span
              [ HA.class "set-item graph-reference" ]
              [ div
                  [ HA.class "package-badge"
                  , HA.title "This unique badge identifies this specific computation."
                  ]
                  [ TypedSvg.svg
                      [ TypedSvg.Attributes.viewBox 0 0 30 18 ]
                      [ GraphEditor.viewGraphReference pkg_uuid 4 0 0 ]
                    |> Html.Styled.fromUnstyled
                  ]
              , case pkg.description of
                  Just s ->
                    div
                      [ HA.class "description" ]
                      [ text s ]
                  Nothing ->
                    text ""
              ]
              -- [ text <| Maybe.withDefault "(no description)" pkg.description ]
          )
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
                            [ HA.class "set-left-bracket" ]
                            [ text "{"
                            ]
                        , div -- terminal items group
                            [ HA.class "terminals" ]
                            ( AutoSet.toList terminal
                              |> List.filterMap htmlForTransition
                            )
                        , div
                            [ HA.class "set-separator" ]
                            []
                        , div -- normal (non-terminal) items group
                            [ HA.class "non-terminals" ]
                            ( AutoSet.toList nonTerminal
                              |> List.filterMap htmlForTransition
                            )
                        , div
                            [ HA.class "set-right-bracket" ]
                            [ text "}"
                            ]
                        ]
                    ]
                , div -- quick input
                    [ HA.class "quick-input" ]
                    [ div
                        [ HA.class "quick-input-bar"
                        , HE.onInput (UIMsg << ConnectionEditorInput)
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
                        |> Maybe.map
                          (\graph_view ->
                            let
                              via = ViaGraphReference graph_view.package.userGraph.graphIdentifier
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
                                , HE.onClick (UIMsg <| ToggleConnectionTransition via)
                                ]
                                [ viewGraph graph_view
                                , div
                                    [ HA.class "description" ]
                                    [ graph_view.package.description
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
                                          [ GraphEditor.viewGraphReference graph_view.package.userGraph.graphIdentifier 4 0 0 ]
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

view : Model -> Html Msg
view model =
  div
    []
    [ case mostRecentInteraction model of
        Just (Just uuid, EditingConnection alteration props) ->
          viewConnectionEditor model uuid alteration props
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