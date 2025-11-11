module Main exposing (..)
import Browser
import Browser.Events as BE
import Html.Styled exposing
  (Html, div, h3, p, ul, li, input, textarea, span, toUnstyled, text, button)
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
import Ports
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
import List.Extra

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
                  Just (ModifyConnection (CreatingNewNode {dest})) ->
                    maybe_fromBool
                      (node.id == dest)
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

identifyCardinality : NodeId -> Graph.NodeContext Entity Connection -> Cardinality
identifyCardinality from to =
  if to.node.id == from then
    Recursive
  else if linkExistsInGraph to from then
    Bidirectional
  else
    Unidirectional

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
            identifyCardinality sourceNode.node.id destNode
        in
          ( (sourceNode.node.id, destNode.node.id)
          , { cardinality = cardinality
            , executionData =
                { executed = False
                , smallest_recency = -1
                , chosen = AutoDict.empty acceptConditionToString
                }
            , label = label
            , pathBetween =
                GraphEditor.path_between
                  sourceNode.node.label
                  destNode.node.label
                  cardinality
                  7 5
            , isPhantom = False
            }
          )
    )
  |> Dict.fromList
  

{-| Creates a new GraphView from a GraphPackage within the `packages`
    dictionary, and adds it to the `graph_views` dictionary in the `Model`.

    If there is no such `GraphPackage`, then nothing is done and no
    `GraphView` is returned.
-}
viewFromPackage : (Float, Float) -> (Float, Float) -> Bool -> Uuid -> Model -> (Maybe GraphView, Model)
viewFromPackage (w, h) (x, y) createFrozen package_uuid model =
  let
    (id, model_) = getUuid model
  in
    AutoDict.get package_uuid model.packages
    |> Maybe.map
      (\pkg ->
        let
          computed : GraphEditor.ComputeGraphResult
          computed = GraphEditor.computeGraphFully (w, h) pkg.userGraph
          -- now that we have the positions, calculate the dimensions of
          -- the viewport.
          -- first, let's get the aspect ratio.  That will let us figure out
          -- the height of the viewport "automatically" once we have the right width.
          solved_pkg =
            { pkg | userGraph = computed.solvedGraph }
          guest_viewport =
            calculateGuestDimensionsForHost
              (w, h)
              createFrozen
              solved_pkg.userGraph.graph
          properties = defaultViewProperties createFrozen
          graph_view : GraphView
          graph_view =
            { id = id
            , isFrozen = createFrozen
            , package = solved_pkg
            , simulation = computed.simulation
            , host_dimensions = (w, h)
            , host_coordinates = (x, y)
            , guest_dimensions = guest_viewport.dimensions
            , guest_coordinates = guest_viewport.coordinates
            , guest_center_coordinates = guest_viewport.center_coordinates
            , guest_center_dimensions = guest_viewport.center_dimensions
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
          ( Just graph_view
          , { model_
              | graph_views =
                  AutoDict.insert id graph_view model_.graph_views
            }
          )
      )
    |> Maybe.withDefault
      (Nothing, model)

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
                  (Automata.Data.empty mainUuid)
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
          { min = 50
          , max = decoded.width / 2 - 60
          , initial = clamp 50 (decoded.width / 2 - 60) 300
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
      }
    model =
      -- I _know_ that this will succeed, because I've added this
      -- exact one
      let
        (v, model_with_viewDict) =
          viewFromPackage
            state.dimensions.mainEditor
            ( if state.open.sideBar then
                Tuple.first state.dimensions.sideBar + 48 + 8
              else
                0
            , 0
            )
            False
            mainPackage.userGraph.graphIdentifier
            model_excl_views
      in
        case v of
          Nothing ->
            Debug.log "🚨 FAILURE! GraphView could not be created. `mainGraphView` WILL be incorrect!"
            model_with_viewDict
          Just v_ ->
            { model_with_viewDict | mainGraphView = v_.id }
  in
    ( model , Cmd.none )

-- UPDATE

{-| Note: EVERYWHERE that I use persistPackage, I should ALSO
    update the `packages` dictionary!

    (yes, I should one day figure out an automagic way to do this…
     maybe. but it's in so few places right now that hey, YAGNI?)
-}
persistPackage : GraphPackage -> Cmd Msg
persistPackage =
  Ports.saveToStorage << encodeGraphPackage

-- updateGraphViewsOnTick : Model -> Model
-- updateGraphViewsOnTick model =
--   let
--     updateView : GraphView -> GraphView
--     updateView v =
--       let
--         props = v.properties
--       in
--         if props.tickCount <= 300 && not props.isFrozen then
--           let
--             ( newSimulationState, list ) =
--                 Graph.nodes v.package.userGraph.graph
--                 |> List.map .label
--                 |> Force.tick v.simulation
--             pkg : GraphPackage
--             pkg = v.package
--             g : AutomatonGraph
--             g = pkg.userGraph
--             newGraph =
--               updateGraphWithList g.graph list
--           in
--           { v
--             | properties =
--                 { props | tickCount = props.tickCount + 1 }
--             , simulation = newSimulationState
--             , package =
--                 { pkg
--                   | userGraph =
--                       { g | graph = newGraph }
--                 }
--           }
--         else
--           v
--   in
--   { model
--     | graph_views =
--         AutoDict.map
--           (\_ -> updateView)
--           model.graph_views
--   }

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
    ViewportUpdated dim ->
      { model
        | graph_views =
            updateGraphView uuid
              (Just << resizeGraphView dim)
              model.graph_views
      }
    NodeDragStart nodeId ->
      { model
      | interactionsDict = Just <| Dragging nodeId
      -- , simulation = Force.reheat model.simulation
      }

    NodeDragEnd ->
      case model.interactionsDict of
        Just (Dragging nodeId) ->
          let
            ( x, y ) =
              mapCorrespondingPair (+) model.pan model.mouseCoords
            nearby =
              nearby_nodes nearby_node_repulsionDistance model
              -- |> Debug.log "Nearby nodes at end of drag"
            sf =
              IntDict.insert nodeId
                [ Force.towardsX [{ node = nodeId, strength = 2, target = x }]
                , Force.towardsY [{ node = nodeId, strength = 2, target = y }]
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
            pkg = model.package
            g = pkg.userGraph
            newGraph =
              Graph.update nodeId
                ( Maybe.map (updateNode (x,y) model.pan) )
                g.graph
          in
            { model
              | interactionsDict = Nothing
              , currentPackage =
                  { pkg
                    | userGraph =
                        { g | graph = newGraph }
                        |> debugAutomatonGraphXY ("Dragging #" ++ String.fromInt nodeId ++ " ended with updated node")
                  }
              , specificForces = sf
              , simulation = Force.simulation (List.concat (IntDict.values sf))
            }

        _ ->
          model

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

    MouseMove x y ->
      let
        pkg = model.package
        ug = pkg.userGraph
        newGraph =
          case model.interactionsDict of
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
                              mapCorrespondingPair (+) model.pan ( x, y )
                          in
                            { ctx
                            | node =
                              { node
                              | label = l |> setXY node_x node_y
                              }
                            }
                        )
                      )
                      ug.graph
              }
            _ ->
              ug
      in
        { model
          | mouseCoords = (x, y) -- |> Debug.log "Set mouse-coords"
          , currentPackage = { pkg | userGraph = newGraph }
        }

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
    
    Pan xAmount yAmount ->
      let
        ( xPan, yPan ) = model.pan
      in
        { model
        | pan =
            case model.interactionsDict of
              Just (ModifyingGraph _ { dest }) ->
                case dest of
                  NoDestination ->
                    ( xPan + xAmount, yPan + yAmount )
                  _ ->
                    model.pan
              Nothing ->
                ( xPan + xAmount, yPan + yAmount )
              Just (Dragging _) ->
                ( xPan + xAmount, yPan + yAmount )
              Just (AlteringConnection _ _) ->
                model.pan
              Just (Splitting _) ->
                model.pan
              Just (Executing _ _) ->
                ( xPan + xAmount, yPan + yAmount )
        }

    SelectNode index ->
      { model | interactionsDict = Just <| ModifyingGraph ChooseCharacter <| GraphModification index NoDestination (AutoSet.empty transitionToString) }

    SetMouseOver ->
      { model | mouseIsHere = True }

    SetMouseOut ->
      { model | mouseIsHere = False }

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

    EditTransition src dest conn ->
      { model
        | interactionsDict =
            Just <| AlteringConnection ChooseCharacter (ConnectionAlteration src dest conn)
      }

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

    Undo ->
      case (model.interactionsDict, model.package.undoBuffer) of
        (_, []) ->
          model
        (Just _, _) ->
          model -- do not permit undo/redo while I'm performing any operation.
        (Nothing, h::t) ->
          let
            pkg = model.package
          in
            { model
              | currentPackage =
                  { pkg
                    | undoBuffer = t
                    , redoBuffer = model.package.userGraph :: model.package.redoBuffer
                    , userGraph = h
                  }
              , disconnectedNodes = identifyDisconnectedNodes h
            }

    Redo ->
      case (model.interactionsDict, model.package.redoBuffer) of
        (_, []) ->
          model
        (Just _, _) ->
          model -- do not permit undo/redo while I'm performing any operation.
        (Nothing, h::t) ->
          let
            pkg = model.package
          in
          { model
            | currentPackage =
                { pkg
                  | redoBuffer = t
                  , undoBuffer = model.package.userGraph :: model.package.undoBuffer
                  , userGraph = h
                }
            , disconnectedNodes = identifyDisconnectedNodes h
          }
        
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
  , center_coordinates : (Float, Float)
  , center_dimensions : (Float, Float)
  }
{-| Accepts host dimensions, view properties, and a graph, and calculates the
    appropriate guest coordinates & guest dimensions (i.e. viewport).
-}
calculateGuestDimensionsForHost : (Float, Float) -> Bool -> Graph.Graph Entity Connection -> GuestDimensions
calculateGuestDimensionsForHost (w, h) isFrozen graph =
  let
    aspectRatio : Float
    aspectRatio = w / h
    inner_pad : Float -- 85-105 in SVG-coordinates seems to be a "good" amount of space
    inner_pad =
      if isFrozen then
        -- we don't need any buffer.
        -- So, just put in a "buffer" for the node radius.
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
    ( (min_x, max_x), (min_y, max_y) ) =
      ( (min_x_raw - inner_pad, max_x_raw + inner_pad)
      , (min_y_raw - inner_pad, max_y_raw + inner_pad))
      -- |> Debug.log "Bounds"
    -- from this, I can figure out the appropriate coordinates
    center_y = (min_y + max_y) / 2
    center_x = (min_x + max_x) / 2
    autoHeight = (max_x - min_x) / aspectRatio
    -- now, we want a center within that autoHeight.
    guestCoordinates = ( min_x, center_y - autoHeight / 2 )
    guestDimensions = ( max_x - min_x, autoHeight )
    centerCoordinates =
      ( clamp min_x center_x (min_x + 80)
      , clamp min_y center_y (min_y + 80)
      )
    centerDimensions =
      ( max_x_raw - 80 - min_x_raw
      , max_y_raw - 80 - min_y_raw)
  in
    { dimensions = guestDimensions
    , coordinates = guestCoordinates
    , center_coordinates = centerCoordinates
    , center_dimensions = centerDimensions
    }

updateMainEditorDimensions : Model -> Model
updateMainEditorDimensions ({uiState} as model) =
  -- On resize, I must update the dimensions for the graph view that
  -- is displayed in the "main" editor section as well.
  { model
    | graph_views =
        AutoDict.update model.mainGraphView
          (Maybe.map
            (\graph_view ->
              let
                guest_viewport =
                  calculateGuestDimensionsForHost
                    uiState.dimensions.mainEditor
                    graph_view.isFrozen
                    graph_view.package.userGraph.graph
              in
                { graph_view
                  | host_dimensions = uiState.dimensions.mainEditor
                  , guest_dimensions = guest_viewport.dimensions
                  , guest_coordinates = guest_viewport.coordinates
                  , host_coordinates =
                      if uiState.open.sideBar then
                        (Tuple.first uiState.dimensions.sideBar + 8 + 48, 0)
                      else
                        (0, 0)
                }
            )
          >> Maybe.orElseLazy (\() -> Debug.todo "PUNT: there is no main graph view?!!")
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
        clamp constants.toolsPanelHeight.min constants.toolsPanelHeight.max (Automata.Data.height dimensions.viewport - 8 - coord)
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
      |> List.Extra.maximumBy Tuple.first
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
      Just <|
        ( h
        , { model | interactionsDict = AutoDict.remove uuid model.interactionsDict }
        )
    Just (r, h :: t) ->
      -- if it has something, pop, and also return an updated model
      Just <|
        ( h
        , { model | interactionsDict = AutoDict.insert uuid (r - 1, t) model.interactionsDict }
        )
    _ ->
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

mostRecentInteraction : Model -> Maybe (Maybe Uuid, InteractionState)
mostRecentInteraction model =
  AutoDict.toList model.interactionsDict
  |> List.Extra.maximumBy (Tuple.second >> Tuple.first)
  |> Maybe.andThen
    (\(uuid, (_, interaction)) ->
      case interaction of
        [] -> Nothing
        h::_ -> Just (uuid, h)
    )

update_ui : UIMsg -> Model -> ( Model, Cmd Msg)
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
              model_ |> updateMainEditorDimensions
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
    SelectNavigation item ->
      Debug.todo "branch 'SelectNavigationItem item' not implemented"

    SelectTool _ ->
      Debug.todo "branch 'SelectTool _' not implemented"

    MouseMove _ _ ->

      Debug.todo "branch 'MouseMove _ _' not implemented"

    OnResize dims ->
      ( resizeViewport dims model
      , Cmd.none
      )

selectNodeInView : Model -> Uuid -> NodeId -> (Float, Float) -> Model
selectNodeInView model view_uuid node_id coordinates =
  AutoDict.get view_uuid model.graph_views
  |> Maybe.map
    (\graph_view ->
      let
        newNodeId : NodeId
        newNodeId =
          Graph.nodeIdRange graph_view.package.userGraph.graph
          |> Maybe.map (Tuple.second >> (+) 1)
          |> Maybe.withDefault 0
        nodeDrawingData : NodeDrawingData
        nodeDrawingData =
          { exclusiveAttributes = Just DrawPhantom
          , isTerminal = True
          , isDisconnected = False
          , coordinates = coordinates
          , isRoot = False
          , canSplit = False
          , view_uuid = view_uuid
          , isSelected = False
          }
        linkDrawingData : LinkDrawingData
        linkDrawingData =
          { cardinality = Unidirectional
          , pathBetween =
              { pathString = ""
              , transition_coordinates = { x = 0, y = 0 }
              , length = 0.1
              , control_point = { x = 0, y = 0 }
              , source_connection_point = { x = 0, y = 0 }
              , target_connection_point = { x = 0, y = 0 }
              }
          , executionData =
              { executed = False
              , smallest_recency = -1
              , chosen = AutoDict.empty acceptConditionToString
              }
          , label = AutoSet.empty transitionToString
          , isPhantom = True
          }
        interaction =
          ModifyConnection <|
            CreatingNewNode
              { source = node_id
              , dest = newNodeId
              , connection = AutoSet.empty transitionToString
              , picker = ChooseCharacter
              }
        updateDrawingData drawingData =
          -- update drawing data to show a new "phantom" node
          -- and a new "phantom" link leading to it
          { drawingData
            | node_drawing =
                Dict.insert newNodeId
                  nodeDrawingData
                  drawingData.node_drawing
                |> Dict.update node_id
                  (Maybe.map
                    (\source_node ->
                        { source_node | isSelected = True }
                    )
                  )
            , link_drawing =
                Dict.insert (node_id, newNodeId)
                  linkDrawingData
                  drawingData.link_drawing
          }
      in
        pushInteractionForStack (Just view_uuid) interaction model
          -- and now modify the drawing-data for that view
        |> (\model_ ->
              { model_
                | graph_views =
                    AutoDict.insert view_uuid
                      { graph_view
                        | drawingData = updateDrawingData graph_view.drawingData
                          , properties = -- 🔴🔴🔴🔴🔴🔴 TODO 🔴🔴🔴🔴🔴🔴 update this when I shift to something centralized
                              let properties = graph_view.properties in
                              { properties
                                | canSelectConnections = False
                                , canSelectEmptySpace = True
                                , canSplitNodes = False
                              }
                      }
                      model_.graph_views
              }
          )
    )
  |> Maybe.withDefault model

moveNode : NodeId -> NodeId -> (Float, Float) -> GraphView -> GraphView
moveNode source_id dest_id (x, y) graph_view =
  { graph_view
    | drawingData =
        let
          drawingData = graph_view.drawingData
          cardinality =
            if source_id == dest_id then
              Recursive
            else
              let
                maybeDest =
                  Graph.get dest_id graph_view.package.userGraph.graph
              in
                case maybeDest of
                  Nothing ->
                    Unidirectional
                  Just dest ->
                    if linkExistsInGraph dest source_id then
                      Bidirectional
                    else
                      Unidirectional
        in
        { drawingData
          | node_drawing =
              Dict.update dest_id
                (Maybe.map
                  (\phantom_node ->
                      { phantom_node | coordinates = (x, y) }
                  )
                )
                drawingData.node_drawing
          , link_drawing =
              let
                sourceNode =
                  Graph.get source_id graph_view.package.userGraph.graph
              in
                Dict.update (source_id, dest_id)
                  (Maybe.map
                    (\phantom_link ->
                        { phantom_link
                          | pathBetween =
                              sourceNode
                              |> Maybe.map
                                (\src ->
                                  path_between
                                    src.node.label
                                    { x = x, y = y }
                                    cardinality
                                    7 5
                                )
                              |> Maybe.withDefault phantom_link.pathBetween                                
                        }
                    )
                  )
                  drawingData.link_drawing
        }
  }

cancelNewNodeCreation : Uuid -> NodeId -> NodeId -> Model -> Model
cancelNewNodeCreation view_uuid source dest model =
  let
    updateDrawingData drawingData =
      { drawingData
        | node_drawing =
            Dict.remove dest drawingData.node_drawing
        , link_drawing =
            Dict.remove (source, dest) drawingData.link_drawing
      }
  in
  case popInteraction (Just view_uuid) model of
    Just (ModifyConnection (CreatingNewNode _), model_) ->
      { model_
        | graph_views =
            AutoDict.update view_uuid
              (Maybe.map (\graph_view ->
                { graph_view
                  | drawingData =
                      updateDrawingData graph_view.drawingData
                  , properties = -- 🔴🔴🔴🔴🔴🔴 TODO 🔴🔴🔴🔴🔴🔴 Centralize when I get to that step!
                      let properties = graph_view.properties in
                      { properties
                        | canSelectConnections = True
                      }
                }
              ))
              model_.graph_views
      }
    _ ->
      model


editConnection : Model -> Uuid -> ConnectionAlteration -> NodeId -> Model
editConnection model view_uuid old_alteration new_dest =
  let
    editConnectionInView : GraphView -> Model
    editConnectionInView graph_view =
      let
        viewWithDestination : GraphView
        viewWithDestination = -- ensure that the destination node exists in the graph.
          { graph_view
            | package =
                let package = graph_view.package in
                { package
                  | userGraph =
                    let userGraph = package.userGraph in
                    { userGraph
                      | graph =
                          Graph.update new_dest
                            ( Maybe.map identity
                              >> Maybe.orElseLazy
                                (\() ->
                                  Just <|
                                    { node = { id = new_dest, label = entity new_dest NoEffect }
                                    , incoming = IntDict.empty
                                    , outgoing = IntDict.empty
                                    }
                                )
                            )
                            userGraph.graph
                    }
                }
          }
        interaction : InteractionState
        interaction = -- this is the connection between the two nodes
          -- if there is no such view, then the interaction is Nothing.
          ModifyConnection <|
            EditExistingConnection
              { old_alteration
                | dest = new_dest
                , connection =
                    Graph.get new_dest viewWithDestination.package.userGraph.graph
                      |> Maybe.map (\node ->
                        IntDict.get old_alteration.source node.incoming
                        |> Maybe.withDefault (AutoSet.empty transitionToString)
                      )
                    -- if the new_dest doesn't exist in the graph, then maybe it is
                    -- a totally to-be-created new-node.  So let's put it as an
                    -- empty connection for now.  
                    |> Maybe.withDefault (AutoSet.empty transitionToString)
              }
      in
        cancelNewNodeCreation view_uuid old_alteration.source old_alteration.dest model
        |> pushInteractionForStack (Just view_uuid) interaction
        |>  (\model_ ->
              { model_
                | -- and now we will enter the editing-connection phase, so
                  graph_views =
                    AutoDict.update view_uuid
                      (Maybe.map (\gv ->
                        { gv
                          | properties =
                              let properties = gv.properties in
                              { properties
                                | canSelectConnections = False
                                , canSelectEmptySpace = False
                                , canSelectNodes = False
                                , canSplitNodes = False
                              }
                        }
                      ))
                      model_.graph_views
              }
            )

  in
    AutoDict.get view_uuid model.graph_views
    |> Maybe.map (editConnectionInView)
    |> Maybe.withDefault model

defaultViewProperties : Bool -> GraphViewProperties
defaultViewProperties isFrozen =
  { canSelectConnections = not isFrozen
  , canSelectEmptySpace = False
  , canSelectNodes = not isFrozen
  , canSplitNodes = not isFrozen
  , canDragNodes = not isFrozen
  , canInspectRefs = True
  , canPan = True
  }

defaultMainProperties : MainUIProperties
defaultMainProperties =
  { canEscape = False
  , canDragSplitter = True
  , canAcceptCharacters = False
  }

setProperties : Model -> Model
setProperties model =
  let
    setLocalProperties f a = (Tuple.first f) a
    setMainProperties f = (Tuple.second f)
    default : ((Bool -> GraphViewProperties), MainUIProperties)
    default = ( defaultViewProperties, defaultMainProperties )
    whenSplittingNode : ((Bool -> GraphViewProperties), MainUIProperties)
    whenSplittingNode =
      ( \isFrozen ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = not isFrozen
          , canInspectRefs = True
          , canPan = True
          }
      , { canEscape = True
        , canDragSplitter = True
        , canAcceptCharacters = False
        }
      )
    whenDraggingNode : ((Bool -> GraphViewProperties), MainUIProperties)
    whenDraggingNode =
      ( \isFrozen ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = False
          , canInspectRefs = False
          , canPan = True
          }
      , { canEscape = True
        , canDragSplitter = False
        , canAcceptCharacters = False
        }
      )
    whenDraggingSplitter : ((Bool -> GraphViewProperties), MainUIProperties)
    whenDraggingSplitter =
      ( \isFrozen ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = False
          , canInspectRefs = False
          , canPan = False
          }
      , { canEscape = False
        , canDragSplitter = False
        , canAcceptCharacters = False
        }
      )
    whenSourceNodeSelected : ((Bool -> GraphViewProperties), MainUIProperties)
    whenSourceNodeSelected =
      ( \isFrozen ->
          { canSelectConnections = False
          , canSelectEmptySpace = True
          , canSelectNodes = True
          , canSplitNodes = True
          , canDragNodes = True
          , canInspectRefs = False
          , canPan = True
          }
      , { canEscape = True
        , canDragSplitter = True
        , canAcceptCharacters = False
        }
      )
    whenEditingConnection : ((Bool -> GraphViewProperties), MainUIProperties)
    whenEditingConnection =
      ( \isFrozen ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = True
          , canInspectRefs = True
          , canPan = True
          }
      , { canEscape = True
        , canDragSplitter = False
        , canAcceptCharacters = True
        }
      )
    whenExecuting : ((Bool -> GraphViewProperties), MainUIProperties)
    whenExecuting =
      ( \isFrozen ->
          { canSelectConnections = False
          , canSelectEmptySpace = False
          , canSelectNodes = False
          , canSplitNodes = False
          , canDragNodes = False
          , canInspectRefs = True
          , canPan = True
          }
      , { canEscape = True
        , canDragSplitter = True
        , canAcceptCharacters = False
        }
      )
    mainProperties_base =
      case peekInteraction (Nothing) model of
        Just (DraggingSplitter _) ->
          setMainProperties whenDraggingSplitter
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
                        setLocalProperties default v.isFrozen
                      Just (SplittingNode _) ->
                        setLocalProperties whenSplittingNode v.isFrozen
                      Just (DraggingNode _) ->
                        setLocalProperties whenDraggingNode v.isFrozen
                      Just (ModifyConnection (CreatingNewNode _)) ->
                        setLocalProperties whenSourceNodeSelected v.isFrozen
                      Just (ModifyConnection _) ->
                        setLocalProperties whenEditingConnection v.isFrozen
                      Just (Executing _) ->
                        setLocalProperties whenExecuting v.isFrozen
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
            Just (_, ModifyConnection (CreatingNewNode _)) ->
              { mainProperties_base | canEscape = True }
            Just (_, ModifyConnection _) ->
              { mainProperties_base | canEscape = True, canAcceptCharacters = True }
            Just (_, Executing _) ->
              { mainProperties_base | canEscape = True }
            Just (_, DraggingSplitter _) ->
              mainProperties_base
            Nothing ->
              mainProperties_base
    }
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg {- |> (\v -> if v == ForceDirectedMsg FDG.Tick then v else Debug.log "MESSAGE" v) -} of
    UIMsg ui_msg ->
      update_ui ui_msg model
    SelectNode view_uuid node_id (x, y) ->
      ( case peekInteraction (Just view_uuid) model of
          Nothing ->
            selectNodeInView model view_uuid node_id (x, y)
            |> setProperties
          Just (ModifyConnection (CreatingNewNode alteration)) ->
            -- we already have a node selected, and now, an existing
            -- node is being selected as the destination.
            editConnection model view_uuid alteration node_id
            |> setProperties
          _ ->
            model
      , Cmd.none
      )
    
    MoveNode view_uuid source_id dest_id (x, y) ->
      ( { model
          | graph_views =
              AutoDict.update view_uuid
                (Maybe.map
                  (moveNode source_id dest_id (x, y))
                )
                model.graph_views
        }
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
            Just (Just uuid, ModifyConnection (CreatingNewNode {source,dest}), _) ->
              cancelNewNodeCreation uuid source dest model -- this will pop, and handle graph changes too.
              |> setProperties
            Just (_, _, model_) ->
              model_ -- yay, I could pop from the global
              |> setProperties
        else
          model
      , Cmd.none
      )

    -- GraphViewMsg uuid submsg ->
    --   AutoDict.get uuid model.graph_views
    --   |> Maybe.map (updateGraphView submsg)
    --   |> Maybe.withDefault ( model, Cmd.none )
    -- OnResize (width, height) ->
    --   let
    --     newModel =
    --       { model
    --         -- When I receive this, it is the size of the Application in the
    --         -- x-direction EXCLUDING external borders, but it is the size of
    --         -- the Application in the y-direction INCLUDING external borders.
    --         -- No; I don't understand that at all.  But it's what I have to
    --         -- work with, I guess.
    --         | mainPanelDimensions = (width - 4, height)
    --       }
    --     (newRightTopWidth, newRightTopHeight, newFdgModel) = calculateRightTopDimensions newModel
    --   in
    --   ( { newModel
    --       | rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
    --       , fdg_model = newFdgModel
    --     }
    --   , Cmd.none
    --   )

    -- ClickIcon icon ->
    --   let
    --     (newLeftPanelOpen, newSelectedIcon) =
    --       if model.selectedIcon == Just icon && model.leftPanelOpen then
    --         (False, Nothing)  -- Close panel if same icon clicked
    --       else
    --         (True, Just icon)  -- Open panel with new icon
    --     (newRightTopWidth, newRightTopHeight, newFdgModel) =
    --       calculateRightTopDimensions { model | leftPanelOpen = newLeftPanelOpen }
    --   in
    --   ( { model 
    --     | leftPanelOpen = newLeftPanelOpen
    --     , selectedIcon = newSelectedIcon
    --     , rightTopPanelDimensions = (newRightTopWidth, newRightTopHeight)
    --     , fdg_model = newFdgModel
    --     }
    --   , Cmd.none
    --   )

    -- StartDraggingHorizontalSplitter ->
    --   ( { model | isDraggingHorizontalSplitter = True }, Cmd.none )

    -- StartDraggingVerticalSplitter ->
    --   ( { model | isDraggingVerticalSplitter = True }, Cmd.none )

    -- StopDragging ->
    --   ( { model 
    --     | isDraggingHorizontalSplitter = False
    --     , isDraggingVerticalSplitter = False
    --     }
    --   , Cmd.none
    --   )

    -- MouseMoveDrag x y ->
    --   if model.isDraggingHorizontalSplitter then
    --     let
    --       (viewportWidth, _) = model.mainPanelDimensions
    --       leftOffset =
    --         60 -- icon-bar width
    --         + 1 -- icon-bar right-border
    --         + 20 -- left-panel left-padding
    --         + 20 -- left-panel right-padding
    --         + 1 -- left-panel right-border
    --         + 4 -- half the splitter-width
    --         + 2 -- …it looks visually nicer, because the cursor takes up some area
    --       minWidth = 100 + leftOffset
    --       maxWidth = viewportWidth / 2
    --       newLeftPanelWidth = clamp minWidth maxWidth (x - leftOffset)
    --     in
    --       ( recalculateUI { model | leftPanelWidth = newLeftPanelWidth }
    --       , Cmd.none
    --       )
    --   else if model.isDraggingVerticalSplitter then
    --     let
    --       (_, viewportHeight) = model.mainPanelDimensions
    --       minHeight = 185  -- 8em @ 16px font ≈ 128px
    --       maxHeight = viewportHeight / 2
    --       statusBarHeight = 30
    --       newBottomHeight = clamp minHeight maxHeight (viewportHeight - y - statusBarHeight)
    --     in
    --     ( recalculateUI { model | rightBottomPanelHeight = newBottomHeight }
    --     , Cmd.none
    --     )
    --   else
    --     ( model, Cmd.none )

    -- ToggleBottomPanel ->
    --   ( recalculateUI { model | rightBottomPanelOpen = not model.rightBottomPanelOpen }
    --   , Cmd.none
    --   )

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

    -- UpdateRightTopDimensions width height ->
    --   ( { model | rightTopPanelDimensions = (width, height) }, Cmd.none )

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

    -- SelectNode _ _ ->
    --   Debug.todo "branch 'SelectNode _ _' not implemented"

    -- Pan _ _ _ ->
    --   Debug.todo "branch 'Pan _ _ _' not implemented"

    -- Zoom _ _ ->
    --   Debug.todo "branch 'Zoom _ _' not implemented"

    -- Undo _ ->
    --   Debug.todo "branch 'Undo _' not implemented"

    -- Redo _ ->
    --   Debug.todo "branch 'Redo _' not implemented"

    -- StartSplit _ _ ->
    --   Debug.todo "branch 'StartSplit _ _' not implemented"

    -- ResetView _ ->
    --   Debug.todo "branch 'ResetView _' not implemented"

    -- Escape ->
    --   Debug.todo "branch 'Escape' not implemented"

    -- Confirm ->
    --   Debug.todo "branch 'Confirm' not implemented"

    -- SetMouseOver _ _ ->
    --   Debug.todo "branch 'SetMouseOver _ _' not implemented"

    -- KeyPressed _ ->
    --   Debug.todo "branch 'KeyPressed _' not implemented"

    -- MouseUp ->
    --   Debug.todo "branch 'MouseUp' not implemented"

    -- MouseMove _ _ ->
    --   Debug.todo "branch 'MouseMove _ _' not implemented"

    -- RunExecution ->
    --   Debug.todo "branch 'RunExecution' not implemented"

    -- ResetExecution ->
    --   Debug.todo "branch 'ResetExecution' not implemented"

-- calculateRightTopDimensions : Model -> ( Float, Float, FDG.Model )
-- calculateRightTopDimensions model =
--   let
--     -- the main panel dimensions are the size of the application area.
--     (viewportWidth, viewportHeight) = model.mainPanelDimensions
--     -- the icon bar is 60px wide. Not included: a 1px right-border.
--     iconBarWidth = 60
--     -- the left-panel also includes its right-border size here, for convenience.
--     -- and it also includes 20px of padding on either side, if it's open.
--     leftPanelWidth = if model.leftPanelOpen then model.leftPanelWidth + 1 + 20 + 20 else 0
--     -- the status bar is 30px tall.  Not included: a 1px top border.
--     statusBarHeight = 30
--     -- the bottom-right panel is a particular height. Included: a 1px border on all sides.
--     bottomPanelHeight = if model.rightBottomPanelOpen then model.rightBottomPanelHeight + 2 else 0
--     -- Always account for some splitter height (either full splitter or collapsed splitter)
--     leftRightSplitterWidth = if model.leftPanelOpen then 8 else 0
--     rightTopBottomSplitterHeight = if model.rightBottomPanelOpen then 8 else 4
--     -- add in: the 1px border from iconBar
--     -- Also, the right-panel has a 1px border all around; add it in.
--     rightTopWidth = viewportWidth - iconBarWidth - leftPanelWidth - leftRightSplitterWidth - 1 - 2
--     -- add in: the 1px statusBar  border
--     -- Also, the right-panel has a 1px border all around; add it in.
--     rightTopHeight = viewportHeight - statusBarHeight - bottomPanelHeight - rightTopBottomSplitterHeight - 1 - 2
--     newFdgModel =
--       FDG.update
--         (FDG.ViewportUpdated (rightTopWidth, rightTopHeight))
--         model.fdg_model        
--   in
  -- Debug.log
  --   ( "Dimension calculation"
  --   ++"\nApplication area width & height: " ++ Debug.toString (viewportWidth, viewportHeight)
  --   ++"\nWidth calculation:"
  --   ++"\n   Icon-bar width (excl. 1px right-border): " ++ String.fromFloat iconBarWidth
  --   ++"\n   Left-panel width (INCL. 1px right-border + 20px padding on each side): " ++ String.fromFloat leftPanelWidth
  --   ++"\n   Left-right splitter width: " ++ String.fromFloat leftRightSplitterWidth
  --   ++"\n   Calculation: " ++ String.fromFloat viewportWidth ++ " [total width] "
  --   ++" - " ++ String.fromFloat iconBarWidth ++ " [icon-bar width] "
  --   ++" - " ++ String.fromFloat leftPanelWidth ++ " [left-panel width] "
  --   ++" - " ++ String.fromFloat leftRightSplitterWidth ++ " [left-right splitter width] "
  --   ++" - 1 [icon-bar border] - 2 [right-panel 1px border] = " ++ String.fromFloat rightTopWidth
  --   ++"\nHeight calculation:"
  --   ++"\n   Status-bar height (excl. 1px top-border): " ++ String.fromFloat statusBarHeight
  --   ++"\n   Bottom-panel height (INCL. 1px border on each side): " ++ String.fromFloat bottomPanelHeight
  --   ++"\n   Right-top-bottom splitter height: " ++ String.fromFloat rightTopBottomSplitterHeight
  --   ++"\n   Calculation: " ++ String.fromFloat viewportHeight ++ " [total height] "
  --   ++" - " ++ String.fromFloat statusBarHeight ++ " [status-bar height] "
  --   ++" - " ++ String.fromFloat bottomPanelHeight ++ " [bottom-panel height] "
  --   ++" - " ++ String.fromFloat rightTopBottomSplitterHeight ++ " [right-top-bottom splitter height] "
  --   ++" - 1 [status-bar border] - 1 [bottom-panel border] - 2 [right-panel 1px border] = " ++ String.fromFloat rightTopHeight
  --   ) () |> \_ ->
  -- ( rightTopWidth, rightTopHeight, newFdgModel )

-- recalculateUI : Model -> Model
-- recalculateUI model =
--   let
--     ( rightTopWidth, rightTopHeight, newFdgModel ) =
--       calculateRightTopDimensions model
--   in
--     { model
--       | fdg_model = newFdgModel
--       , rightTopPanelDimensions = ( rightTopWidth, rightTopHeight )
--     }

-- VIEW

viewMainInterface : Model -> Html Msg
viewMainInterface model =
  let
    lastInteraction =
      mostRecentInteraction model
      |> Maybe.map Tuple.second
  in
  div
    [ HA.class "editor-frame" ]
    [ viewNavigatorsArea model
    , viewSplitter
        5 LeftRight
        lastInteraction
        (model.uiState.open.sideBar)
    , div
        [ HA.class "editor-and-tools-panel" ]
        [ div
            [ HA.class "editor-main"
            , HA.css
                [ Css.maxWidth <| px <| Tuple.first model.uiState.dimensions.mainEditor
                , Css.maxHeight <| px <| Tuple.second model.uiState.dimensions.mainEditor
                ]
            ]
            [ UserInterface.debugDimensions model.uiState.dimensions.mainEditor
            , AutoDict.get model.mainGraphView model.graph_views
              |> Maybe.map (GraphEditor.viewGraph >> Html.Styled.fromUnstyled)
              |> Maybe.withDefault
                (div [ HA.class "error graph-not-loaded" ] [ text "⚠ Graph to load was not found!" ]) -- erk! say what now?!
            ]
        , viewSplitter
            4 UpDown
            lastInteraction
            model.uiState.open.bottomPanel
        , viewToolsArea model
        ]
    ]

viewCharacterPicker : Model -> Uuid -> NodeId -> NodeId -> Connection -> Html Msg
viewCharacterPicker model uuid source dest connection =
  div [] [ text "character picker" ]

viewGraphPicker : Model -> Uuid -> NodeId -> NodeId -> Connection -> Html Msg
viewGraphPicker model uuid source dest connection =
  div [] [ text "graph picker" ]

viewConnectionEditor : Model -> Uuid -> ConnectionAlteration -> Html Msg
viewConnectionEditor model uuid {source, dest, connection, picker} =
  case picker of
    ChooseCharacter ->
      viewCharacterPicker model uuid source dest connection
    ChooseGraphReference ->
      viewGraphPicker model uuid source dest connection

view : Model -> Html Msg
view model =
  case mostRecentInteraction model of
    Just (Just uuid, ModifyConnection (EditExistingConnection alteration)) ->
      viewConnectionEditor model uuid alteration
    _ ->
      viewMainInterface model

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

-- viewIcon : LeftPanelIcon -> String -> List (Html Msg) -> Model -> Html Msg
-- viewIcon icon iconText extra model =
--   let
--     isSelected = model.selectedIcon == Just icon
--     iconClass = if isSelected then "icon icon--selected" else "icon"
--   in
--   div 
--     [ HA.class iconClass
--     , onClick (ClickIcon icon)
--     , HA.css
--         [ Css.position Css.relative
--         ]
--     ]
--     ( text iconText :: extra )






    

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

-- viewLeftPanel : Model -> Html Msg
-- viewLeftPanel model =
--   div 
--     [ HA.class "left-panel"
--     , HA.style "width" (String.fromFloat model.leftPanelWidth ++ "px")
--     ]
--     [ case model.selectedIcon of
--         Just ComputationsIcon ->
--           div []
--             [ h2
--                 []
--                 [ text "Computations "
--                 , div
--                     [ HA.class "button button--primary"
--                     , onClick CreateNewPackage
--                     , HA.title "Create new computation"
--                     ]
--                     [ text "➕" ]
--                 ]
--             -- , p [] [ text "File management functionality would go here." ]
--             , ul []
--               ( model.fdg_model.packages
--                 |> AutoDict.toList
--                 |> List.map Tuple.second
--                 |> List.sortBy (Time.posixToMillis << .created)
--                 |> List.reverse
--                 |> List.map
--                   (\pkg ->
--                     li
--                       []
--                       [ viewPackageItem model pkg ]
--                   )
--               )
--             ]
        
--         Just SearchIcon ->
--           div []
--             [ h3 [] [ text "Search" ]
--             , input 
--               [ HA.type_ "text"
--               , HA.placeholder "Search in files..."
--               ] []
--             , p [] [ text "Search results would appear here." ]
--             ]
        
--         Just GitIcon ->
--           div []
--             [ h3 [] [ text "Source Control" ]
--             , p [] [ text "Git integration would go here." ]
--             , div [ HA.class "panel-content" ]
--               [ p [] [ text "• 3 changes" ]
--               , p [] [ text "• 1 staged file" ]
--               , p [] [ text "• main branch" ]
--               ]
--             ]
        
--         Just TestsIcon ->
--           viewLeftTestPanel model
        
--         Just ExtensionsIcon ->
--           div []
--             [ h3 [] [ text "Extensions" ]
--             , p [] [ text "Extension management would go here." ]
--             , div [ HA.class "panel-content" ]
--               [ p [] [ text "🔧 Elm Language Support" ]
--               , p [] [ text "🎨 Dracula Theme" ]
--               , p [] [ text "📝 Auto Format" ]
--               ]
--             ]
        
--         Nothing ->
--           text ""
--     ]

-- viewHorizontalSplitter : Html Msg
-- viewHorizontalSplitter =
--   div 
--     [ HA.class "horizontal-splitter"
--     , onMouseDown StartDraggingHorizontalSplitter
--     ]
--     []

-- viewRightSection : Model -> Html Msg
-- viewRightSection model =
--   div 
--     [ HA.class "right-section" ]
--     [ viewRightTopPanel model
--     , if model.rightBottomPanelOpen then
--         div []
--           [ viewVerticalSplitter
--           , viewRightBottomPanel model
--           ]
--       else
--         -- Show a minimal splitter when panel is closed
--         viewCollapsedVerticalSplitter
--     ]

-- viewRightTopPanel : Model -> Html Msg
-- viewRightTopPanel model =
--   div 
--     [ HA.class "right-top-panel" ]
--     [ -- For now, display dimensions as requested in comments
--       div 
--         [ HA.class "right-top-panel__content" ]
--         [ Html.Styled.fromUnstyled
--             ( FDG.view model.fdg_model
--               |> Html.map ForceDirectedMsg
--             )
--         ]
--     ]

-- viewVerticalSplitter : Html Msg
-- viewVerticalSplitter =
--   div 
--     [ HA.class "vertical-splitter"
--     , onMouseDown StartDraggingVerticalSplitter
--     ]
--     [ div 
--       [ HA.class "vertical-splitter__handle"
--       , onClick ToggleBottomPanel
--       ]
--       []
--     ]

-- viewCollapsedVerticalSplitter : Html Msg
-- viewCollapsedVerticalSplitter =
--   div 
--     [ HA.class "collapsed-vertical-splitter"
--     , onClick ToggleBottomPanel
--     ]
--     [ div 
--       [ HA.class "collapsed-vertical-splitter__handle" ]
--       []
--     ]

-- viewRightBottomPanel : Model -> Html Msg
-- viewRightBottomPanel model =
--   div 
--     [ HA.class "right-bottom-panel"
--     , HA.style "height" (String.fromFloat model.rightBottomPanelHeight ++ "px")
--     ]
--     [ viewBottomPanelHeader model
--     , viewBottomPanelContent model
--     ]

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

{-
[g] can select connection?
[g] can select empty space?
[g] can select nodes?
[g] can split nodes?
[g] can execute?

-}

translateHostCoordinates : (Float, Float) -> GraphView -> (Float, Float)
translateHostCoordinates (x, y) graph_view =
  let
    ( x_host, y_host ) = graph_view.host_coordinates
    ( w_host, h_host ) = graph_view.host_dimensions
    ( x_guest, y_guest ) = graph_view.guest_coordinates
    ( w_guest, h_guest ) = graph_view.guest_dimensions
    translate_dimension coord coord_host coord_guest dim_host dim_guest =
      if coord <= coord_host then
        coord_guest
      else if coord >= coord_host + dim_host then
        coord_guest + dim_guest
      else
        let
          ratio = dim_guest / dim_host
        in
          (coord - coord_host) * ratio + coord_guest
    translate_x =
      translate_dimension x x_host x_guest w_host w_guest
    translate_y =
      translate_dimension y y_host y_guest h_host h_guest
  in
    ( translate_x, translate_y )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    -- ( xCoord, yCoord ) = model.mouseCoords -- |> Debug.log "Mouse coords"
    -- ( width, height ) = model.dimensions
    -- panSubscription =
    --   let
    --     xPan = xPanAt width xCoord
    --     yPan = yPanAt height yCoord
    --   in
    --     if model.mouseIsHere && (xPan /= 0 || yPan /= 0) then
    --       Time.every 20 (\_ -> Pan xPan yPan {- |> Debug.log "Pan request" -})
    --     else
    --       Sub.none

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
        createNodeMoveSubscription uuid source dest =
          AutoDict.get uuid model.graph_views
          |> Maybe.map
            (\graph_view ->
              BE.onMouseMove
                ( D.map2
                    (\x y ->
                      MoveNode
                        uuid
                        source
                        dest
                        (graph_view |> translateHostCoordinates (x, y)) -- this is incorrect! Must translate…
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
              (Just uuid, ModifyConnection (CreatingNewNode {source, dest}) :: _) ->
                createNodeMoveSubscription uuid source dest
              _ ->
                Nothing
          )
  in
    Sub.batch
      ( resizeSubscription ::
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