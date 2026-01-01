module Main exposing (..)
import AutoDict
import Automata.Data exposing (..)
import Automata.Debugging exposing (debugAutomatonGraph, debugAutomatonGraphXY, debugGraph, debugLog_, println)
import Automata.DFA as DFA
import AutoSet
import Basics.Extra exposing (..)
import Browser
import Browser.Events as BE
import Changes as C
import Css exposing (px)
import Dict exposing (Dict)
import Force
import Graph exposing (NodeId)
import GraphEditor
import Html.Styled exposing (button, div, h1, Html, input, li, p, span, text, textarea, toUnstyled, ul)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import IntDict
import Json.Decode as D
import Json.Encode as E
import Jsonify exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Platform.Cmd as Cmd
import Ports exposing (..)
import Queries as Q
import Random.Pcg.Extended as Random
import Set
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes
import Task
import Time
import TypedSvg exposing (g)
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Types exposing (Paint(..), AlignmentBaseline(..), FontWeight(..), AnchorAlignment(..) , Cursor(..), DominantBaseline(..), Transform(..), StrokeLinecap(..))
import UILogic as UI
import Uuid exposing (Uuid)

{-
Quality / technology requirements:

1. Use CSS3, HTML5, and classes.  The file `style.css` can be modified with the
   correct styles.
2. Html.Styled should be used to maintain type safety, unless that is impossible
   or impractical.  If impossible or impractical, a comment should be left to
   explain why that is the case.
-}

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
      |> Result.withDefault (Flags {w = 800, h = 600} 0 [] (Time.millisToPosix 0) [])
    
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
          , max = decoded.viewport.w / 2 - 60
          , initial = clamp 100 (decoded.viewport.w / 2 - 60) 300
          }
      , toolsPanelHeight =
          { min = 80
          , max = decoded.viewport.h / 2 - 40
          , initial = clamp 80 (decoded.viewport.h / 2 - 40) 200
          }
      }
    state : UILayout
    state =
      { dimensions =
          { sideBar = Dimension constants.sideBarWidth.initial decoded.viewport.h
          , bottomPanel =
              Dimension
                ( decoded.viewport.w - constants.sideBarWidth.initial - 8 - 48 )
                ( constants.toolsPanelHeight.initial )
          , mainEditor =
              Dimension
                ( decoded.viewport.w - constants.sideBarWidth.initial - 8 - 48 )
                ( decoded.viewport.h - constants.toolsPanelHeight.initial - 8 )
          , viewport = decoded.viewport
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
      , properties = UI.nilMainProperties
      , computationsExplorer = []
      }
    model =
      -- I _know_ that this will succeed, because I've added this
      -- exact one
      let
        (id, model_) =
          getUuid model_excl_views
        gv =
          UI.makeGraphView id SolvedByCoordinateForces
            state.dimensions.mainEditor False model_.packages
            mainPackage.computation
          |> C.linkGraphViewToPackage model_.packages mainPackage.packageIdentifier
      in
        C.upsertGraphView gv model_
        |> C.setMainView id
  in
    ( UI.selectNavIcon ComputationsIcon model
      |> UI.refreshComputationsList
      |> UI.setProperties
    , Cmd.none
    )

updateGraphInView : (AutomatonGraph -> AutomatonGraph) -> GraphView -> Model -> Model
updateGraphInView updater graph_view model =
  let
    ag = updater graph_view.computation
  in
    C.upsertGraphView
      ( { graph_view | computation = ag }
        |> GraphEditor.recalculateLinksAndNodes model.packages
        |> UI.fitGraphViewToGraph
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

update_graphview : Uuid -> GraphViewMsg -> Model -> ( Model, Cmd Msg )
update_graphview uuid ui_msg model =
  case ui_msg of
    StartDraggingNode nodeId node_host_coord ->
      UI.calculate_host_coord uuid model node_host_coord nodeId
      |> Maybe.map (\host_coord ->
        ( C.pushInteractionForStack (Just uuid) (DraggingNode nodeId host_coord) model
          |> UI.setProperties
        , Cmd.none
        )
      )
      |> Maybe.withDefault (model, Cmd.none)

    Pan pan_x pan_y ->
      ( { model
          | graph_views =
              AutoDict.update uuid
                (Maybe.map (GraphEditor.panGraphView pan_x pan_y))
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

    SelectNode node_id node_coord ->
      ( case C.popInteraction (Just uuid) model of
          Just (ChoosingDestinationFor src etc _, model_) ->
            selectDestination uuid src etc model_
          _ -> -- includes 'Nothing'
            -- this is the initial selection.
            UI.calculate_host_coord uuid model node_coord node_id
            |> Maybe.map (\host_coord ->
              GraphEditor.selectSourceNode model uuid host_coord node_id
              |> UI.setProperties
            )
            |> Maybe.withDefault model
      , Cmd.none
      )

    SelectSpace ->
      ( case C.popInteraction (Just uuid) model of
          Just (ChoosingDestinationFor src etc _, model_) ->
            selectDestination uuid src etc model_
          _ ->
            model
      , Cmd.none
      )
    
    MoveNode coord ->
      ( case Q.peekInteraction (Just uuid) model.interactionsDict of
          Just (DraggingNode node_id _) ->
            GraphEditor.dragNode uuid coord node_id model
            |> UI.setProperties
          Just (ChoosingDestinationFor _ _ _) ->
            GraphEditor.movePhantomNode uuid coord model
            |> UI.setProperties
          _ ->
            model
      , Cmd.none
      )

    StopDraggingNode ->
      ( C.popInteraction (Just uuid) model
        |> Maybe.map (Tuple.second >> UI.setProperties)
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
            UI.startSplit uuid graph_view nodeContext model
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
      -- |> debugLog_ "recursive_connection" AutoSet.toList
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
                    -- |> debugLog_ "Added to Left" IntDict.toList
                  , r_
                  )
                else
                  ( l_
                  , IntDict.update k
                    ( Maybe.map (AutoSet.insert transition)
                      >> Maybe.orElseLazy (\() -> Just <| AutoSet.singleton transitionToString transition)
                    )
                    r_
                    -- |> debugLog_ "Added to Right" IntDict.toList
                  )
              )
              (l, r)
              conn
          )
          (IntDict.empty, IntDict.empty)
    ag =
      graph_view.computation
      -- |> debugAutomatonGraph "Before node-split"
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
              -- |> debugGraph "After inserting left connections"
            |> Graph.insert
                  { node
                    | incoming =
                        rightConnections
                        |> IntDict.insert node.node.id recursive_connection
                  }
              -- |> debugGraph "After inserting right connections"
      }
  in
    newUserGraph
    -- |> debugAutomatonGraphXY "After node-split"

selectDestination : Uuid -> NodeId -> PossibleDestination -> Model -> Model
selectDestination view_uuid src possible_dest model =
  case possible_dest of
    NewNode phantom_id svg_coord ->
      -- I've clicked on some kind of an empty space.  I'll want to create this node,
      -- and then proceed to edit the connection.
      GraphEditor.createNewGraphNode view_uuid phantom_id svg_coord model
      -- editConnection will call UI.setProperties
      |> UI.editConnection (Just view_uuid) svg_coord
          { source = src
          , dest = phantom_id
          , connection = AutoSet.empty transitionToString
          , targetKind = PhantomNodeNewConnection
          }
    ExistingNode dest_id conn ->
      let
        graph_view = AutoDict.get view_uuid model.graph_views
        svg_coord =
          Maybe.andThen (\gv -> Graph.get dest_id gv.computation.graph) graph_view
          |> Maybe.map (\ctx -> Coordinate ctx.node.label.x ctx.node.label.y)
          |> Maybe.withDefault (Coordinate 0 0)
        existing_connection =
          Maybe.andThen (\gv -> Graph.get src gv.computation.graph) graph_view
          |> Maybe.map (\ctx -> Q.linkExistsInGraph ctx dest_id)
          |> Maybe.withDefault False
      in
        -- we already have a node selected, and now, an existing
        -- node is being selected as the destination.
        -- editConnection will call UI.setProperties
        UI.editConnection (Just view_uuid) svg_coord
          { source = src
          , dest = dest_id
          , connection = conn
          , targetKind =
              if existing_connection then
                ExistingNodeExistingConnection
              else
                ExistingNodeNewConnection
          }
          model

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
          C.upsertGraphView (UI.fitGraphViewToGraph updated_view) model_
      in
        UI.refreshComputationsList updated_model
    
    updateExistingNode : NodeId -> NodeId -> Connection -> GraphView -> Model -> Model
    updateExistingNode src dest conn gv model_ =
      GraphEditor.updateLink_graphchange src dest conn gv.computation
      |> \newGraph -> commit_change gv newGraph model_

    confirmPhantomNode : NodeId -> NodeId -> Connection -> GraphView -> Model -> Model
    confirmPhantomNode src dest conn gv model_ =
      let
        ag = GraphEditor.updateLink_graphchange src dest conn gv.computation
      in
      commit_change
        { gv
          | computation = ag
          , drawingData =
              let dd = gv.drawingData in
              { dd
                | tentative_link = Nothing
                , link_drawing = GraphEditor.linkDrawingForPackage ag model.packages
                , node_drawing = GraphEditor.nodeDrawingForPackage ag gv.id
              }
        }
        ag model_
          

    removeLink : NodeId -> NodeId -> GraphView -> Model -> Model
    removeLink src dest gv model_ =
      GraphEditor.removeLink_graphchange src dest gv.computation
      |> \newGraph -> commit_change gv newGraph model_

  in
    case C.popMostRecentInteraction model of
      Just (Just gv_uuid, SplittingNode { to_split, left, right } {mainGraph}, model_) ->
        ( if AutoSet.isEmpty left || AutoSet.isEmpty right then
             C.removeViews [ mainGraph ] model_
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
                   C.removeViews [ mainGraph ] model_
                  |> commit_change gv newGraph
            )
            |> Maybe.withDefault model_
            |> UI.setProperties
        , Cmd.none
        )
      Just (Just gv_uuid, EditingConnection ({ source, dest, connection, targetKind }) {mainGraph, referenceList}, model_) ->
          -- create such an automatongraph
          ( AutoDict.get (gv_uuid) model_.graph_views   
            |> Maybe.map
              (\gv ->
                case targetKind of
                  PhantomNodeNewConnection ->
                    -- this one comes from a phantom node.
                    C.removeViews (mainGraph :: referenceList) model_
                    -- because this comes from a phantom node, ensure that we
                    -- remove the 'dest' from the graph before confirming it as
                    -- the previous graph.
                    |> confirmPhantomNode source dest connection gv
                    |> UI.setProperties
                  ExistingNodeNewConnection ->
                    C.removeViews (mainGraph :: referenceList) model_
                    |> removeLink source dest gv
                    |> UI.setProperties
                  ExistingNodeExistingConnection ->
                    C.removeViews (mainGraph :: referenceList) model_
                    |> updateExistingNode source dest connection gv
                    |> UI.setProperties
              )
            |> Maybe.withDefault model_
          , Cmd.none
          )
      Just (Nothing, DeletingPackage to_delete props, model_) ->
        let
          updated_model =
             C.removeViews (to_delete :: props.directViews ++ props.indirectViews) model_
            |> C.deletePackageFromModel to_delete
            |> UI.refreshComputationsList
            |> UI.setProperties
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
                without_undoredo =
                  { gv
                    -- clear the undo/redo buffers
                    | undoBuffer = []
                    , redoBuffer = []
                  }
                model_ =
                  updateGraphInView
                    (GraphEditor.applyChangesToGraph)
                    without_undoredo
                    model
                updated_model =
                  C.updatePackageFromView model.mainGraphView model_
                  |> UI.refreshComputationsList
                  |> UI.setProperties
              in
                ( updated_model
                , gv.graphPackage
                  |> Maybe.andThen (\pkg_uuid -> AutoDict.get pkg_uuid model.packages)
                  |> Maybe.map persistPackage
                  |> Maybe.withDefault Cmd.none
                )
          )
        |> Maybe.withDefault ( model, Cmd.none )

deletePackage : GraphPackage -> Model -> ( Model, Cmd Msg )
deletePackage package model =
  -- find out which packages are affected by this one.
  let
    package_uuid = package.packageIdentifier
    affected : List Uuid
    affected =
      Q.packagesAffectedBy package_uuid model
  in
    case affected of
      [] ->
        -- meh, one thing, nobody cares about it. Toss it!
        let
          without_package =
            { model | packages = AutoDict.remove package_uuid model.packages }
          relevant_views =
            Q.viewsContainingPackage package_uuid without_package
            |> List.map .id
        in
          (  C.removeViews relevant_views without_package
          , Ports.deleteFromStorage (Uuid.toString package_uuid)
          )
      _ ->
        ( UI.beginDeletionInteraction package_uuid affected package model
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

step_execution : GraphView -> (List ExecutionData -> List ExecutionData) -> Test -> Model -> Model
step_execution orig_graphview execution_function test model =
  let
    (previously_executed, previous_props, interactionFunction) =
      case Q.peekInteraction Nothing model.interactionsDict of
        Just ( Executing results props ) ->
          ( results
          , Just props
          , \new_hx new_props ->
              C.replaceInteraction Nothing (Executing new_hx new_props)
          )
        _ ->
          ( DFA.load test.input (Q.resolutionDict model.packages) orig_graphview.computation
            |> List.singleton
          , Nothing
          , \new_hx new_props ->
              C.pushInteractionForStack Nothing (Executing new_hx new_props)
          )
    applied_step : List ExecutionData
    applied_step =
      execution_function previously_executed
    head_step =
      List.head applied_step
    (id, model_) =
      getUuid model
    (graph_view, model__) =
      Maybe.map
        (\step ->
          let
            edges = executing_edges step
            gv =
              UI.makeGraphView id SolvedBySpreadOutForces
                model.uiLayout.dimensions.mainEditor
                True model.packages step.computation
            executed_model =
              C.upsertGraphView gv model_
              |> C.updateGraphView gv.id (UI.centerAndHighlight edges)
          in
            (gv, executed_model)
        ) head_step
      |> Maybe.withDefault (orig_graphview, model_)
    updated_props =
      Maybe.withDefault
        { expandedSteps = IntDict.empty
        }
        previous_props
  in
    interactionFunction applied_step updated_props model__
    |> C.setMainView graph_view.id
    |> UI.setProperties

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
                        DFA.load s (Q.resolutionDict model.packages) p.computation
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
                      DFA.load s (Q.resolutionDict model.packages) p.computation
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
            let
              (id, model_) =
                getUuid model
              gv =
                UI.makeGraphView id SolvedByCoordinateForces
                  model.uiLayout.dimensions.mainEditor
                  False model.packages pkg.computation
                |> C.linkGraphViewToPackage model.packages pkg.packageIdentifier              
            in
              C.upsertGraphView gv model_
              |>  C.removeViews [ model.mainGraphView ]
              |> C.setMainView gv.id
              |> C.selectPackage pkg.packageIdentifier
              |> UI.setProperties
          )
        |> Maybe.withDefaultLazy
          (\() ->
            println "[update→SelectPackage] Could not find the requested package"
            model
          )
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
            case C.popInteraction Nothing updated_model of
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
            case C.popInteraction Nothing updated_model of
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
                        Q.viewsContainingPackage pkg_uuid m
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
      ( case C.popInteraction (Nothing) model of
          Just (DraggingSplitter movement, model_) ->
            if shouldStop then -- do not, in fact, drag the splitter.
              model_
              |> UI.updateMainEditorDimensions
              |> UI.setProperties
            else -- such a drag.
              { model
                | uiLayout =
                    UI.dragSplitter coord movement model.uiConstants model.uiLayout
              }
              |> UI.updateMainEditorDimensions
              |> UI.setProperties
          _ ->
            model
      , Cmd.none
      )

    QuickInput input ->
      ( case Q.mostRecentInteraction model of
          Just (key_uuid, EditingConnection alteration props) ->
            UI.handleConnectionEditorInput key_uuid input alteration props model
          Just (key_uuid, SplittingNode data props) ->
            String.toList input
            |> List.head
            |> Maybe.map
                (\ch -> UI.nodeSplitSwitch props key_uuid (ViaCharacter ch) data model)
            |> Maybe.withDefault model
          _ ->
            model
      , Cmd.none
      )

    ToggleConnectionTransition via ->
        ( case Q.mostRecentInteraction model of
            Just (key_uuid, EditingConnection ({connection} as alteration) props) ->
              C.replaceInteraction key_uuid 
                ( EditingConnection
                    { alteration | connection = UI.toggleConnectionTransition via connection }
                    props
                )
                model
            Just (key_uuid, SplittingNode data props) ->
              UI.nodeSplitSwitch props key_uuid via data model
            _ ->
              model
        , Cmd.none
        )

    SelectNavigation ComputationsIcon ->
      ( UI.selectNavIcon ComputationsIcon model
        |> UI.refreshComputationsList
        |> UI.setProperties
      , Cmd.none
      )

    SelectNavigation TestsIcon ->
      ( UI.selectNavIcon TestsIcon model
        |> UI.setProperties
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
      ( UI.resizeViewport dims model
      , Cmd.none
      )

    ToggleAreaVisibility where_ ->
      ( { model
          | uiLayout = UI.toggleAreaVisibility where_ model.uiLayout
        }
        |> UI.updateMainEditorDimensions
        |> UI.setProperties
      , Cmd.none
      )
    StartDraggingSplitter movement ->
      ( C.pushInteractionForStack (Nothing) (DraggingSplitter movement) model
        |> UI.setProperties
      , Cmd.none
      )

    Escape ->
      ( if model.properties.canEscape then
          case C.popMostRecentInteraction model of
            Nothing -> -- hmm, let's look at the local interactions of the main editor window.
              -- huh!  Looks like there's nothing to do!  So why was I called??
              -- Ideally, I shouldn't even spawn an event if there's nothing to do.
              model
            Just (Just uuid, ChoosingDestinationFor _ (NewNode _ _) _, _) ->
              GraphEditor.cancelNewNodeCreation uuid model -- this will pop, and also handle graph/UI changes too.
              |> UI.setProperties
            Just (Just uuid, ChoosingDestinationFor source (ExistingNode dest _) _, _) ->
              GraphEditor.removePhantomLink uuid source dest model
            Just (Just uuid, EditingConnection alteration {referenceList, mainGraph}, model_) ->
              UI.escapeConnectionEditor uuid alteration (mainGraph :: referenceList) model_ 
              |> UI.setProperties
            Just (Just _, SplittingNode _ props, model_) ->
               C.removeViews [ props.mainGraph ] model_
              |> UI.setProperties
            Just (Nothing, DeletingPackage _ {mainGraph, directViews, indirectViews}, model_) ->
               C.removeViews (mainGraph :: directViews ++ indirectViews) model_
              |> UI.setProperties
            Just (_, _, model_) ->
              UI.setProperties model_ -- yay, I could pop from the global
        else
          model
      , Cmd.none
      )

    EditConnection coordinates uuid src dest connection ->
      ( UI.editConnection (Just uuid) coordinates
          { source = src
          , dest = dest
          , connection = connection
          , targetKind = ExistingNodeExistingConnection
          }
          model
      , Cmd.none
      )

    Undo ->
      ( { model
          | graph_views =
              AutoDict.update model.mainGraphView
                (Maybe.map (\graph_view ->
                    case (Q.mostRecentInteraction model, graph_view.undoBuffer) of
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
        |> UI.setProperties
      , Cmd.none
      )

    Redo ->
      ( { model
          | graph_views =
              AutoDict.update model.mainGraphView
                (Maybe.map (\graph_view ->
                    case (Q.mostRecentInteraction model, graph_view.redoBuffer) of
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
        |> UI.setProperties
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
        (id, model__) =
          getUuid updated_model
        gv =
          UI.makeGraphView id SolvedByCoordinateForces
            model.uiLayout.dimensions.mainEditor
            False
            model__.packages
            pkg.computation
      in
        ( C.upsertGraphView gv model__
          |> UI.refreshComputationsList
          |> UI.setProperties
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
      case C.popInteraction Nothing model of
        Just ( Executing _ _ , model_ ) ->
          ( UI.setProperties model_, Cmd.none )
        _ ->
          ( model, Cmd.none )

    ToggleDebugStep n ->
      ( case Q.peekInteraction Nothing model.interactionsDict of
          Just ( Executing results props ) ->
            case IntDict.get n props.expandedSteps of
              Just id ->
                -- get rid of this view.
                { model
                  | graph_views = AutoDict.remove id model.graph_views
                }
                -- and now get rid of the expanded step
                |> C.replaceInteraction Nothing
                    (Executing results
                      { props
                        | expandedSteps =
                            IntDict.remove n props.expandedSteps
                      }
                    )
                |> UI.setProperties
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
translateHostCoordinates : Coordinate -> Coordinate -> GraphView -> Coordinate
translateHostCoordinates host_coord mouse {host, guest, pan} =
  let
    ( pan_x, pan_y ) = pan
    translate_dimension coord coord_host coord_guest dim_host dim_guest pan_ =
      if coord <= coord_host then
        coord_guest + pan_ -- on the (left/top) edge
      else if coord >= coord_host + dim_host then
        coord_guest + dim_guest + pan_ -- on the (right/bottom) edge
      else
        -- this is in the actual area
        let
          ratio = dim_guest / dim_host
        in
          (coord - coord_host) * ratio + coord_guest + pan_
    translate_x =
      translate_dimension mouse.x host_coord.x guest.x host.w guest.w pan_x
    translate_y =
      translate_dimension mouse.y host_coord.y guest.y host.h guest.h pan_y
  in
    { x = translate_x, y = translate_y }
    -- |> Debug.log "Translated host coordinates"

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
                    case Q.mostRecentInteraction model of
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
                  case Q.mostRecentInteraction model of
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
      BE.onResize
        (\w h ->
          OnResize { w = toFloat w, h = toFloat h }
          -- |> Debug.log "Raw resize values"
        )
    splitterSubscriptions =
      case Q.peekInteraction Nothing model.interactionsDict of
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
        createNodeMoveSubscription host_coords uuid =
          AutoDict.get uuid model.graph_views
          |> Maybe.map
            (\graph_view ->
                BE.onMouseMove
                  ( D.map2
                      (\x y ->
                        MoveNode
                          (graph_view |> translateHostCoordinates host_coords (Coordinate x y))
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
              (Just uuid, ChoosingDestinationFor _ _ host_coords :: _) ->
                Just <| createNodeMoveSubscription host_coords uuid
              (Just uuid, DraggingNode _ host_coords :: _) ->
                Just <| Sub.batch 
                  [ createNodeMoveSubscription host_coords uuid
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

debugDimensions : Dimension -> Html a
debugDimensions {w, h} =
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
                    model.uiLayout.dimensions.sideBar.w
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
            (\gv_uuid ->
                AutoDict.get gv_uuid model.graph_views
                |> Maybe.andThen
                  (\graph_view ->
                    Maybe.map
                      (\pkg_uuid ->
                        div
                          [ HA.class "package"
                          , graph_view.properties.canSelectPackage
                            |> thenPermitInteraction (HE.onClick (PackageMsg pkg_uuid SelectPackage))
                          ]
                          [ GraphEditor.viewGraph graph_view
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
                      graph_view.graphPackage
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
      Q.peekInteraction Nothing model.interactionsDict
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
            [ GraphEditor.viewGraph gv ]
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
    [ case Q.peekInteraction Nothing model.interactionsDict of
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
                  model.uiLayout.dimensions.bottomPanel.h
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
                [ Css.maxWidth <| px <| model.uiLayout.dimensions.mainEditor.w
                , Css.maxHeight <| px <| model.uiLayout.dimensions.mainEditor.h
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
                  |> Maybe.map GraphEditor.viewGraph
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
                                [ GraphEditor.viewGraph graph_view
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
                  |> Maybe.map GraphEditor.viewGraph
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
            [ GraphEditor.viewGraph graph_view
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
    [ case Q.mostRecentInteraction model of
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