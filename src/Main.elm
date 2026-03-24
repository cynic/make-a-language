module Main exposing (..)
import AutoDict
import Data exposing (..)
import Debugging exposing (..)
import Automata.DFA as DFA
import AutoSet
import Basics.Extra exposing (..)
import Browser
import Browser.Events as BE
import Changes as C
import Graph exposing (NodeId)
import GraphEditor
import Html.Styled exposing (div, Html, input, p, text, toUnstyled)
import Html.Styled.Attributes as HA
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
import Task
import Time
import TypedSvg exposing (g)
import TypedSvg.Attributes.InPx exposing (x, y)
import UILogic as UI
import UIView as UI
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
          -- |> List.map (\v -> Debugging.debugAutomatonGraph "Loaded" v.model.computation |> \_ -> v)
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
      , can = UI.nilMainCapabilities
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
      |> UI.setCapabilities
    , Cmd.none
    )

updateGraphInView : (AutomatonGraph -> AutomatonGraph) -> GraphView -> Model -> Model
updateGraphInView updater graph_view model =
  C.upsertGraphView
    ( C.mapComputation updater graph_view
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
          |> UI.setCapabilities
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
              |> UI.setCapabilities
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
            |> UI.setCapabilities
          Just (ChoosingDestinationFor _ _ _) ->
            GraphEditor.movePhantomNode uuid coord model
            |> UI.setCapabilities
          _ ->
            model
      , Cmd.none
      )

    StopDraggingNode ->
      ( C.popInteraction (Just uuid) model
        |> Maybe.map (Tuple.second >> UI.setCapabilities)
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
      -- editConnection will call UI.setCapabilities
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
        -- editConnection will call UI.setCapabilities
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
    {- Take an AutomatonGraph and push it into the Model. -}
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
          C.upsertGraphView
            ( updated_view
              -- the drawing data must follow the new computation, or the
              -- canvas keeps rendering the pre-commit link/node state.
              |> GraphEditor.recalculateLinksAndNodes model_.packages
              |> UI.fitGraphViewToGraph
            )
            model_
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
      -- note: `gv` is passed WITHOUT the updated computation, so that
      -- commit_change pushes the *pre-commit* graph onto the undo buffer.
      commit_change
        { gv
          | drawingData =
              let dd = gv.drawingData in
              { dd
                | tentative_link = Nothing
                , link_drawing = GraphEditor.linkDrawingForPackage ag model.packages
                , node_drawing = GraphEditor.nodeDrawingForPackage ag gv.id
              }
        }
        ag model_
          

  in
    case C.popMostRecentInteraction model of
      Just (Just gv_uuid, SplittingNode { to_split, left, right } {mainGraph}, model_) ->
        ( if AutoSet.isEmpty left || AutoSet.isEmpty right then
             C.removeViews [ mainGraph ] model_
            |> UI.setCapabilities
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
            |> UI.setCapabilities
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
                    |> UI.setCapabilities
                  ExistingNodeNewConnection ->
                    -- there is no previous link to remove: this is a *new*
                    -- connection between existing nodes, so it must be
                    -- created with its edited transitions.
                    C.removeViews (mainGraph :: referenceList) model_
                    |> updateExistingNode source dest connection gv
                    |> UI.setCapabilities
                  ExistingNodeExistingConnection ->
                    C.removeViews (mainGraph :: referenceList) model_
                    |> updateExistingNode source dest connection gv
                    |> UI.setCapabilities
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
            |> UI.setCapabilities
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
                  |> UI.setCapabilities
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
    (previously_executed, previous_expansion, interactionFunction) =
      case Q.peekInteraction Nothing model.interactionsDict of
        Just ( Executing results expansion ) ->
          ( results
          , Just expansion
          , \new_hx new_props ->
              C.replaceInteraction Nothing (Executing new_hx new_props)
          )
        _ ->
          ( DFA.load test.input (Q.resolutionDict model.packages) orig_graphview.computation
            |> List.singleton
          , Nothing
          , \new_hx expansion ->
              C.pushInteractionForStack Nothing (Executing new_hx expansion)
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
    updated_expansion =
      Maybe.withDefault Nothing previous_expansion
  in
    interactionFunction applied_step updated_expansion model__
    |> C.setMainView graph_view.id
    |> UI.setCapabilities

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
              |> UI.setCapabilities
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
                ( UI.setCapabilities model_
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
                ( UI.setCapabilities model_
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
              |> UI.setCapabilities
            else -- such a drag.
              { model
                | uiLayout =
                    UI.dragSplitter coord movement model.uiConstants model.uiLayout
              }
              |> UI.updateMainEditorDimensions
              |> UI.setCapabilities
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
        |> UI.setCapabilities
      , Cmd.none
      )

    SelectNavigation TestsIcon ->
      ( UI.selectNavIcon TestsIcon model
        |> UI.setCapabilities
      , Cmd.none
      )

    SelectNavigation DebugIcon ->
      ( UI.selectNavIcon DebugIcon model
        |> UI.setCapabilities
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
        -- the graph views must follow the new areas: their `host` drives
        -- both rendering and the client→SVG coordinate translation.
        |> UI.updateMainEditorDimensions
      , Cmd.none
      )

    ToggleAreaVisibility where_ ->
      ( { model
          | uiLayout = UI.toggleAreaVisibility where_ model.uiLayout
        }
        |> UI.updateMainEditorDimensions
        |> UI.setCapabilities
      , Cmd.none
      )
    StartDraggingSplitter movement ->
      ( C.pushInteractionForStack (Nothing) (DraggingSplitter movement) model
        |> UI.setCapabilities
      , Cmd.none
      )

    Escape ->
      ( if model.can.escape then
          case C.popMostRecentInteraction model of
            Nothing -> -- hmm, let's look at the local interactions of the main editor window.
              -- huh!  Looks like there's nothing to do!  So why was I called??
              -- Ideally, I shouldn't even spawn an event if there's nothing to do.
              model
            Just (Just uuid, ChoosingDestinationFor _ (NewNode _ _) _, _) ->
              GraphEditor.cancelNewNodeCreation uuid model -- this will pop, and also handle graph/UI changes too.
              |> UI.setCapabilities
            Just (Just uuid, ChoosingDestinationFor source (ExistingNode dest _) _, model_) ->
              GraphEditor.removePhantomLink uuid source dest model_
              |> UI.setCapabilities
            Just (Just uuid, EditingConnection alteration {referenceList, mainGraph}, model_) ->
              UI.escapeConnectionEditor uuid alteration (mainGraph :: referenceList) model_ 
              |> UI.setCapabilities
            Just (Just _, SplittingNode _ props, model_) ->
               C.removeViews [ props.mainGraph ] model_
              |> UI.setCapabilities
            Just (Nothing, DeletingPackage _ {mainGraph, directViews, indirectViews}, model_) ->
               C.removeViews (mainGraph :: directViews ++ indirectViews) model_
              |> UI.setCapabilities
            Just (_, _, model_) ->
              UI.setCapabilities model_ -- yay, I could pop from the global
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
                        -- drawing data and viewport must follow the
                        -- restored computation (as on commit)
                        |> GraphEditor.recalculateLinksAndNodes model.packages
                        |> UI.fitGraphViewToGraph
                ))
                model.graph_views
        }
        |> UI.setCapabilities
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
                        -- drawing data and viewport must follow the
                        -- restored computation (as on commit)
                        |> GraphEditor.recalculateLinksAndNodes model.packages
                        |> UI.fitGraphViewToGraph
                ))
                model.graph_views
        }
        |> UI.setCapabilities
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
          |> UI.setCapabilities
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
          ( UI.setCapabilities model_, Cmd.none )
        _ ->
          ( model, Cmd.none )

    SelectDebugStep n ->
      ( case Q.peekInteraction Nothing model.interactionsDict of
          Just ( Executing results expansion ) ->
            Maybe.map
              (\item ->
                let
                  expand model_ =
                      let
                        (uuids, seed) =
                          Random.step
                            (Random.list (List.length item.debugging) Uuid.generator)
                            model.randomSeed
                        model__ =
                          -- get rid of the old views.
                          { model_ | randomSeed = seed }
                          -- replace the interaction; we KNOW which UUIDs we'll be creating.
                          |> C.replaceInteraction Nothing
                            (Executing results (Just { expanded = uuids, step = n }))
                      in
                        -- and now expand the new views.
                        List.zip uuids item.debugging
                        |> List.map (\(uuid, f) -> f uuid)
                        |> List.foldl C.upsertGraphView model__
                        |> UI.setCapabilities
                in
                  case expansion of
                    Just expanded ->
                      if expanded.step == n then
                        -- do nothing. Why should we? We've already got this expanded.
                        model
                      else
                        -- get rid of the old views.
                        { model | graph_views = List.foldl AutoDict.remove model.graph_views expanded.expanded }
                        |> expand
                    Nothing ->
                      -- this is my first expansion.
                      expand model
              )
              (List.getAt n results)
            |> Maybe.withDefault model
          _ ->
            model
      , Cmd.none
      )

    CrashWithMessage err ->
      Debug.todo err








{-
  **************************************
  Interaction → interactive capabilities
  **************************************
-}

-- SUBSCRIPTIONS

{-| The decoder that routes keyboard events to `Msg` values, given the
    current model.  Extracted (and exposed) from `subscriptions` so that it
    can be tested directly with synthetic event JSON: the routing rules —
    which keys mean what in which interaction state, and which modifier
    combinations are required — are business logic that update-simulation
    tests cannot see.
-}
keyboardDecoder : Model -> D.Decoder Msg
keyboardDecoder model =
  D.map2
    (\key ctrlPressed -> ( key, ctrlPressed ))
    (D.field "key" D.string)
    (D.field "ctrlKey" D.bool)
  |> D.andThen
    (\(key, ctrlPressed) ->
      case String.toLower key of
        "escape" ->
          if model.can.escape then                        
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

{-| Decoder for the pan payloads arriving from JavaScript via the `pan`
    port — extracted (and exposed) for testing.
-}
panDecoder : D.Decoder Msg
panDecoder =
  D.map3 (\uuid pan_x pan_y -> GraphViewMsg uuid (Pan pan_x pan_y))
    (D.field "uuid" Uuid.decoder)
    (D.at [ "pan", "pan_x" ] D.int)
    (D.at [ "pan", "pan_y" ] D.int)

{-| Decoder for the stop-pan payloads arriving via the `stopPan` port —
    extracted (and exposed) for testing.
-}
stopPanDecoder : D.Decoder Msg
stopPanDecoder =
  D.map (\uuid -> GraphViewMsg uuid StopPan) Uuid.decoder

{-| The mouse-move and mouse-up decoders for splitter dragging — extracted
    (and exposed) for testing.  Produces `Nothing` when no splitter drag is
    in flight, so no splitter messages can be produced in other states.
    Note the LeftRight offset: the navigator bar (48px) plus half the
    splitter (8px) sit between the window edge and the sidebar edge.
-}
splitterDecoders : Model -> Maybe ( D.Decoder Msg, D.Decoder Msg )
splitterDecoders model =
  case Q.peekInteraction Nothing model.interactionsDict of
    Just (DraggingSplitter LeftRight) ->
      let
        offset = -(48 + 8 / 2)
        clientX =
          D.field "clientX" D.float
      in
        Just
          ( D.map ((+) offset >> DragSplitter False) clientX
          , D.map ((+) offset >> DragSplitter True) clientX
          )
    Just (DraggingSplitter UpDown) ->
      let
        clientY =
          D.field "clientY" D.float
      in
        Just
          ( D.map (DragSplitter False) clientY
          , D.map (DragSplitter True) clientY
          )
    _ ->
      Nothing

{-| The mouse-move decoder for an in-flight node interaction: client
    coordinates are translated into SVG coordinates exactly once, via the
    host coordinates stored on the interaction.  Extracted (and exposed)
    for testing.
-}
nodeMoveDecoder : GraphView -> Coordinate -> Uuid -> D.Decoder Msg
nodeMoveDecoder graph_view host_coords uuid =
  D.map2
    (\x y ->
      GraphViewMsg uuid
        (MoveNode (UI.translateHostCoordinates host_coords (Coordinate x y) graph_view))
    )
    (D.field "clientX" D.float)
    (D.field "clientY" D.float)

{-| The node-movement decoders for every in-flight node interaction:
    (view uuid, mouse-move decoder, optional mouse-up decoder).  The
    mouse-up decoder exists only during a node drag.  Extracted (and
    exposed) for testing.
-}
nodeMoveDecoders : Model -> List ( Uuid, D.Decoder Msg, Maybe (D.Decoder Msg) )
nodeMoveDecoders model =
  AutoDict.toList model.interactionsDict
  |> List.filterMap
      (\( key, (_, stack) ) ->
        case ( key, stack ) of
          ( Just uuid, ChoosingDestinationFor _ _ host_coords :: _ ) ->
            AutoDict.get uuid model.graph_views
            |> Maybe.map (\gv -> ( uuid, nodeMoveDecoder gv host_coords uuid, Nothing ))
          ( Just uuid, DraggingNode _ host_coords :: _ ) ->
            AutoDict.get uuid model.graph_views
            |> Maybe.map
                (\gv ->
                  ( uuid
                  , nodeMoveDecoder gv host_coords uuid
                  , Just (D.succeed (GraphViewMsg uuid StopDraggingNode))
                  )
                )
          _ ->
            Nothing
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    panSubscription =
      pan
        ( D.decodeValue panDecoder
          >> Result.withDefault (CrashWithMessage "Invalid pan message from JavaScript")
        )
    stopPanSubscription =
      stopPan
        ( D.decodeValue stopPanDecoder
          >> Result.withDefault (CrashWithMessage "Invalid stop-pan message from JavaScript")
        )
    keyboardSubscription =
      BE.onKeyDown (keyboardDecoder model)
    resizeSubscription =
      BE.onResize
        (\w h ->
          OnResize { w = toFloat w, h = toFloat h }
          -- |> Debug.log "Raw resize values"
        )
    splitterSubscriptions =
      case splitterDecoders model of
        Just (moveDecoder, upDecoder) ->
          [ BE.onMouseMove moveDecoder
          , BE.onMouseUp upDecoder
          ]
        Nothing ->
          []
    nodeMoveSubscriptions =
      nodeMoveDecoders model
      |> List.map
          (\( _, moveDecoder, upDecoder ) ->
            Sub.batch
              (BE.onMouseMove moveDecoder
                :: (upDecoder |> Maybe.map BE.onMouseUp |> Maybe.toList)
              )
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


view : Model -> Html Msg
view model =
  div
    []
    [ case Q.mostRecentInteraction model of
        Just (Just _, EditingConnection {connection} props) ->
          UI.viewConnectionEditor model connection props
        Just (Just _, SplittingNode data props) ->
          UI.viewNodeSplitInterface model data props
        Just (Nothing, DeletingPackage _ props) ->
          UI.viewPackageDeletionWarning props model
        _ ->
          UI.viewMainInterface model
    , div
        [ HA.class "debug gv-size" ]
        [ text <| String.fromInt <| AutoDict.size model.graph_views ]
    , div
        [ HA.class "debug interactionsdict" ]
        [ UI.viewInteractionsDictSummary model.interactionsDict ]
    ]