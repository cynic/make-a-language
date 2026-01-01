module UILogic exposing (..)
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
import Uuid exposing (Uuid)

expand_bounds : Float -> Bounds -> Bounds
expand_bounds n bounds =
  { bounds
    | min = { x = bounds.min.x - n, y = bounds.min.y - n }
    , max = { x = bounds.max.x + n, y = bounds.max.y + n }
  }

{-| Accepts host dimensions, view properties, and a graph, and calculates the
    appropriate guest coordinates & guest dimensions (i.e. viewport).
-}
calculateGuestDimensionsForHost : DimensionLike a -> Bool -> Graph.Graph Entity Connection -> Rectangle
calculateGuestDimensionsForHost {w, h} removePadding graph =
  let
    aspectRatio : Float
    aspectRatio =
      w / h
      -- |> Debug.log "Viewport aspect-ratio"
    raw =
      Q.bounds_for (Graph.nodeIds graph) graph
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
  in
    { x = adjusted.min.x
    , y = center_y - autoHeight / 2
    , w = adjusted.max.x - adjusted.min.x
    , h = autoHeight
    }

fitGraphViewToGraph : GraphView -> GraphView
fitGraphViewToGraph graphView =
  { graphView
  | guest =
      calculateGuestDimensionsForHost
        graphView.host
        graphView.fitClosely
        graphView.computation.graph
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
                  case Q.peekInteraction (Just k) model.interactionsDict of
                    Just (SplittingNode _ _) ->
                      whenSplittingNode
                    Just (DraggingNode _ _) ->
                      whenDraggingNode
                    Just (ChoosingDestinationFor _ _ _) ->
                      whenSourceNodeSelected v.id
                    Just (EditingConnection _ _) ->
                      whenEditingConnection
                    Just (Executing _ _) ->
                      whenExecuting
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
        case Q.mostRecentInteraction model of
          Just (_, SplittingNode _ _) ->
            whenSplittingNode
          Just (_, DraggingNode _ _) ->
            whenDraggingNode
          Just (_, ChoosingDestinationFor _ _ _) ->
            whenSourceNodeSelected
          Just (_, EditingConnection _ _) ->
            whenEditingConnection
          Just (_, Executing _ _) ->
            whenExecuting
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

makeGraphView : Uuid -> GraphViewType -> Dimension -> Bool -> PackageDict -> AutomatonGraph -> GraphView
makeGraphView id viewType dim fitClosely packages ag =
  let
    computeResult =
      case viewType of
        Unsolved ->
          { solvedGraph = ag
          , simulation = Force.simulation []
          , forces = []
          }
        SolvedByCoordinateForces ->
          GraphEditor.computeGraphFully GraphEditor.coordinateForces ag
        SolvedBySpreadOutForces ->
          GraphEditor.computeGraphFully GraphEditor.spreadOutForces ag
  in
    { id = id
    , computation = computeResult.solvedGraph
    , graphPackage = Nothing
    , fitClosely = fitClosely
    , host = dim
    , guest =
        calculateGuestDimensionsForHost
          dim
          fitClosely
          computeResult.solvedGraph.graph
    , pan = ( 0, 0 )
    , activePanDirection = Nothing
    , disconnectedNodes = Set.empty
    , properties = nilViewProperties
    , drawingData =
        { link_drawing = GraphEditor.linkDrawingForPackage computeResult.solvedGraph packages
        , node_drawing = GraphEditor.nodeDrawingForPackage computeResult.solvedGraph id
        , tentative_link = Nothing
        , graphReferenceDescriptions = Q.descriptionsForPackages packages
        , highlighted_links = Set.empty
        , lowlighted_links = Set.empty
        }
    , undoBuffer = []
    , redoBuffer = []
    }

centerAndHighlight : List (NodeId, NodeId) -> GraphView -> GraphView
centerAndHighlight links graph_view =
  let
    bounds =
      Q.bounds_for
        (List.foldl (\(a, b) acc -> a :: b :: acc) [] links)
        graph_view.computation.graph
    inner_padding = 60
    adjusted =
      expand_bounds inner_padding bounds
    aspect_ratio =
      graph_view.host.w / graph_view.host.h
    last_node =
      List.last links
      |> Maybe.map Tuple.second
      |> Maybe.withDefault (graph_view.computation.root)
    linkSet = Set.fromList links
    determine_dimensions : Rectangle -> Rectangle
    determine_dimensions orig_rect =
      if orig_rect.w / graph_view.host.w < 0.25 then
        -- expand to at least this amount.
        let
          new_w = 0.25 * graph_view.host.w
          -- so, how much do I need to expand on either side?
          diff = (new_w - orig_rect.w) / 2
        in
          { x = orig_rect.x - diff
          , y = orig_rect.y
          , w = new_w
          , h = new_w / aspect_ratio
          }
      else
        orig_rect
    (highlight, lowlight) =
      Dict.foldl (\(src, dest) _ (h, l) ->
        if Set.member (src, dest) linkSet then
          -- this is a link to highlight.
          ( Set.insert (src, dest) h, l )
        else if src == last_node then
          ( h, l ) -- neither highlight nor lowlight
        else
          ( h, Set.insert (src, dest) l )
      )
      (Set.empty, Set.empty)
      graph_view.drawingData.link_drawing

  in
    { graph_view
      | drawingData =
          let drawingData = graph_view.drawingData in
            { drawingData
              | highlighted_links = highlight
              , lowlighted_links = lowlight
            }
      , guest =
          determine_dimensions <|
            Rectangle adjusted.min.x adjusted.min.y
              (adjusted.max.x - adjusted.min.x)
              ((adjusted.max.x - adjusted.min.x) / aspect_ratio)
    }

{-| When either the sidebar or the bottom-panel have changed dimensions, this should be
    called to figure out what changes need to be made to any of the other dimensions.
-}
recalculate_uistate : UILayout -> UILayout
recalculate_uistate ({dimensions} as ui) =
  let
    visible_sidebar_width_plus_splitter =
      if ui.open.sideBar then dimensions.sideBar.w + 8 + 48 else 0
    visible_panel_height_plus_splitter =
      if ui.open.bottomPanel then dimensions.bottomPanel.h + 8 else 0
  in
  { ui
    | dimensions =
        { dimensions
          | bottomPanel =
              { w = dimensions.viewport.w - visible_sidebar_width_plus_splitter
              , h = dimensions.bottomPanel.h
              }
          , mainEditor =
              { w = dimensions.viewport.w - visible_sidebar_width_plus_splitter
              , h = dimensions.viewport.h - visible_panel_height_plus_splitter
              }
        }
  }

sidebarGraphDimensions : UILayout -> Dimension
sidebarGraphDimensions uiLayout =
  Dimension
    ( uiLayout.dimensions.sideBar.w - 20 )
    ( 9/16 * ( uiLayout.dimensions.sideBar.w - 20 ))

updateMainEditorDimensions : Model -> Model
updateMainEditorDimensions ({uiLayout, mainGraphView, computationsExplorer} as model) =
  let
    updateMain : GraphView -> GraphView
    updateMain graph_view =
      { graph_view
        | host = uiLayout.dimensions.mainEditor
        , guest =
            calculateGuestDimensionsForHost
              uiLayout.dimensions.mainEditor
              False
              graph_view.computation.graph
      }
    updateSidebar : GraphView -> GraphView
    updateSidebar graph_view =
      let
        sidebarDimensions =
          sidebarGraphDimensions uiLayout
      in
        { graph_view
          | host = sidebarDimensions
          , guest = 
              calculateGuestDimensionsForHost
                sidebarDimensions
                True
                graph_view.computation.graph          
        }
    sidebarIds =
      AutoSet.fromList Uuid.toString computationsExplorer
  in
    -- On resize, I must update the dimensions for the graph view that
    -- is displayed in the "main" editor section as well.
    { model
      | graph_views =
          AutoDict.map
            (\_ graph_view ->
              if graph_view.id == mainGraphView then
                updateMain graph_view
              else if uiLayout.open.sideBar && AutoSet.member graph_view.id sidebarIds then
                updateSidebar graph_view
              else
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
        dimensions.sideBar.w
    panel_height =
      if movement == UpDown then
        clamp constants.toolsPanelHeight.min constants.toolsPanelHeight.max (dimensions.viewport.h - 8 - coord)
      else
        dimensions.bottomPanel.h
  in
    { ui
      | dimensions =
          { dimensions
            | sideBar =
                Dimension sidebar_width dimensions.sideBar.h
            , bottomPanel =
                Dimension dimensions.bottomPanel.w panel_height
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

resizeViewport : Dimension -> Model -> Model
resizeViewport {w, h} ({uiLayout, uiConstants} as model) =
  let
    dim = uiLayout.dimensions
    constants : UIConstants
    constants =
      { uiConstants
        | sideBarWidth =
            let sbw = uiConstants.sideBarWidth in
            { sbw
              | max = w / 2 - 60
              , initial = clamp sbw.min (w / 2 - 60) (dim.sideBar.w)
            }
      , toolsPanelHeight =
          let tph = uiConstants.toolsPanelHeight in
          { tph
            | max = h / 2 - 40
            , initial = clamp tph.min (h / 2 - 40) (dim.bottomPanel.h)
          }
      }
    
    state : UILayout
    state =
      { uiLayout
        | dimensions =
            { dim
              | viewport = Dimension w h
              , sideBar =
                  { w = clamp constants.sideBarWidth.min constants.sideBarWidth.max (dim.sideBar.w)
                  , h = dim.sideBar.h
                  }
              , bottomPanel =
                  { w = dim.bottomPanel.w
                  , h = clamp constants.toolsPanelHeight.min constants.toolsPanelHeight.max (dim.bottomPanel.h)
                  }
            }
      }
      |> recalculate_uistate
  in
    { model
      | uiLayout = state
      , uiConstants = constants
    }

escapeConnectionEditor : Uuid -> ConnectionAlteration -> List Uuid -> Model -> Model
escapeConnectionEditor uuid {source, dest, targetKind} viewsToRemove model =
  case {- Debug.log "targetKind" -} targetKind of
    PhantomNodeNewConnection ->
      C.removeViews viewsToRemove model
      |> C.updateGraphView uuid
        ( C.mapGraph (Graph.remove dest)
          >> C.mapDrawingData
            (\dd ->
              { dd
                | link_drawing = Dict.remove (source, dest) dd.link_drawing
                , node_drawing = Dict.remove dest dd.node_drawing
                , tentative_link = Nothing
              }
            )
        )
    ExistingNodeNewConnection ->
      C.removeViews viewsToRemove model
      |> C.updateGraphView uuid
        ( C.mapGraph
            ( Graph.update dest
                (Maybe.map (\destCtx ->
                  { destCtx | incoming = IntDict.remove source destCtx.incoming }
                ))
            )
          >> C.mapDrawingData
            (\dd ->
              { dd
                | link_drawing = Dict.remove (source, dest) dd.link_drawing
                , tentative_link = Nothing
              }
            )
        )
    ExistingNodeExistingConnection ->
      C.removeViews viewsToRemove model 
      |> C.updateGraphView uuid
        ( C.mapDrawingData (\dd -> { dd | tentative_link = Nothing }) )

packagesToGraphViews : Dimension -> Model -> List GraphPackage -> (List GraphView, Model)
packagesToGraphViews dim model list =
  List.foldl
    (\pkg (acc, model_) ->
      let
        ( id, model__ ) =
          getUuid model_
        gv =
          makeGraphView id SolvedByCoordinateForces
            dim True model_.packages pkg.computation
          |> C.linkGraphViewToPackage model_.packages pkg.packageIdentifier
      in
        ( gv :: acc , C.upsertGraphView gv model__ )
    )
    ( [], model )
    ( List.sortBy (.created >> Time.posixToMillis) list )

{-| Expensive. Refresh all computations. -}
refreshComputationsList : Model -> Model
refreshComputationsList model =
  let
    (computation_views, updated_model) =
      packagesToGraphViews (sidebarGraphDimensions model.uiLayout) model (AutoDict.values model.packages)
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

{-| Given the host-space coordinates of a node, and that same node's coordinates in
    the guest-space, find the (left, top) of the SVG element in host-coordinates.
    By transmitting host-space coordinates only when needed—e.g. on click—we reduce the
    amount of messages going to `update`, and we reduce our reliance on ports.
-}
nodeHostCoordToHostCoord : Coordinate -> Coordinate -> GraphView -> Coordinate
nodeHostCoordToHostCoord node_coord svg_coord {host, guest, pan} =
  let
    -- find out distance from the left edge of the SVG, as SVG coordinates
    ( pan_x, pan_y ) = pan
    from_left =
      (svg_coord.x - pan_x) - guest.x
      -- |> Debug.log "Distance from left-edge to node (in SVG coordinates)"
    -- find out how many SVG coordinates are in a host coordinate
    x_host_per_svg =
      host.w / guest.w
      -- |> Debug.log "SVG px per host px"
    host_on_left =
      from_left * x_host_per_svg
      -- |> Debug.log "Distance from left-edge to node (in Host coordinates)"
    host_left =
      node_coord.x - host_on_left
      -- |> Debug.log "Host left edge is at"
    from_top =
      (svg_coord.y - pan_y) - guest.y
    y_host_per_svg =
      host.h / guest.h
    host_on_top =
      from_top * y_host_per_svg
    host_top =
      node_coord.y - host_on_top
  in
    { x = host_left, y = host_top }
    -- |> Debug.log "Left/Top of container"

calculate_host_coord : Uuid -> Model -> Coordinate -> NodeId -> Maybe Coordinate
calculate_host_coord uuid model node_host_coord nodeId =
  AutoDict.get uuid model.graph_views
  |> Maybe.andThen (\gv ->
    Maybe.combineSecond
      ( gv
      , Graph.get nodeId gv.computation.graph
        |> Maybe.map
          (\ctx ->
            { x = ctx.node.label.x, y = ctx.node.label.y }
            -- |> Debug.log "SVG coordinates of node"
          )
      )
  )
  |> Maybe.map
    (\(gv, svg_coord) ->
      nodeHostCoordToHostCoord
        ( node_host_coord
          -- |> Debug.log "Host coordinates of node"
        )
        svg_coord
        gv
    )

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
    C.replaceInteraction key_uuid
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
        C.replaceInteraction key_uuid
          ( EditingConnection alteration { props | editingMode = CharacterInput } )
          model
      else
        C.replaceInteraction key_uuid
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
      C.replaceInteraction key_uuid
        ( EditingConnection alteration { props | editingMode = GraphReferenceSearch "" } )
        model
    _ ->
      C.replaceInteraction key_uuid
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
          AutoSet.insert (Transition False acceptCondition) resultSet
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

editConnection : Maybe Uuid -> Coordinate -> ConnectionAlteration -> Model -> Model
editConnection view_uuid {x,y} ({source, dest, connection} as alteration) model =
  packagesToGraphViews { w = 430, h = 3/6 * 430} model (AutoDict.values model.packages)
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
            (id, model__) =
              getUuid model_
            main_view =
              makeGraphView id Unsolved { w = 250, h = 250 } True model__.packages ag
            solidified_model =
              C.upsertGraphView main_view model__
              |> C.convertTentativeLinkToPermanent main_view.id
          in
            (List.map .id graph_views, main_view.id, solidified_model)
          )
        |> Maybe.withDefault ( List.map .id graph_views, model.mainGraphView, model_ )
      )
  |>  (\(uuids, main_uuid, model_) ->
        let
          stackModify =
            case Q.peekInteraction view_uuid model_.interactionsDict of
              Just (EditingConnection _ _) -> C.replaceInteraction
              _ -> C.pushInteractionForStack
        in
          C.updateGraphView main_uuid (centerAndHighlight [ (source, dest) ]) model_
          |> stackModify view_uuid
            ( EditingConnection alteration
                { referenceList = uuids
                , shownList = uuids
                , mainGraph = main_uuid
                , editingMode = CharacterInput
                }
            )
      )
  |> setProperties

startSplit : Uuid -> GraphView -> Graph.NodeContext Entity Connection -> Model -> Model
startSplit uuid graph_view nodeContext model =
  let
    (id, model_) =
      getUuid model
    main_view =
      makeGraphView id
        Unsolved { w = 250, h = 250 }
        True model_.packages
        graph_view.computation
    edges_in =
      IntDict.keys nodeContext.incoming
      |> List.map (\to -> (to, nodeContext.node.id))
  in
    C.upsertGraphView main_view model_
    |> C.pushInteractionForStack (Just uuid)
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
    |> C.updateGraphView main_view.id (centerAndHighlight edges_in)
    |> setProperties

beginDeletionInteraction : Uuid -> List Uuid -> GraphPackage -> Model -> Model
beginDeletionInteraction package_uuid affected package model =
  -- Erk; it depends on whether you're okay with the consequences.
  let
    all_packages : List (Uuid, AutoSet.Set String Uuid)
    all_packages = Q.packagesAndRefs model
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
    (id, model_) =
      getUuid model
    mainGraph =
      makeGraphView id SolvedByCoordinateForces
        { w = model.uiLayout.dimensions.viewport.w / 3
        , h = model.uiLayout.dimensions.viewport.h / 3
        }
        True model.packages package.computation
    (directViews, model__) =
      affected
      |> List.filterMap (\uuid -> AutoDict.get uuid model.packages)
      |> packagesToGraphViews 
          { w = model.uiLayout.dimensions.viewport.w / 7
          , h = model.uiLayout.dimensions.viewport.h / 7
          }
          (C.upsertGraphView mainGraph model_)
    (indirectViews, model___) =
      indirect
      |> List.filterMap (\uuid -> AutoDict.get uuid model.packages)
      |> packagesToGraphViews 
          { w = model.uiLayout.dimensions.viewport.w / 7
          , h = model.uiLayout.dimensions.viewport.h / 7
          }
          model__
    props =
      { affectedPackages = affected
      , indirectlyAffectedPackages = indirect
      , mainGraph = mainGraph.id
      , directViews = directViews |> List.map .id
      , indirectViews = indirectViews |> List.map .id
      }
  in
    C.pushInteractionForStack Nothing (DeletingPackage package_uuid props) model___
