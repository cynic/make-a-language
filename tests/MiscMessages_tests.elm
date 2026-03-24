module MiscMessages_tests exposing (suite)
{-| Batch 20: the remaining small handlers and guards.

    Stop/Reset pan handlers (which complete the "Centering" checklist
    items), tool/navigator selections, the pan-in-every-area matrix, the
    undo/redo mid-interaction refusal, and the unknown-uuid no-ops.
-}
import AutoDict
import Data exposing
  ( GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  , NavigatorIcon(..)
  , PackageMsg(..)
  , ToolIcon(..)
  )
import Expect
import Graph
import GraphEditor
import Queries as Q
import Test exposing (..)
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


globalTop : Model -> Maybe InteractionState
globalTop m =
  Q.peekInteraction Nothing m.interactionsDict


panOf : Model -> Uuid -> Maybe ( Float, Float )
panOf m view_uuid =
  AutoDict.get view_uuid m.graph_views
  |> Maybe.map .pan


{-| A model whose main graph has two nodes, with node 0 dragged to (500,300)
    and some panning applied.
-}
draggedAndPanned : Model
draggedAndPanned =
  U.defaultModel
  |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ), ( 1, 300, 0 ) ])
  |> U.step
      [ GraphViewMsg U.defaultModel.mainGraphView (StartDraggingNode 0 { x = 95, y = 95 })
      , GraphViewMsg U.defaultModel.mainGraphView (MoveNode { x = 500, y = 300 })
      , GraphViewMsg U.defaultModel.mainGraphView StopDraggingNode
      ]
  |> U.mapMainView (GraphEditor.panGraphView 1 1)


nodeCoord : Model -> NodeId_ -> Maybe ( Float, Float )
nodeCoord m id =
  U.mainView m
  |> Maybe.andThen (.computation >> .graph >> Graph.get id)
  |> Maybe.map (\ctx -> ( ctx.node.label.x, ctx.node.label.y ))


type alias NodeId_ =
  Graph.NodeId


suite : Test
suite =
  describe "Miscellaneous handlers"
    [ describe "pan indicators"
        [ test "stop-pan clears the direction but keeps the offset" <|
            \_ ->
              U.step1 (GraphViewMsg draggedAndPanned.mainGraphView StopPan) draggedAndPanned
              |> \m ->
                  Expect.all
                    [ \_ -> U.mainViewOrFail m |> .activePanDirection |> Expect.equal Nothing
                      -- (each pan step is 2 units: ceiling(490/250))
                    , \_ -> U.mainViewOrFail m |> .pan |> Expect.equal ( 2, 2 )
                    ]
                        ()
        , test "reset-pan re-centres the view, even after nodes have been dragged elsewhere" <|
            \_ ->
              U.step1 (GraphViewMsg draggedAndPanned.mainGraphView ResetPan) draggedAndPanned
              |> \m ->
                  Expect.all
                    [ \_ -> U.mainViewOrFail m |> .pan |> Expect.equal ( 0, 0 )
                    , \_ -> U.mainViewOrFail m |> .activePanDirection |> Expect.equal Nothing
                    , \_ ->
                        -- the dragged node keeps its new position:
                        -- re-centring moves the viewport, not the graph
                        nodeCoord m 0 |> Expect.equal ( Just ( 500, 300 ) )
                    ]
                        ()
        ]
    , describe "selections"
        [ test "tool selection is recorded" <|
            \_ ->
              U.step1 (SelectTool MetadataToolIcon) U.defaultModel
              |> \m -> m.uiLayout.selected.bottomPanel
              |> Expect.equal MetadataToolIcon
        , test "navigator selections are recorded" <|
            \_ ->
              U.step [ SelectNavigation TestsIcon, SelectNavigation DebugIcon ] U.defaultModel
              |> \m -> m.uiLayout.selected.sideBar
              |> Expect.equal DebugIcon
        , test "the computations icon refreshes the explorer: fresh views of the same packages" <|
            \_ ->
              let
                result = U.step1 (SelectNavigation ComputationsIcon) U.defaultModel
                oldIds = U.defaultModel.computationsExplorer
              in
                Expect.all
                  [ \_ ->
                      -- same packages linked
                      List.map (packageOfView result) result.computationsExplorer
                      |> Expect.equal (List.map (packageOfView U.defaultModel) U.defaultModel.computationsExplorer)
                  , \_ ->
                      -- but the old views are gone
                      List.map (\id -> AutoDict.member id result.graph_views) oldIds
                      |> List.all identity
                      |> Expect.equal False
                  ]
                    ()
        ]
    , describe "pan in every area"
        [ test "computation explorer" <|
            \_ ->
              let
                explorerId = List.head U.defaultModel.computationsExplorer |> Maybe.withDefault U.defaultModel.mainGraphView
              in
                U.step1 (GraphViewMsg explorerId (Pan 1 0)) U.defaultModel
                |> \m -> panOf m explorerId
                |> Expect.equal (Just ( 1, 0 ))
        , test "main window" <|
            \_ ->
              U.step1 (GraphViewMsg U.defaultModel.mainGraphView (Pan 1 0)) U.defaultModel
              |> \m -> panOf m U.defaultModel.mainGraphView
              |> Expect.equal (Just ( 1, 0 ))
        , test "connection editor: main area and references" <|
            \_ ->
              let
                opened =
                  U.step1
                    (EditConnection { x = 50, y = 50 } U.defaultModel.mainGraphView 0 1 (Utility.mkConn "a"))
                    (U.defaultModel |> U.withGraph (Utility.ag "0-a-1"))
                editorIdAndRefId =
                  Q.peekInteraction (Just opened.mainGraphView) opened.interactionsDict
                  |> Maybe.map
                      (\i ->
                        case i of
                          EditingConnection _ props ->
                            ( props.mainGraph, List.head props.referenceList )

                          _ ->
                            ( opened.mainGraphView, Nothing )
                      )
              in
                case editorIdAndRefId of
                  Just ( editorId, Just refId ) ->
                    U.step
                      [ GraphViewMsg editorId (Pan 1 0)
                      , GraphViewMsg refId (Pan 1 0)
                      ]
                      opened
                      |> \m -> ( panOf m editorId, panOf m refId )
                      |> Expect.equal ( Just ( 1, 0 ), Just ( 1, 0 ) )

                  _ ->
                    Expect.fail "expected an editor view and a reference view"
        , test "splitter interface" <|
            \_ ->
              let
                m = U.defaultModel |> U.withGraph (Utility.ag "0-ab-1")
                started = U.step1 (GraphViewMsg m.mainGraphView (StartSplit 1)) m
              in
                case Q.peekInteraction (Just started.mainGraphView) started.interactionsDict of
                  Just (SplittingNode _ props) ->
                    U.step1 (GraphViewMsg props.mainGraph (Pan 1 0)) started
                    |> \r -> panOf r props.mainGraph
                    |> Expect.equal (Just ( 1, 0 ))

                  _ ->
                    Expect.fail "expected a SplittingNode interaction"
        ]
    , describe "guards"
        [ test "undo is refused while an interaction is in flight" <|
            \_ ->
              let
                committed =
                  U.step
                    [ EditConnection { x = 50, y = 50 } U.defaultModel.mainGraphView 0 1 (Utility.mkConn "a")
                    , QuickInput "a" -- a becomes final
                    , Confirm
                    ]
                    (U.defaultModel |> U.withGraph (Utility.ag "0-a-1"))
                interacting =
                  U.step1 (GraphViewMsg committed.mainGraphView (SelectNode 0 { x = 95, y = 95 })) committed
                refused =
                  U.step1 Undo interacting
              in
                Expect.all
                  [ \_ ->
                      -- an interaction is still in flight (view-keyed)
                      Q.mostRecentInteraction refused
                      |> Maybe.map (always True)
                      |> Expect.equal (Just True)
                  , \_ ->
                      -- the computation was NOT undone (still the committed one)
                      U.mainView refused
                      |> Maybe.map (.computation >> .graph >> Graph.edges >> List.length)
                      |> Expect.equal (Just 1)
                  , \_ ->
                      U.mainView refused
                      |> Maybe.map (.undoBuffer >> List.length)
                      |> Expect.equal (Just 1)
                  ]
                    ()
        , test "package messages for an unknown uuid are ignored" <|
            \_ ->
              let
                result =
                  U.step
                    [ PackageMsg (Utility.uuid_from_int 999) (UpdateTestInput "x")
                    , PackageMsg (Utility.uuid_from_int 999) (DeleteTestInput (Utility.uuid_from_int 999))
                    ]
                    U.defaultModel
              in
                Expect.equal
                  ( AutoDict.size result.packages
                  , List.map (.tests >> AutoDict.size) (AutoDict.values result.packages)
                  )
                  ( 1, [ 0 ] )
        ]
    ]


{-| The package-uuid linked to a view.
-}
packageOfView : Model -> Uuid -> Maybe Uuid
packageOfView m view_uuid =
  AutoDict.get view_uuid m.graph_views
  |> Maybe.andThen .graphPackage
