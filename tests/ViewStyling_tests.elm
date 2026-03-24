module ViewStyling_tests exposing (suite)
{-| Batch 12: styling, at whatever level each part can be pinned.

    SVG class attributes cannot be matched by `Test.Html.Selector`
    (`Selector.attribute "class"` is special-cased into a property-based
    matcher that only ever reads the HTML `className`), so the class
    *computation* is pinned through the extracted pure functions
    (`GraphEditor.nodeClassFor` / `linkClassFor` /
    `characterTransitionClass`), and the DOM is pinned where the DOM is
    observable: the `d` attributes of rendered link paths, and the number
    of rendered nodes.  Queries against the DOM are scoped to the main
    graph view by its `graph-id` attribute.
-}
import Data exposing
  ( GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  )
import AutoSet
import Dict
import Executing_tests
import Expect
import GoldenGeometry
import Graph exposing (NodeId)
import GraphEditor
import Html.Attributes
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UILibrary as U
import Uuid
import Utility


{-| The main graph view's DOM subtree.
-}
inMain : Model -> Query.Single Msg
inMain m =
  U.query m
  |> Query.find
      [ Selector.attribute
          (Html.Attributes.attribute "graph-id" (Uuid.toString m.mainGraphView))
      ]


{-| A model whose main graph has a straight link, a curved (bidirectional)
    pair, and a self-loop.
-}
lineModel : Model
lineModel =
  U.defaultModel
  |> U.withGraph
      (GoldenGeometry.mkGraph
        [ ( 0, 0, 0 ), ( 1, 300, 0 ), ( 2, 600, 100 ) ]
        [ ( 0, 1, "a" ), ( 1, 0, "b" ), ( 2, 2, "c" ) ]
      )


modelPathStrings : Model -> List String
modelPathStrings m =
  U.mainView m
  |> Maybe.map
      (.drawingData
        >> .link_drawing
        >> Dict.values
        >> List.map (.pathBetween >> .pathString)
      )
  |> Maybe.withDefault []


{-| The class list of every node of the main view, in node-id order.
-}
nodeClasses : Model -> List ( NodeId, List String )
nodeClasses m =
  U.mainView m
  |> Maybe.map
      (\gv ->
        Dict.toList gv.drawingData.node_drawing
        |> List.map (\( id, d ) -> ( id, GraphEditor.nodeClassFor id gv.drawingData d ))
      )
  |> Maybe.withDefault []


{-| The class list of every drawn link of the main view, in (from,to) order.
-}
linkClasses : Model -> List ( ( NodeId, NodeId ), List String )
linkClasses m =
  U.mainView m
  |> Maybe.map
      (\gv ->
        Dict.toList gv.drawingData.link_drawing
        |> List.map (\( key, _ ) -> ( key, GraphEditor.linkClassFor gv.drawingData key ))
      )
  |> Maybe.withDefault []


{-| The label class lists that the connection editor flow will render:
    one per transition of every drawn link.
-}
labelClasses : Model -> List (List String)
labelClasses m =
  U.mainView m
  |> Maybe.map
      (\gv ->
        Dict.values gv.drawingData.link_drawing
        |> List.concatMap
            (\link ->
              AutoSet.toList link.connection
                |> List.map (\t -> GraphEditor.characterTransitionClass t.isFinal)
            )
      )
  |> Maybe.withDefault []


{-| The phantom flow on `0-a-1`: node 0 selected, phantom node swung out
    to (50,80).
-}
phantomModel : Model
phantomModel =
  U.defaultModel
  |> U.withGraph (Utility.ag "0-a-1")
  |> U.step
      [ GraphViewMsg U.defaultModel.mainGraphView (SelectNode 0 { x = 95, y = 95 })
      , GraphViewMsg U.defaultModel.mainGraphView (MoveNode { x = 50, y = 80 })
      ]


{-| The same flow, confirmed with a final `a` transition.
-}
confirmedPhantom : Model
confirmedPhantom =
  U.step
    [ GraphViewMsg U.defaultModel.mainGraphView (SelectNode 0 { x = 95, y = 95 })
    , GraphViewMsg U.defaultModel.mainGraphView (MoveNode { x = 50, y = 80 })
    , GraphViewMsg U.defaultModel.mainGraphView SelectSpace
    , QuickInput "a"
    , QuickInput "a" -- a becomes final
    , Confirm
    ]
    (U.defaultModel |> U.withGraph (Utility.ag "0-a-1"))


suite : Test
suite =
  describe "Styling"
    [ describe "wiring the drawing into the DOM"
        [ test "every modelled link is rendered with exactly its pathString (straight, curved, recursive)" <|
            \_ ->
              Expect.all
                (List.map
                  (\s ->
                    \_ ->
                      inMain lineModel
                      |> Query.findAll
                          [ Selector.tag "path"
                          , Selector.attribute (Html.Attributes.attribute "d" s)
                          ]
                      -- background + foreground path per link
                      |> Query.count (Expect.equal 2)
                  )
                  (modelPathStrings lineModel)
                )
                ()
        , test "golden pathStrings pin the exact geometry" <|
            \_ ->
              GoldenGeometry.computed
              |> Expect.equal GoldenGeometry.golden
        , test "all nodes are displayed" <|
            \_ ->
              let
                m =
                  U.defaultModel
                    |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ), ( 1, 300, 0 ), ( 2, 600, 0 ) ])
              in
                inMain m
                |> Query.findAll [ Selector.tag "circle" ]
                |> Query.count (Expect.equal 3)
        ]
    , describe "phantom styling"
        [ test "the phantom node and the selected source carry the right classes" <|
            \_ ->
              nodeClasses phantomModel
              |> Expect.equal
                  [ ( 0, [ "graph-node", "selected", "start" ] )
                  , ( 1, [ "graph-node" ] )
                  , ( 2, [ "graph-node", "phantom", "terminal" ] )
                  ]
        , test "the tentative link carries the phantom class; the original link stays drawn" <|
            \_ ->
              -- the real link (0,1) is unrelated to the tentative choice and
              -- remains drawn; only the tentative self-link (0,0) was
              -- replaced by the phantom (0,2)
              linkClasses phantomModel
              |> Expect.equal
                  [ ( ( 0, 1 ), [ "link" ] )
                  , ( ( 0, 2 ), [ "link", "phantom" ] )
                  ]
        ]
    , describe "final styling after confirming a phantom node"
        [ test "the committed link is plain (no phantom class)" <|
            \_ ->
              linkClasses confirmedPhantom
              |> Expect.equal
                  [ ( ( 0, 1 ), [ "link" ] )
                  , ( ( 0, 2 ), [ "link" ] )
                  ]
        , test "the committed transition is final, the old one non-final" <|
            \_ ->
              labelClasses confirmedPhantom
              |> List.sort
              |> Expect.equal
                  [ [ "transition", "text", "final" ]
                  , [ "transition", "text", "nonfinal" ]
                  ]
        , test "node classes reflect the committed state" <|
            \_ ->
              nodeClasses confirmedPhantom
              |> Expect.equal
                  [ ( 0, [ "graph-node", "start" ] )
                  , ( 1, [ "graph-node" ] )
                    -- the committed node now has a final transition in
                  , ( 2, [ "graph-node", "terminal" ] )
                  ]
        ]
    , describe "highlight/lowlight classes (execution)"
        [ test "executed links carry the highlight class" <|
            \_ ->
              -- one step of execution: exactly one link is highlighted.
              -- Note: the *lowlight* class is looked up on the REVERSED
              -- pair in `linkClassFor` (off-path links are dimmed on their
              -- incoming side, and only when the reverse link is drawn),
              -- so with the batch-7 graph no lowlight class appears here —
              -- pinned as observed.
              let
                m = U.step1 Step Executing_tests.execModel
              in
                U.mainView m
                |> Maybe.map
                    (\gv ->
                      Dict.keys gv.drawingData.link_drawing
                      |> List.map (\k -> ( k, GraphEditor.linkClassFor gv.drawingData k ))
                    )
                |> Maybe.map
                    (\classes ->
                      ( List.filter (\( _, c ) -> List.member "highlight" c) classes |> List.length
                      , List.filter (\( _, c ) -> List.member "lowlight" c) classes |> List.length
                      , List.filter (\( _, c ) -> List.member "link" c) classes |> List.length
                      )
                    )
                |> Expect.equal ( Just ( 1, 0, 4 ) )
        ]
    ]
