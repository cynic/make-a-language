module ChoosingDestination_tests exposing (suite)
{-| Batch 4: choosing a destination for a new link, at the update level.

    Flow: click a node (`SelectNode`) → `ChoosingDestinationFor` with an
    `ExistingNode` destination; mouse movement (`MoveNode`) either locks on
    to a nearby node (within 36 SVG units) or swings out to a phantom node;
    clicking (`SelectSpace` / `SelectNode`) confirms the destination and
    opens the connection editor; `Escape` cancels.
-}
import AutoDict
import AutoSet
import Data exposing
  ( ConnectionTarget(..)
  , GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  , PossibleDestination(..)
  , transitionToString
  )
import Dict
import Expect
import Graph exposing (NodeId)
import Queries as Q
import Test exposing (..)
import UILibrary as U
import UILogic as UI


{-| Three nodes in a row at SVG (0,0), (100,0), (200,0); 190×190
    host/guest, no pan — so one host pixel is one SVG unit and node 0 sits
    at client (95,95).
-}
chooseModel : Model
chooseModel =
  U.defaultModel
  |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ), ( 1, 100, 0 ), ( 2, 200, 0 ) ])
  |> U.mapMainView
      (\gv ->
        { gv
          | host = { w = 190, h = 190 }
          , guest = { x = -95, y = -95, w = 190, h = 190 }
          , pan = ( 0, 0 )
        }
      )


emptyConn : Data.Connection
emptyConn =
  AutoSet.empty transitionToString


click0 : Msg
click0 =
  GraphViewMsg chooseModel.mainGraphView (SelectNode 0 { x = 95, y = 95 })


mv : Coordinate_ -> Msg
mv c =
  GraphViewMsg chooseModel.mainGraphView (MoveNode c)


type alias Coordinate_ =
  { x : Float, y : Float }


selected0 : Model
selected0 =
  U.step1 click0 chooseModel


{-| The top-of-stack interaction of the main view.
-}
topOf : Model -> Maybe InteractionState
topOf m =
  Q.peekInteraction (Just m.mainGraphView) m.interactionsDict


{-| The destination node id of an in-progress choice, if any.
-}
destinationId : Model -> Maybe NodeId
destinationId m =
  topOf m
  |> Maybe.map
      (\i ->
        case i of
          ChoosingDestinationFor _ (ExistingNode id _) _ ->
            id

          ChoosingDestinationFor _ (NewNode id _) _ ->
            id

          _ ->
            -1
      )


phantomCoord : Model -> Maybe Coordinate_
phantomCoord m =
  topOf m
  |> Maybe.andThen
      (\i ->
        case i of
          ChoosingDestinationFor _ (NewNode _ c) _ ->
            Just c

          _ ->
            Nothing
      )


nodeCoord : Model -> NodeId -> Maybe ( Float, Float )
nodeCoord m id =
  U.mainView m
  |> Maybe.andThen (\gv -> Graph.get id gv.computation.graph)
  |> Maybe.map (\ctx -> ( ctx.node.label.x, ctx.node.label.y ))


nodeDrawingCoord : Model -> NodeId -> Maybe Coordinate_
nodeDrawingCoord m id =
  U.mainView m
  |> Maybe.map .drawingData
  |> Maybe.andThen (\dd -> Dict.get id dd.node_drawing)
  |> Maybe.map .coordinates


tentative : Model -> Maybe ( NodeId, NodeId )
tentative m =
  U.mainView m
  |> Maybe.map (.drawingData >> .tentative_link)
  |> Maybe.withDefault Nothing


suite : Test
suite =
  describe "Choosing destination for a node"
    [ describe "starting a choice"
        [ test "clicking a node selects it as the source with a recursive tentative link" <|
            \_ ->
              topOf selected0
              |> Expect.equal
                  (Just (ChoosingDestinationFor 0 (ExistingNode 0 emptyConn) { x = 0, y = 0 }))
        , test "a recursive tentative link appears in the drawing data" <|
            \_ ->
              U.mainView selected0
              |> Maybe.map .drawingData
              |> Maybe.map
                  (\dd ->
                    ( dd.tentative_link
                    , Dict.get ( 0, 0 ) dd.link_drawing |> Maybe.map .cardinality
                    )
                  )
              |> Expect.equal ( Just ( Just ( 0, 0 ), Just Data.Recursive ) )
        , test "capabilities become source-node-selected on the main view only" <|
            \_ ->
              let
                mainCaps =
                  U.mainViewOrFail selected0 |> .can
                explorerCaps =
                  List.head selected0.computationsExplorer
                  |> Maybe.andThen (\id -> AutoDict.get id selected0.graph_views)
                  |> Maybe.map .can
              in
                Expect.all
                  [ \_ ->
                      ( mainCaps.selectNodes, mainCaps.selectEmptySpace, mainCaps.pan )
                      |> Expect.equal ( True, True, True )
                  , \_ ->
                      explorerCaps
                      |> Maybe.map (\c -> ( c.selectNodes, c.pan ))
                      |> Expect.equal ( Just ( False, True ) )
                  ]
                    ()
        ]
    , describe "the phantom follows and locks on"
        [ test "moving away from every node creates a phantom node" <|
            \_ ->
              U.step [ click0, mv { x = 50, y = 80 } ] chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> destinationId m |> Expect.equal (Just 3)
                    , \_ -> phantomCoord m |> Expect.equal (Just { x = 50, y = 80 })
                    , \_ -> nodeDrawingCoord m 3 |> Expect.equal (Just { x = 50, y = 80 })
                    , \_ -> tentative m |> Expect.equal (Just ( 0, 3 ))
                    ]
                      ()
        , test "the phantom follows the mouse" <|
            \_ ->
              U.step [ click0, mv { x = 50, y = 80 }, mv { x = 60, y = 90 } ] chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> phantomCoord m |> Expect.equal (Just { x = 60, y = 90 })
                    , \_ -> nodeDrawingCoord m 3 |> Expect.equal (Just { x = 60, y = 90 })
                    ]
                      ()
        , test "the phantom follows the mouse when the graph is panned" <|
            \_ ->
              -- pan (7,3); node 0 at SVG (0,0) is at client (88,92).  The user
              -- clicks there and moves +50,+80 in client pixels; the phantom
              -- must appear at SVG (50,80) — exactly under the cursor.
              let
                panned =
                  U.mapMainView (\gv -> { gv | pan = ( 7, 3 ) }) chooseModel
                uid =
                  panned.mainGraphView
                clicked =
                  U.step1 (GraphViewMsg uid (SelectNode 0 { x = 88, y = 92 })) panned
                stored =
                  Q.peekInteraction (Just uid) clicked.interactionsDict
                  |> Maybe.map
                      (\i ->
                        case i of
                          ChoosingDestinationFor _ _ hc ->
                            hc

                          _ ->
                            { x = -1, y = -1 }
                      )
                movedTo client =
                  UI.translateHostCoordinates { x = 0, y = 0 } client (U.mainViewOrFail clicked)
              in
                Expect.all
                  [ \_ -> stored |> Expect.equal (Just { x = 0, y = 0 })
                  , \_ -> movedTo { x = 138, y = 172 } |> Expect.equal { x = 50, y = 80 }
                  , \_ ->
                      U.step1 (GraphViewMsg uid (MoveNode (movedTo { x = 138, y = 172 }))) clicked
                      |> phantomCoord
                      |> Expect.equal (Just { x = 50, y = 80 })
                  ]
                    ()
        , test "moving within lock-on distance (≤36) of another node locks on to it" <|
            \_ ->
              -- node 1 is at SVG (100,0); the cursor at (110,0) is 10 away
              U.step [ click0, mv { x = 50, y = 80 }, mv { x = 110, y = 0 } ] chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> destinationId m |> Expect.equal (Just 1)
                    , \_ -> nodeDrawingCoord m 3 |> Expect.equal Nothing -- phantom is gone
                    , \_ -> tentative m |> Expect.equal (Just ( 0, 1 ))
                    ]
                      ()
        , test "moving beyond lock-on distance swings back out to a phantom" <|
            \_ ->
              U.step
                [ click0, mv { x = 50, y = 80 }, mv { x = 110, y = 0 }, mv { x = 50, y = 80 } ]
                chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> destinationId m |> Expect.equal (Just 3)
                    , \_ -> nodeDrawingCoord m 3 |> Expect.equal (Just { x = 50, y = 80 })
                    ]
                      ()
        , test "hovering the same nearby node again changes nothing" <|
            \_ ->
              U.step
                [ click0, mv { x = 50, y = 80 }, mv { x = 110, y = 0 }, mv { x = 105, y = 0 } ]
                chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> destinationId m |> Expect.equal (Just 1)
                    , \_ -> tentative m |> Expect.equal (Just ( 0, 1 ))
                    ]
                      ()
        ]
    , describe "pan-and-choose"
        [ test "panning during a choice keeps the choice alive and movable" <|
            \_ ->
              U.step
                [ click0
                , mv { x = 50, y = 80 }
                , GraphViewMsg chooseModel.mainGraphView (Pan 1 0)
                , mv { x = 60, y = 90 }
                ]
                chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> U.mainViewOrFail m |> .pan |> Expect.equal ( 1, 0 )
                    , \_ -> destinationId m |> Expect.equal (Just 3)
                    , \_ -> phantomCoord m |> Expect.equal (Just { x = 60, y = 90 })
                    ]
                      ()
        ]
    , describe "clicking confirms the destination"
        [ test "clicking empty space places the phantom node and opens the connection editor" <|
            \_ ->
              let
                placed =
                  U.step [ click0, mv { x = 50, y = 80 }, GraphViewMsg chooseModel.mainGraphView SelectSpace ] chooseModel
              in
                case topOf placed of
                  Just (EditingConnection alteration props) ->
                    Expect.all
                      [ \_ -> nodeCoord placed 3 |> Expect.equal (Just ( 50, 80 ))
                      , \_ -> ( alteration.source, alteration.dest ) |> Expect.equal ( 0, 3 )
                      , \_ -> alteration.targetKind |> Expect.equal PhantomNodeNewConnection
                      , \_ -> props.mainGraph /= placed.mainGraphView |> Expect.equal True
                      , \_ -> AutoDict.member props.mainGraph placed.graph_views |> Expect.equal True
                      ]
                        ()

                  _ ->
                    Expect.fail "expected an EditingConnection interaction on top of the stack"
        , test "clicking an existing node connects to it (new connection)" <|
            \_ ->
              let
                placed =
                  U.step [ click0, mv { x = 110, y = 0 }, GraphViewMsg chooseModel.mainGraphView SelectSpace ] chooseModel
              in
                case topOf placed of
                  Just (EditingConnection alteration props) ->
                    Expect.all
                      [ \_ -> ( alteration.source, alteration.dest ) |> Expect.equal ( 0, 1 )
                      , \_ -> alteration.targetKind |> Expect.equal ExistingNodeNewConnection
                      , \_ -> AutoDict.member props.mainGraph placed.graph_views |> Expect.equal True
                      ]
                        ()

                  _ ->
                    Expect.fail "expected an EditingConnection interaction on top of the stack"
        , test "clicking any node while choosing confirms the current destination" <|
            \_ ->
              -- a second node-click is treated as "confirm": the *phantom*
              -- destination is what gets created, not the clicked node
              let
                placed =
                  U.step [ click0, mv { x = 50, y = 80 }, GraphViewMsg chooseModel.mainGraphView (SelectNode 1 { x = 185, y = 95 }) ] chooseModel
              in
                case topOf placed of
                  Just (EditingConnection alteration _) ->
                    Expect.all
                      [ \_ -> nodeCoord placed 3 |> Expect.equal (Just ( 50, 80 ))
                      , \_ -> ( alteration.source, alteration.dest ) |> Expect.equal ( 0, 3 )
                      ]
                        ()

                  _ ->
                    Expect.fail "expected an EditingConnection interaction on top of the stack"
        ]
    , describe "escaping a choice"
        [ test "escape during phantom creation cancels everything" <|
            \_ ->
              U.step
                [ click0, mv { x = 50, y = 80 }, Escape ]
                chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> topOf m |> Expect.equal Nothing
                    , \_ -> nodeDrawingCoord m 3 |> Expect.equal Nothing
                    , \_ -> tentative m |> Expect.equal Nothing
                    , \_ -> nodeCoord m 3 |> Expect.equal Nothing -- no node was created
                    ]
                      ()
        , test "escape during existing-node selection clears the tentative link" <|
            \_ ->
              U.step [ click0, Escape ] chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> tentative m |> Expect.equal Nothing
                    , \_ ->
                        U.mainView m
                        |> Maybe.map (.drawingData >> \dd -> Dict.member ( 0, 0 ) dd.link_drawing)
                        |> Expect.equal (Just False)
                    ]
                      ()
        , test "escape during existing-node selection pops the interaction and resets capabilities" <|
            \_ ->
              U.step [ click0, Escape ] chooseModel
              |> \m ->
                  Expect.all
                    [ \_ -> topOf m |> Expect.equal Nothing
                    , \_ -> m.can.escape |> Expect.equal False -- back to idle: escape is gated again
                    , \_ -> U.mainViewOrFail m |> .can |> .selectEmptySpace |> Expect.equal False -- was True mid-selection
                    , \_ -> U.mainViewOrFail m |> .can |> .selectConnections |> Expect.equal True -- was False mid-selection
                    ]
                      ()
        , test "escape in the idle state is not even permitted" <|
            \_ ->
              U.step1 Escape U.defaultModel
              |> \m ->
                  Expect.all
                    [ \_ -> m.can.escape |> Expect.equal False
                    , \_ -> AutoDict.size m.interactionsDict |> Expect.equal 0
                    ]
                      ()
        ]
    ]
