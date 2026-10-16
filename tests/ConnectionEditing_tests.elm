module ConnectionEditing_tests exposing (suite)
{-| Batch 6: editing a connection, at the update level.

    Flow: an `EditConnection` message (or a destination choice) opens
    `EditingConnection`; typing characters cycles each transition through
    on → final(priority) → off; `Confirm` commits the altered connection
    into the graph; a second `Confirm` optimises the graph (round-trip),
    which removes edges left with empty connections.
-}
import AutoDict
import AutoSet
import Data exposing
  ( AcceptVia(..)
  , Connection
  , ConnectionTarget(..)
  , GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  , Transition
  , transitionToString
  )
import Dict
import Expect
import Graph
import Graph exposing (NodeId)
import IntDict
import Queries as Q
import Set
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


{-| A graph with a single edge: `0-a-1` (a, non-final).
-}
editModel : Model
editModel =
  U.defaultModel
  |> U.withGraph (Utility.ag "0-a-1")


uid : Uuid
uid =
  editModel.mainGraphView


openEditor : Msg
openEditor =
  EditConnection { x = 50, y = 50 } uid 0 1 (Utility.mkConn "a")


editor : Model
editor =
  U.step1 openEditor editModel


plainA : Transition
plainA =
  Transition False (ViaCharacter 'a')


plainB : Transition
plainB =
  Transition False (ViaCharacter 'b')


finalA : Transition
finalA =
  Transition True (ViaCharacter 'a')


topOf : Model -> Maybe InteractionState
topOf m =
  Q.peekInteraction (Just m.mainGraphView) m.interactionsDict


connString : Connection -> List String
connString conn =
  AutoSet.toList conn
  |> List.map transitionToString
  |> List.sort


{-| The connection of the in-progress edit, if any.
-}
editedConnection : Model -> Maybe (List String)
editedConnection m =
  topOf m
  |> Maybe.map
      (\i ->
        case i of
          EditingConnection alteration _ ->
            connString alteration.connection

          _ ->
            [ "NOT EDITING" ]
      )


{-| The committed connection `src → dest` in the main view's graph.
-}
edgeConnection : Model -> NodeId -> NodeId -> List String
edgeConnection m src dest =
  U.mainView m
  |> Maybe.andThen (.computation >> .graph >> Graph.get dest)
  |> Maybe.map
      (\ctx ->
        ctx.incoming
        |> IntDict.get src
        |> Maybe.withDefault (AutoSet.empty transitionToString)
        |> connString
      )
  |> Maybe.withDefault [ "NO EDGE" ]


suite : Test
suite =
  describe "Editing a connection"
    [ describe "opening the editor"
        [ test "EditConnection pushes EditingConnection for the existing edge" <|
            \_ ->
              case topOf editor of
                Just (EditingConnection alteration props) ->
                  Expect.all
                    [ \_ -> ( alteration.source, alteration.dest ) |> Expect.equal ( 0, 1 )
                    , \_ -> alteration.targetKind |> Expect.equal ExistingNodeExistingConnection
                    , \_ -> connString alteration.connection |> Expect.equal ( connString (Utility.mkConn "a") )
                    , \_ -> props.mainGraph /= uid |> Expect.equal True
                    ]
                    ()

                _ ->
                    Expect.fail "expected an EditingConnection interaction on top of the stack"
        , test "an editor view is created and typing is permitted" <|
            \_ ->
              let
                interfaceId =
                  case topOf editor of
                    Just (EditingConnection _ props) ->
                      props.mainGraph

                    _ ->
                      uid
              in
                Expect.all
                  [ \_ -> AutoDict.member interfaceId editor.graph_views |> Expect.equal True
                  , \_ -> editor.can.acceptCharacters |> Expect.equal True
                  , \_ -> editor.can.escape |> Expect.equal True
                  ]
                    ()
        , test "the edited edge is highlighted in the editor view" <|
            \_ ->
              let
                interfaceId =
                  case topOf editor of
                    Just (EditingConnection _ props) ->
                      props.mainGraph

                    _ ->
                      uid
              in
                AutoDict.get interfaceId editor.graph_views
                |> Maybe.map (.drawingData >> .highlighted_links >> Set.member ( 0, 1 ))
                |> Expect.equal (Just True)
        ]
    , describe "cycling input: on → final → off"
        [ test "typing a cycles the transition through its three states" <|
            \_ ->
              let
                e1 = U.step1 (QuickInput "a") editor -- becomes final
                e2 = U.step1 (QuickInput "a") e1 -- removed
                e3 = U.step1 (QuickInput "a") e2 -- back to plain on
                e4 = U.step1 (QuickInput "a") e3 -- and final again
              in
                Expect.all
                  [ \_ -> editedConnection editor |> Maybe.map List.length |> Expect.equal (Just 1)
                  , \_ -> editedConnection e1 |> Maybe.map (List.member (transitionToString finalA)) |> Expect.equal (Just True)
                  , \_ -> editedConnection e2 |> Expect.equal (Just [])
                  , \_ -> editedConnection e3 |> Maybe.map (List.member (transitionToString plainA)) |> Expect.equal (Just True)
                  , \_ -> editedConnection e4 |> Maybe.map (List.member (transitionToString finalA)) |> Expect.equal (Just True)
                  ]
                    ()
        , test "cycling one character leaves the others alone" <|
            \_ ->
              U.step1 (EditConnection { x = 50, y = 50 } uid 0 1 (Utility.mkConn "ab")) editModel
              |> U.step1 (QuickInput "a")
              |> editedConnection
              |> Maybe.map
                  (\cs ->
                    ( List.member (transitionToString finalA) cs
                    , List.member (transitionToString plainA) cs
                    , List.member "b" cs
                    )
                  )
              |> Expect.equal ( Just ( True, False, True ) )
        , test "ToggleConnectionTransition cycles identically" <|
            \_ ->
              U.step1 (ToggleConnectionTransition (ViaCharacter 'a')) editor
              |> editedConnection
              |> Maybe.map (List.member (transitionToString finalA))
              |> Expect.equal (Just True)
        ]
    , describe "space transitions are visible, not blank"
        [ test "typing a space adds a space transition" <|
            \_ ->
              U.step [ openEditor, QuickInput " " ] editModel
              |> editedConnection
              |> Maybe.map (List.member (transitionToString (Transition False (ViaCharacter ' '))))
              |> Expect.equal (Just True)
        , test "typing a space three times cycles it back off again" <|
            \_ ->
              U.step [ openEditor, QuickInput " ", QuickInput " ", QuickInput " " ] editModel
              |> editedConnection
              |> Expect.equal (Just [ transitionToString plainA ])
        , test "the set display renders a space as ␣, not as blank" <|
            \_ ->
              -- open the editor on an empty connection, so that the space
              -- we type is the only set item on display.
              U.step1 (EditConnection { x = 50, y = 50 } uid 0 1 (Utility.mkConn "")) editModel
              |> U.step1 (QuickInput " ")
              |> U.query
              |> Query.find [ Selector.class "set-item", Selector.class "character" ]
              |> Query.has [ Selector.text "␣" ]
        ]
    , describe "committing the edit"
        [ test "confirm reflects the updated connection (final form) in the graph" <|
            \_ ->
              let
                result =
                  U.step [ openEditor, QuickInput "a", Confirm ] editModel
              in
                Expect.all
                  [ \_ -> edgeConnection result 0 1 |> Expect.equal [ transitionToString finalA ]
                  , \_ -> topOf result |> Expect.equal Nothing
                  , \_ -> AutoDict.size result.graph_views |> Expect.equal 2 -- editor view is gone
                  ]
                    ()
        , test "the link drawing reflects the updated connection after confirm" <|
            \_ ->
              U.step [ openEditor, QuickInput "a", Confirm ] editModel
              |> U.mainView
              |> Maybe.map
                  (.drawingData
                    >> .link_drawing
                    >> Dict.get ( 0, 1 )
                    >> Maybe.map (.connection >> connString)
                  )
              |> Expect.equal (Just (Just [ transitionToString finalA ]))
        , test "removing all transitions empties the link in the graph" <|
            \_ ->
              let
                result =
                  U.step [ openEditor, QuickInput "a", QuickInput "a", Confirm ] editModel
              in
                Expect.all
                  [ \_ -> edgeConnection result 0 1 |> Expect.equal []
                  , \_ -> topOf result |> Expect.equal Nothing
                  ]
                    ()
        , test "a second confirm optimises: emptied link and its disconnected node are removed" <|
            \_ ->
              -- removing every transition of a link leaves the destination
              -- node unreachable; the optimisation round-trip removes both
              -- the emptied edge and the now-disconnected node.
              let
                result =
                  U.step [ openEditor, QuickInput "a", QuickInput "a", Confirm, Confirm ] editModel
              in
                Expect.all
                  [ \_ ->
                      U.mainView result
                      |> Maybe.map (.computation >> .graph >> Graph.edges >> List.length)
                      |> Expect.equal (Just 0)
                  , \_ ->
                      U.mainView result
                      |> Maybe.map (.computation >> .graph >> Graph.nodeIds >> List.sort)
                      |> Expect.equal (Just [ 0 ])
                  , \_ ->
                      -- and the drawing data no longer shows the link
                      U.mainView result
                      |> Maybe.map (.drawingData >> .link_drawing >> Dict.member ( 0, 1 ))
                      |> Expect.equal (Just False)
                  , \_ ->
                      U.mainView result
                      |> Maybe.map (.undoBuffer >> List.isEmpty)
                      |> Expect.equal (Just True)
                  ]
                    ()
        ]
    , describe "connecting two existing nodes for the first time"
        [ test "the (ab)+ flow: create a node, then link it back to the source" <|
            \_ ->
              -- create node 1 with transition a, then connect 1 back to 0
              -- with b.  The second connection is an ExistingNodeNewConnection
              -- commit, which must ADD the link (there is none to "remove").
              let
                base =
                  U.defaultModel
                    |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ) ])
                    |> U.mapMainView
                        (\gv ->
                          { gv
                            | host = { w = 190, h = 190 }
                            , guest = { x = -95, y = -95, w = 190, h = 190 }
                            , pan = ( 0, 0 )
                          }
                        )
                chainUid =
                  base.mainGraphView
                result =
                  U.step
                    [ GraphViewMsg chainUid (SelectNode 0 { x = 95, y = 95 })
                    , GraphViewMsg chainUid (MoveNode { x = 150, y = 150 }) -- swing out a phantom
                    , GraphViewMsg chainUid SelectSpace
                    , QuickInput "a"
                    , Confirm -- node 1 exists with 0 -a→ 1
                    , GraphViewMsg chainUid (SelectNode 1 { x = 150, y = 150 })
                    , GraphViewMsg chainUid (MoveNode { x = 0, y = 0 }) -- lock on to node 0
                    , GraphViewMsg chainUid (SelectNode 0 { x = 0, y = 0 }) -- confirms destination 1 → 0
                    , QuickInput "b"
                    , Confirm
                    ]
                    base
              in
                Expect.all
                  [ \_ -> edgeConnection result 0 1 |> Expect.equal [ transitionToString plainA ]
                  , \_ -> edgeConnection result 1 0 |> Expect.equal [ transitionToString plainB ]
                  , \_ ->
                      U.mainView result
                      |> Maybe.map (.drawingData >> .link_drawing >> Dict.member ( 1, 0 ))
                      |> Expect.equal (Just True)
                  ]
                    ()
        , test "confirming a self-loop between an existing node and itself commits too" <|
            \_ ->
              -- same branch, other cell: no mouse-move between the two
              -- clicks, so the destination stays the node itself
              let
                base =
                  U.defaultModel
                    |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ) ])
                    |> U.mapMainView
                        (\gv ->
                          { gv
                            | host = { w = 190, h = 190 }
                            , guest = { x = -95, y = -95, w = 190, h = 190 }
                            , pan = ( 0, 0 )
                          }
                        )
                chainUid =
                  base.mainGraphView
                result =
                  U.step
                    [ GraphViewMsg chainUid (SelectNode 0 { x = 95, y = 95 })
                    , GraphViewMsg chainUid (MoveNode { x = 150, y = 150 })
                    , GraphViewMsg chainUid SelectSpace
                    , QuickInput "a"
                    , Confirm
                    , GraphViewMsg chainUid (SelectNode 1 { x = 150, y = 150 })
                    , GraphViewMsg chainUid (SelectNode 1 { x = 150, y = 150 }) -- confirms 1 → 1
                    , QuickInput "c"
                    , Confirm
                    ]
                    base
              in
                Expect.all
                  [ \_ -> edgeConnection result 0 1 |> Expect.equal [ transitionToString plainA ]
                  , \_ -> edgeConnection result 1 1 |> Expect.equal [ transitionToString (Transition False (ViaCharacter 'c')) ]
                  ]
                    ()
        ]
    ]
