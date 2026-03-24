module Splitter_tests exposing (suite)
{-| Batch 8: dragging the splitters, at the update level.

    The LeftRight splitter resizes the sidebar (computation explorer) and
    the main window; the UpDown splitter resizes the bottom window (during
    execution: the tools area; otherwise: the input/description area) and
    the main window.  Both go through `UI.dragSplitter` (with clamping) and
    `UI.updateMainEditorDimensions` (which re-derives each affected
    view's host and guest rectangles).
-}
import AutoDict
import Changes as C
import Data exposing
  ( InteractionState(..)
  , Model
  , Msg(..)
  , PackageMsg(..)
  , SplitterMovement(..)
  )
import Expect
import Queries as Q
import Test exposing (..)
import UILibrary as U
import UILogic as UI


globalTop : Model -> Maybe InteractionState
globalTop m =
  Q.peekInteraction Nothing m.interactionsDict


explorerView : Model -> Maybe Data.GraphView
explorerView m =
  List.head m.computationsExplorer
  |> Maybe.andThen (\id -> AutoDict.get id m.graph_views)


{-| Drag the horizontal splitter to x=200 (from its initial 300).
-}
dragTo200 : List Msg
dragTo200 =
  [ StartDraggingSplitter LeftRight, DragSplitter False 200 ]


suite : Test
suite =
  describe "Dragging a splitter"
    [ describe "starting a drag"
        [ test "pushes a global DraggingSplitter interaction" <|
            \_ ->
              U.step1 (StartDraggingSplitter LeftRight) U.defaultModel
              |> globalTop
              |> Expect.equal (Just (DraggingSplitter LeftRight))
        , test "freezes every other capability while dragging" <|
            \_ ->
              U.step1 (StartDraggingSplitter UpDown) U.defaultModel
              |> \m ->
                  Expect.all
                    [ \_ ->
                        m.can
                        |> \can -> ( can.dragSplitter, can.escape, can.createNewPackage )
                        |> Expect.equal ( False, False, False )
                    , \_ ->
                        U.mainViewOrFail m
                        |> .can
                        |> \can -> ( can.pan, can.selectNodes, can.dragNodes )
                        |> Expect.equal ( False, False, False )
                    ]
                      ()
        ]
    , describe "the horizontal splitter (computation explorer / main window)"
        [ test "resizes the sidebar and the main window graphs" <|
            \_ ->
              let
                result = U.step dragTo200 U.defaultModel
                mainHost = U.mainViewOrFail result |> .host
                explorerHost = explorerView result |> Maybe.map .host
              in
                Expect.all
                  [ \_ -> result.uiLayout.dimensions.sideBar.w |> Expect.equal 200
                  , \_ -> mainHost |> Expect.equal { w = 544, h = 392 }
                  , \_ -> explorerHost |> Expect.equal (Just { w = 180, h = 101.25 })
                  ]
                    ()
        , test "re-derives the main window's guest rectangle for the new host" <|
            \_ ->
              let
                result = U.step dragTo200 U.defaultModel
                gv = U.mainViewOrFail result
              in
                gv.guest
                |> Expect.equal (UI.calculateGuestDimensionsForHost { w = 544, h = 392 } False gv.computation.graph)
        , test "re-derives the explorer's guest rectangle for the new host" <|
            \_ ->
              let
                result = U.step dragTo200 U.defaultModel
              in
                explorerView result
                |> Maybe.map
                    (\gv ->
                      ( gv.guest
                      , UI.calculateGuestDimensionsForHost { w = 180, h = 101.25 } True gv.computation.graph
                      )
                    )
                |> Maybe.map (\( a, b ) -> a == b)
                |> Expect.equal (Just True)
        , test "clamps the sidebar to its allowed range" <|
            \_ ->
              let
                tooSmall =
                  U.step [ StartDraggingSplitter LeftRight, DragSplitter False 10 ] U.defaultModel
                tooBig =
                  U.step [ StartDraggingSplitter LeftRight, DragSplitter False 700 ] U.defaultModel
              in
                Expect.equal
                  ( tooSmall.uiLayout.dimensions.sideBar.w
                  , tooBig.uiLayout.dimensions.sideBar.w
                  )
                  ( 100, 340 )
        , test "releasing restores idle capabilities and keeps the new size" <|
            \_ ->
              U.step (dragTo200 ++ [ DragSplitter True 200 ]) U.defaultModel
              |> \m ->
                  Expect.all
                    [ \_ -> globalTop m |> Expect.equal Nothing
                    , \_ -> m.can.dragSplitter |> Expect.equal True
                    , \_ -> m.uiLayout.dimensions.sideBar.w |> Expect.equal 200
                    ]
                      ()
        ]
    , describe "the vertical splitter (bottom window / main window)"
        [ test "resizes the bottom window and the main window graph" <|
            \_ ->
              let
                result =
                  U.step [ StartDraggingSplitter UpDown, DragSplitter False 350 ] U.defaultModel
              in
                Expect.all
                  [ \_ -> result.uiLayout.dimensions.bottomPanel.h |> Expect.equal 242
                  , \_ -> U.mainViewOrFail result |> .host |> Expect.equal { w = 444, h = 350 }
                  , \_ -> explorerView result |> Maybe.map .host |> Expect.equal (Just { w = 280, h = 157.5 })
                  ]
                    ()
        , test "clamps the bottom window to its allowed range" <|
            \_ ->
              let
                tooSmall =
                  U.step [ StartDraggingSplitter UpDown, DragSplitter False 1000 ] U.defaultModel
                tooBig =
                  U.step [ StartDraggingSplitter UpDown, DragSplitter False 0 ] U.defaultModel
              in
                Expect.equal
                  ( tooSmall.uiLayout.dimensions.bottomPanel.h
                  , tooBig.uiLayout.dimensions.bottomPanel.h
                  )
                  ( 80, 260 )
        , test "during execution: dragging keeps the Executing interaction underneath" <|
            \_ ->
              -- simulate an ongoing execution on the global stack
              C.pushInteractionForStack Nothing (Executing [] Nothing) U.defaultModel
              |> U.step (dragTo200 ++ [ DragSplitter True 200 ])
              |> \m ->
                  Expect.all
                    [ \_ -> globalTop m |> Expect.equal (Just (Executing [] Nothing))
                    , \_ -> m.uiLayout.dimensions.sideBar.w |> Expect.equal 200
                    ]
                      ()
        , test "while entering a description: dragging still resizes the input area" <|
            \_ ->
              let
                withDescription =
                  U.step1 (PackageMsg U.defaultModel.selectedPackage (UpdateComputationDescription "a description")) U.defaultModel
                result =
                  U.step [ StartDraggingSplitter UpDown, DragSplitter False 350 ] withDescription
              in
                Expect.all
                  [ \_ -> result.uiLayout.dimensions.bottomPanel.h |> Expect.equal 242
                  , \_ -> U.mainViewOrFail result |> .host |> Expect.equal { w = 444, h = 350 }
                  , \_ ->
                      -- the description survives the drag
                      AutoDict.get result.selectedPackage result.packages
                      |> Maybe.map (.computation >> .description)
                      |> Expect.equal (Just (Just "a description"))
                  ]
                    ()
        ]
    ]
