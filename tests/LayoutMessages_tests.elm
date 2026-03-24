module LayoutMessages_tests exposing (suite)
{-| Batch 18: layout messages at the update level — window resizing and
    area collapse/expand, plus the two "message outside its interaction"
    guards.

    `OnResize` clamps the UI constants to the new viewport and re-derives
    the areas via `recalculate_uistate`; the graph views must be re-hosted
    to match (their `host` drives both rendering *and* the client→SVG
    coordinate translation).
-}
import AutoDict
import Data exposing
  ( AreaUITarget(..)
  , GraphViewMsg(..)
  , Model
  , Msg(..)
  )
import Expect
import Graph
import Test exposing (..)
import UILibrary as U


explorerHost : Model -> Maybe ( Float, Float )
explorerHost m =
  List.head m.computationsExplorer
  |> Maybe.andThen (\id -> AutoDict.get id m.graph_views)
  |> Maybe.map (\gv -> ( gv.host.w, gv.host.h ))


mainHost : Model -> ( Float, Float )
mainHost m =
  U.mainViewOrFail m |> .host |> \h -> ( h.w, h.h )


suite : Test
suite =
  describe "Layout messages"
    [ describe "resizing the window"
        [ test "re-clamps the constants and re-derives the areas" <|
            \_ ->
              let
                result = U.step1 (OnResize { w = 500, h = 400 }) U.defaultModel
                dims = result.uiLayout.dimensions
              in
                Expect.all
                  [ \_ -> result.uiConstants.sideBarWidth.max |> Expect.equal 190
                  , \_ -> result.uiConstants.toolsPanelHeight.max |> Expect.equal 160
                  , \_ -> ( dims.viewport.w, dims.viewport.h ) |> Expect.equal ( 500, 400 )
                  , \_ -> dims.sideBar.w |> Expect.equal 190
                  , \_ -> dims.bottomPanel.h |> Expect.equal 160
                  , \_ -> dims.bottomPanel.w |> Expect.equal 254
                  , \_ -> ( dims.mainEditor.w, dims.mainEditor.h ) |> Expect.equal ( 254, 232 )
                  ]
                    ()
        , test "re-hosts the graph views to match (expected to fail before bug #10's fix)" <|
            \_ ->
              -- resizeViewport did not call updateMainEditorDimensions, so
              -- the views kept their pre-resize hosts — skewing every
              -- subsequent client→SVG coordinate translation.
              let
                result = U.step1 (OnResize { w = 500, h = 400 }) U.defaultModel
              in
                Expect.all
                  [ \_ -> mainHost result |> Expect.equal ( 254, 232 )
                  , \_ -> explorerHost result |> Expect.equal ( Just ( 170, 95.625 ) )
                  ]
                    ()
        ]
    , describe "collapsing and expanding areas"
        [ test "collapsing the sidebar widens the main window; explorer views keep their hosts while closed" <|
            \_ ->
              let
                collapsed =
                  U.step1 (ToggleAreaVisibility NavigatorsArea) U.defaultModel
                reopened =
                  U.step1 (ToggleAreaVisibility NavigatorsArea) collapsed
              in
                Expect.all
                  [ \_ -> collapsed.uiLayout.open.sideBar |> Expect.equal False
                  , \_ ->
                      let dims = collapsed.uiLayout.dimensions in
                      ( dims.mainEditor.w, dims.bottomPanel.w )
                      |> Expect.equal ( 800, 800 )
                  , \_ ->
                      -- pinned: closed sidebar views are NOT re-hosted
                      explorerHost collapsed |> Expect.equal ( Just ( 280, 157.5 ) )
                  , \_ -> reopened.uiLayout.open.sideBar |> Expect.equal True
                  , \_ ->
                      let dims = reopened.uiLayout.dimensions in
                      dims.mainEditor.w |> Expect.equal 444
                  ]
                    ()
        , test "collapsing the bottom panel heightens the main window" <|
            \_ ->
              let
                collapsed =
                  U.step1 (ToggleAreaVisibility ToolsArea) U.defaultModel
              in
                Expect.all
                  [ \_ -> collapsed.uiLayout.open.bottomPanel |> Expect.equal False
                  , \_ ->
                      let dims = collapsed.uiLayout.dimensions in
                      ( dims.mainEditor.w, dims.mainEditor.h )
                      |> Expect.equal ( 444, 600 )
                  ]
                    ()
        ]
    , describe "messages outside their interaction"
        [ test "splitter-drag messages outside a drag are ignored" <|
            \_ ->
              U.step1 (DragSplitter False 200) U.defaultModel
              |> \m -> m.uiLayout.dimensions.sideBar.w
              |> Expect.equal 300
        , test "QuickInput without a matching interaction is ignored" <|
            \_ ->
              let
                result = U.step1 (QuickInput "a") U.defaultModel
              in
                Expect.all
                  [ \_ ->
                      U.mainView result
                      |> Maybe.map (.computation >> .graph >> Graph.edges >> List.length)
                      |> Expect.equal (Just 0)
                  , \_ ->
                      AutoDict.values result.packages
                      |> List.map (.tests >> AutoDict.size)
                      |> Expect.equal [ 0 ]
                  ]
                    ()
        ]
    ]
