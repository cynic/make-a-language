module Splitting_tests exposing (suite)
{-| Batch 5: splitting a node, at the update level.

    Flow: ctrl-click a node (`StartSplit`) → a `SplittingNode` interaction
    plus a dedicated 250×250 interface view; typing (`QuickInput`) or
    `ToggleConnectionTransition` moves transitions between the left and
    right sides; `Confirm` commits the split (only when both sides are
    non-empty), `Escape` cancels.
-}
import AutoDict
import AutoSet
import Data exposing
  ( AcceptVia(..)
  , GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  , Transition
  )
import Expect
import Graph exposing (NodeId)
import Queries as Q
import Test exposing (..)
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


{-| A graph whose node 1 has two incoming transitions from node 0:
    `0-ab-1`.  Splitting node 1 therefore has something to distribute.
-}
splitModel : Model
splitModel =
  U.defaultModel
  |> U.withGraph (Utility.ag "0-ab-1")


uid : Uuid
uid =
  splitModel.mainGraphView


startSplit : Msg
startSplit =
  GraphViewMsg uid (StartSplit 1)


started : Model
started =
  U.step1 startSplit splitModel


topOf : Model -> Maybe InteractionState
topOf m =
  Q.peekInteraction (Just m.mainGraphView) m.interactionsDict


nonFinalChar : Char -> AutoSet.Set String Transition -> Bool
nonFinalChar ch set =
  AutoSet.member (Transition False (ViaCharacter ch)) set


mainGraphIds : Model -> List NodeId
mainGraphIds m =
  U.mainView m
  |> Maybe.map (.computation >> .graph >> Graph.nodeIds >> List.sort)
  |> Maybe.withDefault []


suite : Test
suite =
  describe "Splitting a node"
    [ describe "StartSplit"
        [ test "pushes SplittingNode with the empty left side and the full right side" <|
            \_ ->
              case topOf started of
                Just (SplittingNode d props) ->
                  Expect.all
                    [ \_ -> d.to_split |> Expect.equal 1
                    , \_ -> AutoSet.isEmpty d.left |> Expect.equal True
                    , \_ -> ( nonFinalChar 'a' d.right, nonFinalChar 'b' d.right ) |> Expect.equal ( True, True )
                    , \_ -> props.mainGraph /= uid |> Expect.equal True
                    , \_ -> AutoDict.member props.mainGraph started.graph_views |> Expect.equal True
                    ]
                    ()

                _ ->
                  Expect.fail "expected a SplittingNode interaction on top of the stack"
        , test "the interface view mirrors the computation" <|
            \_ ->
              let
                interfaceId =
                  case topOf started of
                    Just (SplittingNode _ props) ->
                      props.mainGraph

                    _ ->
                      uid
              in
                AutoDict.get interfaceId started.graph_views
                |> Maybe.map (.computation >> .graph >> Graph.nodeIds >> List.sort)
                |> Expect.equal (Just ( mainGraphIds started ))
        , test "capabilities: escape, character input and splitter dragging are permitted" <|
            \_ ->
              started.can
              |> \can -> ( can.escape, can.acceptCharacters, can.dragSplitter )
              |> Expect.equal ( True, True, True )
        , test "capabilities: the source view permits dragging and inspecting, but not selecting" <|
            \_ ->
              U.mainViewOrFail started
              |> .can
              |> \can -> ( ( can.dragNodes, can.inspectRefs, can.pan ), can.selectNodes )
              |> Expect.equal ( ( True, True, True ), False )
        ]
    , describe "moving transitions between the sides"
        [ test "typing a moves transition a to the left" <|
            \_ ->
              U.step [ startSplit, QuickInput "a" ] splitModel
              |> topOf
              |> Maybe.map
                  (\i ->
                    case i of
                      SplittingNode d _ ->
                        ( nonFinalChar 'a' d.left, nonFinalChar 'a' d.right, nonFinalChar 'b' d.right )

                      _ ->
                        ( False, False, False )
                  )
              |> Expect.equal ( Just ( True, False, True ) )
        , test "typing a again moves it back (round-trip)" <|
            \_ ->
              U.step [ startSplit, QuickInput "a", QuickInput "a" ] splitModel
              |> topOf
              |> Maybe.map
                  (\i ->
                    case i of
                      SplittingNode d _ ->
                        ( AutoSet.isEmpty d.left, nonFinalChar 'a' d.right, nonFinalChar 'b' d.right )

                      _ ->
                        ( False, False, False )
                  )
              |> Expect.equal ( Just ( True, True, True ) )
        , test "ToggleConnectionTransition does the same as typing" <|
            \_ ->
              U.step [ startSplit, ToggleConnectionTransition (ViaCharacter 'a') ] splitModel
              |> topOf
              |> Maybe.map
                  (\i ->
                    case i of
                      SplittingNode d _ ->
                        nonFinalChar 'a' d.left

                      _ ->
                        False
                  )
              |> Expect.equal (Just True)
        ]
    , describe "Confirm"
        [ test "commits the split: each side keeps its own transitions" <|
            \_ ->
              let
                result =
                  U.step [ startSplit, QuickInput "a", Confirm ] splitModel
              in
                Expect.all
                  [ \_ ->
                      U.mainView result
                      |> Maybe.map .computation
                      |> Maybe.map (Utility.ag_equals (Utility.ag "0-a-1 0-b-2"))
                      |> Maybe.withDefault (Expect.fail "no main view")
                  , \_ -> topOf result |> Expect.equal Nothing
                  , \_ -> AutoDict.size result.graph_views |> Expect.equal 2 -- main + explorer; interface is gone
                  , \_ ->
                      U.mainView result
                      |> Maybe.map (.undoBuffer >> List.length)
                      |> Expect.equal (Just 1)
                  ]
                    ()
        , test "confirming with an empty side merely cancels" <|
            \_ ->
              let
                result =
                  U.step [ startSplit, Confirm ] splitModel -- left is empty
              in
                Expect.all
                  [ \_ ->
                      U.mainView result
                      |> Maybe.map .computation
                      |> Maybe.map (Utility.ag_equals (Utility.ag "0-ab-1"))
                      |> Maybe.withDefault (Expect.fail "no main view")
                  , \_ -> topOf result |> Expect.equal Nothing
                  , \_ -> AutoDict.size result.graph_views |> Expect.equal 2
                  , \_ -> result.can.escape |> Expect.equal False -- capabilities must return to idle
                  , \_ -> U.mainViewOrFail result |> .can |> .selectNodes |> Expect.equal True
                  ]
                    ()
        ]
    , describe "Escape"
        [ test "cancels: removes the interface and leaves the graph untouched" <|
            \_ ->
              let
                result =
                  U.step [ startSplit, Escape ] splitModel
              in
                Expect.all
                  [ \_ ->
                      U.mainView result
                      |> Maybe.map .computation
                      |> Maybe.map (Utility.ag_equals (Utility.ag "0-ab-1"))
                      |> Maybe.withDefault (Expect.fail "no main view")
                  , \_ -> topOf result |> Expect.equal Nothing
                  , \_ -> AutoDict.size result.graph_views |> Expect.equal 2
                  ]
                    ()
        , test "resets the capabilities to idle" <|
            \_ ->
              U.step [ startSplit, Escape ] splitModel
              |> \m ->
                  Expect.all
                    [ \_ -> m.can.escape |> Expect.equal False
                    , \_ -> U.mainViewOrFail m |> .can |> .selectNodes |> Expect.equal True
                    ]
                      ()
        ]
    ]
