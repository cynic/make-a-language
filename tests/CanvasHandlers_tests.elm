module CanvasHandlers_tests exposing (suite)
{-| Batch 16: the canvas event handlers embedded in `GraphEditor`'s view —
    the click/ctrl-click decoder, the shift-mousedown decoder, and the
    transition-label click message ("clicking on connection text to start
    editing").  Tested with synthetic event JSON against the extracted
    pure functions.
-}
import Data exposing
  ( GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  )
import Dict
import Expect
import Graph exposing (NodeId)
import GraphEditor
import Json.Decode as D
import Json.Encode as E
import Queries as Q
import Test exposing (..)
import UILibrary as U
import Uuid
import Utility


decodeWith : D.Decoder a -> E.Value -> Maybe a
decodeWith decoder value =
  D.decodeValue decoder value
  |> Result.toMaybe


{-| A model whose node 1 is splittable (two transitions in from node 0)
    and whose idle main view permits everything.
-}
canvasModel : Model
canvasModel =
  U.defaultModel
  |> U.withGraph (Utility.ag "0-ab-1")


viewId : Uuid.Uuid
viewId =
  canvasModel.mainGraphView


can : Data.GraphViewCapabilities
can =
  U.mainViewOrFail canvasModel |> .can


nodeData : NodeId -> Data.NodeDrawingData
nodeData id =
  U.mainViewOrFail canvasModel
  |> .computation
  |> (\ag -> GraphEditor.nodeDrawingForPackage ag viewId)
  |> Dict.get id
  |> Maybe.withDefault
      { isDisconnected = False
      , isTerminal = False
      , coordinates = { x = 0, y = 0 }
      , isRoot = id == 0
      , canSplit = False
      , view_uuid = viewId
      , isSelected = False
      }


clickEvent : Bool -> Float -> Float -> E.Value
clickEvent ctrl x y =
  E.object
    [ ( "ctrlKey", E.bool ctrl )
    , ( "clientX", E.float x )
    , ( "clientY", E.float y )
    ]


mousedownEvent : Bool -> E.Value
mousedownEvent shift =
  E.object [ ( "shiftKey", E.bool shift ) ]


isSplitting : InteractionState -> Bool
isSplitting i =
  case i of
    SplittingNode _ _ ->
      True

    _ ->
      False


isChoosing : InteractionState -> Bool
isChoosing i =
  case i of
    ChoosingDestinationFor _ _ _ ->
      True

    _ ->
      False


isEditing : InteractionState -> Bool
isEditing i =
  case i of
    EditingConnection _ _ ->
      True

    _ ->
      False


suite : Test
suite =
  describe "Canvas handlers"
    [ describe "clicking a node"
        [ test "ctrl-click on a splittable node produces StartSplit" <|
            \_ ->
              decodeWith (GraphEditor.nodeClickDecoder 1 can (nodeData 1)) (clickEvent True 0 0)
              |> Expect.equal (Just (GraphViewMsg viewId (StartSplit 1)))
        , test "ctrl-click on a non-splittable node produces nothing" <|
            \_ ->
              decodeWith (GraphEditor.nodeClickDecoder 0 can (nodeData 0)) (clickEvent True 0 0)
              |> Expect.equal Nothing
        , test "plain click produces SelectNode with the event coordinates" <|
            \_ ->
              decodeWith (GraphEditor.nodeClickDecoder 1 can (nodeData 1)) (clickEvent False 12.5 40)
              |> Expect.equal
                  (Just (GraphViewMsg viewId (SelectNode 1 { x = 12.5, y = 40 })))
        , test "with selection and splitting disabled, nothing is produced" <|
            \_ ->
              let
                denied = { can | selectNodes = False, splitNodes = False }
              in
                Expect.all
                  [ \_ ->
                      decodeWith (GraphEditor.nodeClickDecoder 1 denied (nodeData 1)) (clickEvent False 0 0)
                      |> Expect.equal Nothing
                  , \_ ->
                      decodeWith (GraphEditor.nodeClickDecoder 1 denied (nodeData 1)) (clickEvent True 0 0)
                      |> Expect.equal Nothing
                  ]
                    ()
        ]
    , describe "starting a node drag"
        [ test "shift-mousedown produces StartDraggingNode with the event coordinates" <|
            \_ ->
              decodeWith (GraphEditor.nodeDragStartDecoder 1 can (nodeData 1))
                (E.object [ ( "shiftKey", E.bool True ), ( "clientX", E.float 7 ), ( "clientY", E.float 8 ) ])
              |> Expect.equal
                  (Just (GraphViewMsg viewId (StartDraggingNode 1 { x = 7, y = 8 })))
        , test "plain mousedown produces nothing" <|
            \_ ->
              decodeWith (GraphEditor.nodeDragStartDecoder 1 can (nodeData 1)) (mousedownEvent False)
              |> Expect.equal Nothing
        , test "with dragging disabled, shift-mousedown produces nothing" <|
            \_ ->
              decodeWith
                (GraphEditor.nodeDragStartDecoder 1 { can | dragNodes = False } (nodeData 1))
                (mousedownEvent True)
              |> Expect.equal Nothing
        ]
    , describe "clicking connection text"
        [ test "produces EditConnection at the label position, when connection selection is permitted" <|
            \_ ->
              let
                conn = Utility.mkConn "a"
              in
                GraphEditor.linkClickMsg can viewId ( 0, 1 ) conn { x = 33, y = 44 }
                |> Maybe.map
                    (\msg ->
                      case msg of
                        EditConnection pos uuid src dest connection ->
                          ( ( pos.x, pos.y ), ( uuid, ( src, dest, connection == conn ) ) )

                        _ ->
                          ( ( 0, 0 ), ( viewId, ( -1, -1, False ) ) )
                    )
                |> Expect.equal (Just ( ( 33, 44 ), ( viewId, ( 0, 1, True ) ) ))
        , test "produces nothing when connection selection is not permitted" <|
            \_ ->
              GraphEditor.linkClickMsg { can | selectConnections = False } viewId ( 0, 1 ) (Utility.mkConn "a") { x = 0, y = 0 }
              |> Expect.equal Nothing
        ]
    , describe "the extracted messages drive the real flows"
        [ test "StartSplit message opens the split interface" <|
            \_ ->
              decodeWith (GraphEditor.nodeClickDecoder 1 can (nodeData 1)) (clickEvent True 0 0)
              |> Maybe.map (\m -> U.step1 m canvasModel)
              |> Maybe.andThen (\m -> Q.peekInteraction (Just viewId) m.interactionsDict)
              |> Maybe.map isSplitting
              |> Expect.equal (Just True)
        , test "SelectNode message starts a destination choice" <|
            \_ ->
              decodeWith (GraphEditor.nodeClickDecoder 0 can (nodeData 0)) (clickEvent False 95 95)
              |> Maybe.map (\m -> U.step1 m canvasModel)
              |> Maybe.andThen (\m -> Q.peekInteraction (Just viewId) m.interactionsDict)
              |> Maybe.map isChoosing
              |> Expect.equal (Just True)
        , test "EditConnection message opens the connection editor" <|
            \_ ->
              GraphEditor.linkClickMsg can viewId ( 0, 1 ) (Utility.mkConn "a") { x = 33, y = 44 }
              |> Maybe.map (\m -> U.step1 m canvasModel)
              |> Maybe.andThen (\m -> Q.peekInteraction (Just viewId) m.interactionsDict)
              |> Maybe.map isEditing
              |> Expect.equal (Just True)
        ]
    ]
