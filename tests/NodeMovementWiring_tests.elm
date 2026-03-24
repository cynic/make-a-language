module NodeMovementWiring_tests exposing (suite)
{-| Batch 15: node-movement wiring — the extracted `Main.nodeMoveDecoder` /
    `nodeMoveDecoders`, tested with synthetic event JSON.

    The invariant: client coordinates are translated into SVG coordinates
    *exactly once*, via the host coordinates stored on the interaction when
    the drag/choice began.
-}
import Data exposing
  ( GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  )
import Expect
import Graph exposing (NodeId)
import Json.Decode as D
import Json.Encode as E
import Main
import Test exposing (..)
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


decodeWith : D.Decoder a -> E.Value -> Maybe a
decodeWith decoder value =
  D.decodeValue decoder value
  |> Result.toMaybe


{-| One node at SVG (0,0); 190×190 host and guest; the given pan.
    One host pixel equals one SVG unit.
-}
wiringModel : ( Float, Float ) -> Model
wiringModel pan =
  U.defaultModel
  |> U.withGraph (U.graphWithNodes [ ( 0, 0, 0 ) ])
  |> U.mapMainView
      (\gv ->
        { gv
          | host = { w = 190, h = 190 }
          , guest = { x = -95, y = -95, w = 190, h = 190 }
          , pan = pan
        }
      )


moveDecoderFor : Model -> Maybe ( D.Decoder Msg, Maybe (D.Decoder Msg) )
moveDecoderFor m =
  Main.nodeMoveDecoders m
  |> List.head
  |> Maybe.map (\( _, move, up ) -> ( move, up ))


mouseEvent : Float -> Float -> E.Value
mouseEvent x y =
  E.object [ ( "clientX", E.float x ), ( "clientY", E.float y ) ]


suite : Test
suite =
  describe "Node-movement wiring"
    [ describe "during a node drag"
        [ test "mouse-move produces MoveNode with translated coordinates" <|
            \_ ->
              let
                m =
                  U.step1
                    (GraphViewMsg (wiringModel ( 0, 0 ) |> .mainGraphView) (StartDraggingNode 0 { x = 50, y = 25 }))
                    (wiringModel ( 0, 0 ))
              in
                case moveDecoderFor m of
                  Just ( moveDecoder, _ ) ->
                    -- stored host coord (−45,−70); mouse at (60,35) → SVG (10,10)
                    decodeWith moveDecoder (mouseEvent 60 35)
                    |> Expect.equal
                        (Just
                            (GraphViewMsg m.mainGraphView (MoveNode { x = 10, y = 10 }))
                        )

                  Nothing ->
                    Expect.fail "expected a node-move decoder during a drag"
        , test "a mouse-up produces StopDraggingNode" <|
            \_ ->
              let
                m =
                  U.step1
                    (GraphViewMsg (wiringModel ( 0, 0 ) |> .mainGraphView) (StartDraggingNode 0 { x = 50, y = 25 }))
                    (wiringModel ( 0, 0 ))
              in
                case moveDecoderFor m of
                  Just ( _, Just upDecoder ) ->
                    -- the up-decoder needs no event fields at all
                    decodeWith upDecoder (E.object [])
                    |> Expect.equal (Just (GraphViewMsg m.mainGraphView StopDraggingNode))

                  _ ->
                    Expect.fail "expected a mouse-up decoder during a drag"
        , test "translation happens exactly once (panned graph)" <|
            \_ ->
              -- pan (7,3); the node sits at SVG (0,0), i.e. client (88,92);
              -- the stored host coordinate is (0,0).  Moving to (98,102)
              -- must land on SVG (10,10): a *second* translation pass
              -- would land far away (−78,−82), so the exact value pins
              -- the once-only invariant.
              let
                base = wiringModel ( 7, 3 )
                m =
                  U.step1
                    (GraphViewMsg base.mainGraphView (StartDraggingNode 0 { x = 88, y = 92 }))
                    base
              in
                case moveDecoderFor m of
                  Just ( moveDecoder, _ ) ->
                    decodeWith moveDecoder (mouseEvent 98 102)
                    |> Expect.equal
                        (Just (GraphViewMsg m.mainGraphView (MoveNode { x = 10, y = 10 })))

                  Nothing ->
                    Expect.fail "expected a node-move decoder during a panned drag"
        ]
    , describe "during a destination choice"
        [ test "mouse-move produces MoveNode; there is no mouse-up decoder" <|
            \_ ->
              let
                base = wiringModel ( 7, 3 )
                m =
                  U.step
                    [ GraphViewMsg base.mainGraphView (SelectNode 0 { x = 88, y = 92 }) ]
                    base
              in
                case moveDecoderFor m of
                  Just ( moveDecoder, upDecoder ) ->
                    Expect.all
                      [ \_ ->
                          decodeWith moveDecoder (mouseEvent 98 102)
                          |> Expect.equal
                              (Just (GraphViewMsg m.mainGraphView (MoveNode { x = 10, y = 10 })))
                      , \_ -> upDecoder |> Expect.equal Nothing
                      ]
                        ()

                  Nothing ->
                    Expect.fail "expected a node-move decoder during a destination choice"
        ]
    , describe "outside node interactions"
        [ test "no node-move decoders are produced" <|
            \_ ->
              Main.nodeMoveDecoders U.defaultModel
              |> Expect.equal []
        , test "…nor during interactions that do not track the mouse this way" <|
            \_ ->
              -- a global splitter drag is keyed Nothing and has its own
              -- decoders; node-move decoders must not appear
              U.step1 (StartDraggingSplitter Data.LeftRight) U.defaultModel
              |> Main.nodeMoveDecoders
              |> Expect.equal []
        ]
    ]
