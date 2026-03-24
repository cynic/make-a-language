module SplitterDecoders_tests exposing (suite)
{-| Batch 14: the splitter-drag decoders and the pan-port decoders —
    extracted from `Main.subscriptions` and tested with synthetic JSON.

    The splitter decoders encode the one non-obvious piece of wiring
    business logic: the LeftRight offset of −52px (navigator bar 48px +
    half the splitter 8px) that turns a window-edge clientX into a
    sidebar-width coordinate.  The pan decoders pin the payload shape
    routed from JavaScript.
-}
import Data exposing
  ( GraphViewMsg(..)
  , Model
  , Msg(..)
  , SplitterMovement(..)
  )
import Expect
import Json.Decode as D
import Json.Encode as E
import Main
import Test exposing (..)
import UILibrary as U
import Uuid
import Utility


decodeWith : D.Decoder a -> E.Value -> Maybe a
decodeWith decoder value =
  D.decodeValue decoder value
  |> Result.toMaybe


{-| Decoders for a splitter drag of the given direction.
-}
splitterDecodersFor : SplitterMovement -> Maybe ( D.Decoder Msg, D.Decoder Msg )
splitterDecodersFor movement =
  U.step1 (StartDraggingSplitter movement) U.defaultModel
  |> Main.splitterDecoders


testUuid : Uuid.Uuid
testUuid =
  Utility.uuid_from_int 3


panPayload : E.Value
panPayload =
  E.object
    [ ( "uuid", Uuid.encode testUuid )
    , ( "pan"
      , E.object [ ( "pan_x", E.int 3 ), ( "pan_y", E.int -2 ) ]
      )
    ]


suite : Test
suite =
  describe "Splitter and pan-port decoders"
    [ describe "splitter dragging"
        [ test "LeftRight: the −52px navigator-bar offset is applied to clientX" <|
            \_ ->
              case splitterDecodersFor LeftRight of
                Just ( moveDecoder, upDecoder ) ->
                  Expect.all
                    [ \_ ->
                        decodeWith moveDecoder (E.object [ ( "clientX", E.float 300 ) ])
                        |> Expect.equal (Just (DragSplitter False 248))
                    , \_ ->
                        decodeWith upDecoder (E.object [ ( "clientX", E.float 300 ) ])
                        |> Expect.equal (Just (DragSplitter True 248))
                    ]
                      ()

                Nothing ->
                    Expect.fail "expected splitter decoders during a LeftRight drag"
        , test "UpDown: clientY passes through unmodified" <|
            \_ ->
              case splitterDecodersFor UpDown of
                Just ( moveDecoder, upDecoder ) ->
                  Expect.all
                    [ \_ ->
                        -- clientX is present but must be ignored
                        decodeWith moveDecoder (E.object [ ( "clientX", E.float 999 ), ( "clientY", E.float 100 ) ])
                        |> Expect.equal (Just (DragSplitter False 100))
                    , \_ ->
                        decodeWith upDecoder (E.object [ ( "clientY", E.float 100 ) ])
                        |> Expect.equal (Just (DragSplitter True 100))
                    ]
                      ()

                Nothing ->
                    Expect.fail "expected splitter decoders during an UpDown drag"
        , test "no splitter messages are produced outside a splitter drag" <|
            \_ ->
              Main.splitterDecoders U.defaultModel
              |> Expect.equal Nothing
        ]
    , describe "pan ports"
        [ test "a pan payload produces Pan for the encoded view uuid" <|
            \_ ->
              decodeWith Main.panDecoder panPayload
              |> Expect.equal (Just (GraphViewMsg testUuid (Pan 3 -2)))
        , test "a stop-pan payload produces StopPan for the encoded view uuid" <|
            \_ ->
              -- the stopPan port delivers the uuid directly, not an object
              decodeWith Main.stopPanDecoder (Uuid.encode testUuid)
              |> Expect.equal (Just (GraphViewMsg testUuid StopPan))
        , test "malformed pan payloads fail the decoder (and are routed to CrashWithMessage by the subscription)" <|
            \_ ->
              Expect.all
                [ \_ ->
                    decodeWith Main.panDecoder (E.object [ ( "uuid", Uuid.encode testUuid ) ])
                    |> Expect.equal Nothing
                , \_ ->
                    decodeWith Main.stopPanDecoder E.null
                    |> Expect.equal Nothing
                ]
                  ()
        ]
    ]
