module KeyboardRouting_tests exposing (suite)
{-| Batch 13: keyboard routing — the extracted `Main.keyboardDecoder`,
    tested with synthetic event JSON.  These rules live in the decoder (they
    decide *which* `Msg`, if any, a keypress produces in a given interaction
    state) and are invisible to update-simulation tests.
-}
import Data exposing
  ( GraphViewMsg(..)
  , Model
  , Msg(..)
  )
import Expect
import Json.Decode as D
import Json.Encode as E
import Main
import Test exposing (..)
import UILibrary as U
import Utility


{-| Decode a synthetic keydown event.
-}
decodeKey : Model -> String -> Bool -> Maybe Msg
decodeKey model key ctrl =
  D.decodeValue
    (Main.keyboardDecoder model)
    (E.object [ ( "key", E.string key ), ( "ctrlKey", E.bool ctrl ) ])
  |> Result.toMaybe


idle : Model
idle =
  U.defaultModel


{-| A connection-editing state (escape and confirm require Ctrl).
-}
editing : Model
editing =
  let
    m = U.defaultModel |> U.withGraph (Utility.ag "0-a-1")
  in
    U.step1 (EditConnection { x = 50, y = 50 } m.mainGraphView 0 1 (Utility.mkConn "a")) m


{-| A node-splitting state (escape and confirm require Ctrl).
-}
splitting : Model
splitting =
  let
    m = U.defaultModel |> U.withGraph (Utility.ag "0-ab-1")
  in
    U.step1 (GraphViewMsg m.mainGraphView (StartSplit 1)) m


{-| A node-dragging state: escape is permitted without Ctrl.
-}
dragging : Model
dragging =
  let
    m = U.defaultModel |> U.withGraph (Utility.ag "0-a-1")
  in
    U.step1 (GraphViewMsg m.mainGraphView (StartDraggingNode 0 { x = 95, y = 95 })) m


suite : Test
suite =
  describe "Keyboard routing"
    [ describe "Escape"
        [ test "is ignored (with and without Ctrl) when escape is not permitted" <|
            \_ ->
              Expect.equal
                ( decodeKey idle "Escape" False
                , decodeKey idle "Escape" ctrlHeld
                )
                ( Nothing, Nothing )
        , test "requires Ctrl during connection-editing" <|
            \_ ->
              Expect.equal
                ( decodeKey editing "Escape" ctrlHeld
                , decodeKey editing "Escape" False
                )
                ( Just Escape, Nothing )
        , test "requires Ctrl during node-splitting" <|
            \_ ->
              Expect.equal
                ( decodeKey splitting "Escape" ctrlHeld
                , decodeKey splitting "Escape" False
                )
                ( Just Escape, Nothing )
        , test "needs no modifier in other escape-permitted states" <|
            \_ ->
              Expect.equal
                ( decodeKey dragging "Escape" False
                , decodeKey dragging "escape" False -- case-insensitive
                )
                ( Just Escape, Just Escape )
        ]
    , describe "Enter / Confirm"
        [ test "Enter confirms in general" <|
            \_ ->
              Expect.equal
                ( decodeKey idle "Enter" False
                , decodeKey idle "Enter" ctrlHeld
                )
                ( Just Confirm, Just Confirm )
        , test "requires Ctrl during connection-editing" <|
            \_ ->
              Expect.equal
                ( decodeKey editing "Enter" ctrlHeld
                , decodeKey editing "Enter" False
                )
                ( Just Confirm, Nothing )
        , test "requires Ctrl during node-splitting" <|
            \_ ->
              Expect.equal
                ( decodeKey splitting "Enter" ctrlHeld
                , decodeKey splitting "Enter" False
                )
                ( Just Confirm, Nothing )
        ]
    , describe "undo and redo"
        [ test "Ctrl-z means Undo; plain z means nothing" <|
            \_ ->
              Expect.equal
                ( decodeKey idle "z" ctrlHeld
                , decodeKey idle "z" False
                )
                ( Just Undo, Nothing )
        , test "Ctrl-y means Redo; plain y means nothing" <|
            \_ ->
              Expect.equal
                ( decodeKey idle "y" ctrlHeld
                , decodeKey idle "y" False
                )
                ( Just Redo, Nothing )
        ]
    , describe "unmapped keys"
        [ test "produce no message" <|
            \_ ->
              Expect.equal
                ( decodeKey idle "q" False
                , decodeKey idle "Shift" False
                , decodeKey idle "F5" False
                )
                ( Nothing, Nothing, Nothing )
        ]
    ]


{-| A local alias so the test reads cleanly.
-}
ctrlHeld : Bool
ctrlHeld =
  True
