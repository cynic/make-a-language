module TestInputManagement_tests exposing (suite)
{-| Batch 17: the testing tool's messages, at the update level.

    `UpdateTestInput` materialises a test on first write (at the current —
    possibly dangling — test-key) and recomputes its result against the
    package's own computation; emptying the input deletes the test.
    `SelectTest` / `CreateNewTestInput` move the current test-key and stop
    an in-flight execution.
-}
import AutoDict
import Data exposing
  ( ExecutionResult(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  , PackageMsg(..)
  , TestExpectation(..)
  )
import Executing_tests
import Expect
import Queries as Q
import Test exposing (..)
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


globalTop : Model -> Maybe InteractionState
globalTop m =
  Q.peekInteraction Nothing m.interactionsDict


{-| The selected package of the Batch-7 execution model (which starts with
    the test "ab" at `currentTestKey`).
-}
pkg : Model -> Uuid
pkg m =
  m.selectedPackage


testsOf : Model -> Maybe (AutoDict.Dict String Uuid Data.Test)
testsOf m =
  AutoDict.get (pkg m) m.packages
  |> Maybe.map .tests


currentTestKeyOf : Model -> Maybe Uuid
currentTestKeyOf m =
  AutoDict.get (pkg m) m.packages
  |> Maybe.map .currentTestKey


currentTestOf : Model -> Maybe Data.Test
currentTestOf m =
  AutoDict.get (pkg m) m.packages
  |> Maybe.andThen (\p -> AutoDict.get p.currentTestKey p.tests)


otherKey : Uuid
otherKey =
  Utility.uuid_from_int 42


suite : Test
suite =
  describe "Testing tool"
    [ describe "writing test inputs"
        [ test "selecting a fresh key and writing materialises the test with a recomputed result" <|
            \_ ->
              let
                result =
                  U.step
                    [ PackageMsg (pkg Executing_tests.execModel) (SelectTest otherKey)
                    , PackageMsg (pkg Executing_tests.execModel) (UpdateTestInput "ab")
                    ]
                    Executing_tests.execModel
              in
                Expect.all
                  [ \_ -> currentTestKeyOf result |> Expect.equal (Just otherKey)
                  , \_ ->
                      currentTestOf result
                      |> Maybe.map (\t -> ( t.input, t.expectation, t.result ))
                      |> Expect.equal (Just ( "ab", ExpectAccepted, Accepted ))
                  , \_ ->
                      -- the original test survives at the old key
                      testsOf result
                      |> Maybe.map AutoDict.size
                      |> Expect.equal (Just 2)
                  ]
                    ()
        , test "results recompute as the input changes" <|
            \_ ->
              let
                rejected =
                  U.step1 (PackageMsg (pkg Executing_tests.execModel) (UpdateTestInput "a")) Executing_tests.execModel
                accepted =
                  U.step1 (PackageMsg (pkg Executing_tests.execModel) (UpdateTestInput "ab")) rejected
              in
                Expect.all
                  [ \_ ->
                      currentTestOf rejected
                      |> Maybe.map (\t -> ( t.input, t.result ))
                      |> Expect.equal (Just ( "a", Rejected ))
                  , \_ ->
                      currentTestOf accepted
                      |> Maybe.map (\t -> ( t.input, t.result ))
                      |> Expect.equal (Just ( "ab", Accepted ))
                  ]
                    ()
        , test "emptying the input deletes the current test; the dangling key is harmless" <|
            \_ ->
              let
                deleted =
                  U.step1 (PackageMsg (pkg Executing_tests.execModel) (UpdateTestInput "")) Executing_tests.execModel
                again =
                  U.step1 (PackageMsg (pkg Executing_tests.execModel) (UpdateTestInput "")) deleted
              in
                Expect.all
                  [ \_ -> testsOf deleted |> Maybe.map AutoDict.size |> Expect.equal (Just 0)
                  , \_ ->
                      -- the key still points at the (now missing) test
                      currentTestKeyOf deleted |> Expect.equal (currentTestKeyOf Executing_tests.execModel)
                  , \_ -> testsOf again |> Maybe.map AutoDict.size |> Expect.equal (Just 0)
                  , \_ -> currentTestOf again |> Expect.equal Nothing
                  ]
                    ()
        ]
    , describe "flipping the acceptance condition"
        [ test "flips the expectation, and flips it back; the result is untouched" <|
            \_ ->
              let
                flipped =
                  U.step1 (PackageMsg (pkg Executing_tests.execModel) FlipAcceptanceCondition) Executing_tests.execModel
                flippedBack =
                  U.step1 (PackageMsg (pkg Executing_tests.execModel) FlipAcceptanceCondition) flipped
              in
                Expect.all
                  [ \_ -> currentTestOf flipped |> Maybe.map .expectation |> Expect.equal (Just ExpectRejected)
                  , \_ -> currentTestOf flipped |> Maybe.map .result |> Expect.equal (Just Accepted)
                  , \_ -> currentTestOf flippedBack |> Maybe.map .expectation |> Expect.equal (Just ExpectAccepted)
                  ]
                    ()
        , test "is a no-op on a dangling key" <|
            \_ ->
              U.step
                [ PackageMsg (pkg Executing_tests.execModel) (SelectTest otherKey)
                , PackageMsg (pkg Executing_tests.execModel) FlipAcceptanceCondition
                ]
                Executing_tests.execModel
              |> testsOf
              |> Maybe.map AutoDict.size
              |> Expect.equal (Just 1) -- only the original test remains
        ]
    , describe "creating and selecting tests"
        [ test "SelectTest switches the current key and keeps existing tests" <|
            \_ ->
              U.step1 (PackageMsg (pkg Executing_tests.execModel) (SelectTest otherKey)) Executing_tests.execModel
              |> \m ->
                  Expect.all
                    [ \_ -> currentTestKeyOf m |> Expect.equal (Just otherKey)
                    , \_ -> testsOf m |> Maybe.map AutoDict.size |> Expect.equal (Just 1)
                    ]
                        ()
        , test "CreateNewTestInput switches to a fresh key and keeps existing tests" <|
            \_ ->
              U.step1 (PackageMsg (pkg Executing_tests.execModel) CreateNewTestInput) Executing_tests.execModel
              |> \m ->
                  Expect.all
                    [ \_ ->
                        currentTestKeyOf m
                        |> Expect.notEqual (currentTestKeyOf Executing_tests.execModel)
                    , \_ -> testsOf m |> Maybe.map AutoDict.size |> Expect.equal (Just 1)
                    ]
                        ()
        ]
    , describe "the testing tool and execution"
        [ test "SelectTest stops an in-flight execution" <|
            \_ ->
              U.step
                [ Step
                , PackageMsg (pkg Executing_tests.execModel) (SelectTest otherKey)
                ]
                Executing_tests.execModel
              |> globalTop
              |> Expect.equal Nothing
        , test "CreateNewTestInput stops an in-flight execution" <|
            \_ ->
              U.step
                [ Step
                , PackageMsg (pkg Executing_tests.execModel) CreateNewTestInput
                ]
                Executing_tests.execModel
              |> globalTop
              |> Expect.equal Nothing
        , test "stopping execution this way resets the capabilities" <|
            \_ ->
              -- expected to fail before bug #9's fix: both stop-execution
              -- branches omit setCapabilities, so the stale whenExecuting
              -- capabilities survive (can.escape stays True).
              U.step
                [ Step
                , PackageMsg (pkg Executing_tests.execModel) (SelectTest otherKey)
                ]
                Executing_tests.execModel
              |> \m -> ( m.can.escape, m.can.dragSplitter )
              |> Expect.equal ( False, True )
        ]
    ]
