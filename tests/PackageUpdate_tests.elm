module PackageUpdate_tests exposing (suite)
{-| Regression tests for `Changes.updatePackageFromView`: the committed
    computation must be in the resolution dictionary when the package's tests
    are recomputed, otherwise a self-reference resolves to the pre-commit
    graph.
-}
import AutoDict
import AutoSet
import Automata.DFA as DFA
import Changes as C
import Data exposing
  ( AcceptVia(..)
  , AutomatonGraph
  , ExecutionResult(..)
  , GraphPackage
  , Model
  , TestExpectation(..)
  , Transition
  , transitionToString
  )
import Expect
import Graph
import Test exposing (..)
import Time
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


pkgId : Uuid
pkgId =
  Utility.uuid_from_int 7


testId : Uuid
testId =
  Utility.uuid_from_int 42


{-| The package as stored: accepts "a" through a final character transition.
-}
oldGraph : AutomatonGraph
oldGraph =
  Utility.ag "0-!a-1"


{-| The freshly committed graph: its only node has a `b` self-loop and a
    reference to the package itself.  Resolving the reference against the new
    graph leaves only the `b` path (and rejects "a"); resolving it against
    `oldGraph` accepts "a".
-}
newGraph : AutomatonGraph
newGraph =
  { graph =
      Graph.fromNodesAndEdges
        [ Graph.Node 0 (Utility.dummyEntity 0) ]
        [ Graph.Edge 0 0
            (AutoSet.fromList transitionToString
              [ Transition True (ViaGraphReference pkgId)
              , Transition True (ViaCharacter 'b')
              ]
            )
        ]
  , description = Nothing
  , root = 0
  }


pkg : GraphPackage
pkg =
  { computation = oldGraph
  , packageIdentifier = pkgId
  , created = Time.millisToPosix 1000
  , currentTestKey = testId
  , tests =
      AutoDict.fromList Uuid.toString
        [ ( testId
          , { input = "a"
            , expectation = ExpectAccepted
            , result = Accepted
            }
          )
        ]
  }


{-| The stored package still has `oldGraph`, while the main view has the
    just-committed `newGraph`.
-}
model : Model
model =
  U.initWithPackages [ pkg ]
  |> U.withGraph newGraph


resultOf : Model -> Maybe ExecutionResult
resultOf m =
  AutoDict.get pkgId m.packages
  |> Maybe.andThen (.tests >> AutoDict.get testId)
  |> Maybe.map .result


suite : Test
suite =
  describe "updatePackageFromView"
    [ test "recomputes tests against the committed computation (self-reference case)" <|
        \_ ->
          let
            resolutionWithNew =
              AutoDict.fromList Uuid.toString [ ( pkgId, newGraph ) ]

            resolutionWithOld =
              AutoDict.fromList Uuid.toString [ ( pkgId, oldGraph ) ]

            expected =
              DFA.resultForInput "a" resolutionWithNew newGraph

            stale =
              DFA.resultForInput "a" resolutionWithOld newGraph

            updated =
              C.updatePackageFromView model.mainGraphView model
          in
            Expect.all
              [ \_ -> stale |> Expect.equal Accepted
              , \_ -> expected |> Expect.notEqual stale
              , \_ -> resultOf updated |> Expect.equal (Just expected)
              ]
                ()
    ]
