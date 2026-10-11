module SingleLoopCommit_tests exposing (suite)
{-| Building, entirely via `Msg` values, the graph the user described:
    a single node whose ONLY link is a recursive self-loop carrying two
    transitions of DIFFERENT finality — a character `t` (non-final) and a
    graph reference (final) — then committing it and letting the idle
    Confirm optimise it.
-}
import AutoSet
import Data exposing
  ( AcceptVia(..)
  , ConnectionTarget(..)
  , GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  , PackageMsg(..)
  , transitionToString
  )
import Debugging
import Dict
import Expect
import Graph
import IntDict
import GoldenGeometry
import Queries as Q
import Test exposing (..)
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


refTarget : Uuid.Uuid
refTarget =
  Utility.uuid_from_int 9


{-| Two packages; the reference will point at the older one.
-}
loopModel : Model
loopModel =
  U.initWithPackages
    [ Utility.packageWithGraph (Utility.uuid_from_int 7) 1000 (Utility.ag "0-b-1 1-!c-2")
    , Utility.packageWithGraph refTarget 2000 (Utility.ag "0-d-1")
    ]
  |> U.withGraph (GoldenGeometry.mkGraph [ ( 0, 100, 100 ) ] [])


uid : Uuid
uid =
  loopModel.mainGraphView


{-| The full UI flow: click the node, click it again (self-destination),
    add the reference (final) and the character t (non-final), commit.
-}
buildLoop : List Msg
buildLoop =
  [ GraphViewMsg uid (SelectNode 0 { x = 195, y = 195 })
  , GraphViewMsg uid (SelectNode 0 { x = 195, y = 195 }) -- confirms self-destination
  , ToggleConnectionTransition (ViaGraphReference refTarget) -- added non-final…
  , ToggleConnectionTransition (ViaGraphReference refTarget) -- …now final
  , QuickInput "t" -- and the non-final character
  ]


connString : Data.Connection -> List String
connString conn =
  AutoSet.toList conn
  |> List.map transitionToString
  |> List.sort


committedLoop : Model -> List String
committedLoop m =
  U.mainView m
  |> Maybe.andThen (.computation >> .graph >> Graph.get 0)
  |> Maybe.map (.incoming >> IntDict.get 0 >> Maybe.withDefault (AutoSet.empty transitionToString) >> connString)
  |> Maybe.withDefault [ "NO SELF-LOOP" ]


nodeIdsOf : Model -> List NodeId_
nodeIdsOf m =
  U.mainView m
  |> Maybe.map (.computation >> .graph >> Graph.nodes >> List.map .id >> List.sort)
  |> Maybe.withDefault []


type alias NodeId_ =
  Graph.NodeId


suite : Test
suite =
  describe "In a single recursive link with mixed-finality transitions"
    [ test "the UI flow commits a self-loop carrying {t non-final, ref final}" <|
        \_ ->
          let
            result = U.step (buildLoop ++ [ Confirm ]) loopModel
          in
            Expect.all
              [ \_ -> nodeIdsOf result |> Expect.equal [ 0 ]
              , \_ ->
                  committedLoop result
                  |> Expect.equal
                      (connString (AutoSet.fromList Data.transitionToString
                        [ Data.Transition False (ViaCharacter 't')
                        , Data.Transition True (ViaGraphReference refTarget)
                        ]))
              ]
                    ()
    , describe "and when we do the optimising idle-Confirm"
        [ test "the optimisation preserves the mixed-finality self-loop (with reference)" <|
                \_ ->
                  let
                    committed = U.step (buildLoop ++ [ Confirm ]) loopModel
                    optimised = U.step (buildLoop ++ [ Confirm, Confirm ]) loopModel
                  in
                    Expect.all
                      [ \_ ->
                          U.mainView optimised
                          |> Maybe.map .computation
                          |> Maybe.map
                              (Utility.ag_equals
                                (U.mainView committed
                                  |> Maybe.map .computation
                                  |> Maybe.withDefault Data.empty))
                          |> Maybe.withDefault (Expect.fail "no main view")
                      , \_ -> nodeIdsOf optimised |> Expect.equal [ 0 ]
                      ]
                        ()
        , test "the optimisation preserves the mixed-finality characters-only self-loop" <|
            \_ ->
              let
                flowOf model =
                  [ GraphViewMsg model.mainGraphView (SelectNode 0 { x = 195, y = 195 })
                  , GraphViewMsg model.mainGraphView (SelectNode 0 { x = 195, y = 195 })
                  , QuickInput "t"
                  , QuickInput "t" -- t becomes final
                  , QuickInput "u"
                  ]
                fresh =
                  U.defaultModel |> U.withGraph (GoldenGeometry.mkGraph [ ( 0, 100, 100 ) ] [])
                committed = U.step (flowOf fresh ++ [ Confirm ]) fresh
                optimised = U.step (flowOf fresh ++ [ Confirm, Confirm ]) fresh
              in
                Expect.all
                  [ \_ ->
                      U.mainView optimised
                      |> Maybe.map .computation
                      |> Maybe.map
                          (Utility.ag_equals
                            (U.mainView committed
                              |> Maybe.map .computation
                              |> Maybe.withDefault Data.empty))
                      |> Maybe.withDefault (Expect.fail "no main view")
                  , \_ -> committedLoop optimised |> Expect.notEqual [ "NO SELF-LOOP" ]
                  ]
                    ()
        ]
    ]
