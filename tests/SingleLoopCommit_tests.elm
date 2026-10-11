module SingleLoopCommit_tests exposing (suite)
{-| Building, entirely via `Msg` values, the graph the user described:
    a single node whose ONLY link is a recursive self-loop carrying two
    transitions of DIFFERENT finality — a character `t` (non-final) and a
    graph reference (final) — then committing it and letting the idle
    Confirm optimise it.

    Pinned: the first commit produces exactly that graph
    (`Edge 0->0 (!ref t)`).
    Known bugs (tests skipped):
    - #13: the optimising round-trip annihilates the reference version —
      `splitTerminalAndNonTerminal` faithfully splits the mixed-finality
      self-loop into a 3-node cross-wired loop, and `nfaToDFA` then
      returns an EMPTY graph for that shape.
    - #14: the characters-only variant loses just the self-loop (the node
      survives as an isolated vertex).
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
  describe "A single recursive link with mixed-finality transitions"
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
    , describe "the optimising idle-Confirm (two known bugs, tests skipped)"
        [ -- BUG #13: the round-trip annihilates this graph.  The exact
          -- stage is `nfaToDFA`: `splitTerminalAndNonTerminal` faithfully
          -- splits the mixed-finality self-loop into a 3-node cross-wired
          -- loop, and nfaToDFA then returns an EMPTY graph for it.
          -- Desired: the optimisation is language-preserving.
          -- Un-skip when the engine is fixed.
          Test.skip
            (test "the optimisation preserves the mixed-finality self-loop (with reference)" <|
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
            )
        , -- BUG #14: the characters-only variant loses just the loop
          -- (the node survives as an isolated vertex), so the eater is the
          -- same area but the annihilation is partial.  Un-skip when fixed.
          Test.skip
            (test "the optimisation preserves the mixed-finality characters-only self-loop" <|
                \_ ->
                  let
                    flow =
                      [ GraphViewMsg uid (SelectNode 0 { x = 195, y = 195 })
                      , GraphViewMsg uid (SelectNode 0 { x = 195, y = 195 })
                      , QuickInput "t"
                      , QuickInput "t" -- t becomes final
                      , QuickInput "u"
                      ]
                    fresh =
                      U.defaultModel |> U.withGraph (GoldenGeometry.mkGraph [ ( 0, 100, 100 ) ] [])
                    committed = U.step (flow ++ [ Confirm ]) fresh
                    optimised = U.step (flow ++ [ Confirm, Confirm ]) fresh
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
            )
        ]
    ]
