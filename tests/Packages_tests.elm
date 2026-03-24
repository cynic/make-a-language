module Packages_tests exposing (suite)
{-| Batch 9: the computation explorer and packages, at the update level.

    - Selecting a package (`PackageMsg … SelectPackage`) loads its graph
      into a fresh main editor view.
    - `TimeValueForPackage` creates a new package; the explorer list is
      sorted newest-first, so the new computation lands at the head.
    - Deleting: unreferenced packages go immediately; referenced packages
      open a `DeletingPackage` interaction (with a warning view) that must
      be confirmed or escaped.  The computation being edited has no
      delete capability at all.
-}
import AutoDict
import Data exposing
  ( InteractionState(..)
  , Model
  , Msg(..)
  , PackageMsg(..)
  )
import Expect
import Queries as Q
import Test exposing (..)
import Time
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


globalTop : Model -> Maybe InteractionState
globalTop m =
  Q.peekInteraction Nothing m.interactionsDict


{-| The package uuids shown in the explorer, newest first (derived from the
    views' package links).
-}
explorerPackages : Model -> List (Maybe Uuid)
explorerPackages m =
  List.map .graphPackage <|
    List.filterMap (\id -> AutoDict.get id m.graph_views) m.computationsExplorer


{-| A model with two independent computations: A ("0-a-1", older) and
    B ("0-b-1 1-c-2", newer, so it starts out selected).
-}
twoModels : Model
twoModels =
  U.initWithPackages
    [ Utility.packageWithGraph (Utility.uuid_from_int 7) 1000 (Utility.ag "0-a-1")
    , Utility.packageWithGraph (Utility.uuid_from_int 9) 2000 (Utility.ag "0-b-1 1-c-2")
    ]


{-| A model where B references A via a graph-reference transition
    (`0-@7-1`): deleting A therefore affects B.
-}
linkedModel : Model
linkedModel =
  U.initWithPackages
    [ Utility.packageWithGraph (Utility.uuid_from_int 7) 1000 (Utility.ag "0-a-1")
    , Utility.packageWithGraph (Utility.uuid_from_int 9) 2000 (Utility.ag "0-@7-1")
    ]


suite : Test
suite =
  describe "Computation explorer and packages"
    [ describe "selection"
        [ test "selecting a computation loads its graph into the main editor" <|
            \_ ->
              let
                bId = Utility.uuid_from_int 9
                result = U.step1 (PackageMsg bId SelectPackage) twoModels
              in
                Expect.all
                  [ \_ ->
                      U.mainView result
                      |> Maybe.map .computation
                      |> Maybe.map (Utility.ag_equals (Utility.ag "0-b-1 1-c-2"))
                      |> Maybe.withDefault (Expect.fail "no main view")
                  , \_ -> result.selectedPackage |> Expect.equal bId
                  , \_ ->
                      U.mainView result
                      |> Maybe.map .graphPackage
                      |> Expect.equal (Just (Just bId))
                  , \_ ->
                      -- the previous main view is replaced, not accumulated
                      AutoDict.member twoModels.mainGraphView result.graph_views
                      |> Expect.equal False
                  ]
                    ()
        , test "the explorer still lists both computations" <|
            \_ ->
              U.step1 (PackageMsg (Utility.uuid_from_int 9) SelectPackage) twoModels
              |> explorerPackages
              |> List.length
              |> Expect.equal 2
        ]
    , describe "creation"
        [ test "a new computation lands at the head of the explorer list" <|
            \_ ->
              let
                result =
                  U.step1 (TimeValueForPackage (Time.millisToPosix 3000)) twoModels
                newestCreated =
                  AutoDict.values result.packages
                  |> List.filter (.created >> Time.posixToMillis >> (==) 3000)
                  |> List.head
                  |> Maybe.map .packageIdentifier
              in
                Expect.all
                  [ \_ -> AutoDict.size result.packages |> Expect.equal 3
                  , \_ -> explorerPackages result |> List.head |> Expect.equal (Just newestCreated)
                  , \_ ->
                      -- …and the previous head is now second
                      explorerPackages result
                      |> List.tail
                      |> Maybe.withDefault []
                      |> List.head
                      |> Expect.equal (Just (Just (Utility.uuid_from_int 9)))
                  ]
                    ()
        ]
    , describe "deletion"
        [ test "an unreferenced computation is deleted immediately" <|
            \_ ->
              let
                bId = Utility.uuid_from_int 9
                result = U.step1 (PackageMsg bId DeletePackage) twoModels
              in
                Expect.all
                  [ \_ -> AutoDict.member bId result.packages |> Expect.equal False
                  , \_ -> AutoDict.size result.packages |> Expect.equal 1
                  , \_ -> explorerPackages result |> List.length |> Expect.equal 1
                  , \_ -> globalTop result |> Expect.equal Nothing
                  ]
                    ()
        , test "the computation being edited has no delete capability" <|
            \_ ->
              Expect.all
                [ \_ -> U.mainViewOrFail twoModels |> .can |> .deletePackage |> Expect.equal False
                , \_ ->
                    -- every explorer entry may be deleted, however
                    List.filterMap (\id -> AutoDict.get id twoModels.graph_views) twoModels.computationsExplorer
                    |> List.map (.can >> .deletePackage)
                    |> List.all identity
                    |> Expect.equal True
                ]
                  twoModels
        , test "a referenced computation opens a confirmation interaction first" <|
            \_ ->
              let
                aId = Utility.uuid_from_int 7
                result = U.step1 (PackageMsg aId DeletePackage) linkedModel
              in
                case globalTop result of
                  Just (DeletingPackage toDelete _) ->
                    Expect.all
                      [ \_ -> toDelete |> Expect.equal aId
                      , \_ -> AutoDict.size result.packages |> Expect.equal 2 -- nothing deleted yet
                      , \_ ->
                          -- the confirmation screen permits escape (and only escape-ish things)
                          result.can
                          |> \can -> ( can.escape, can.dragSplitter, can.createNewPackage )
                          |> Expect.equal ( True, False, False )
                      ]
                      ()

                  _ ->
                    Expect.fail "expected a DeletingPackage interaction on the global stack"
        , test "confirming the deletion removes the package but keeps the affected ones" <|
            \_ ->
              let
                result =
                  U.step [ PackageMsg (Utility.uuid_from_int 7) DeletePackage, Confirm ] linkedModel
              in
                Expect.all
                  [ \_ -> AutoDict.member (Utility.uuid_from_int 7) result.packages |> Expect.equal False
                  , \_ -> AutoDict.size result.packages |> Expect.equal 1
                  , \_ ->
                      -- the referencing computation survives
                      AutoDict.member (Utility.uuid_from_int 9) result.packages
                      |> Expect.equal True
                  , \_ -> globalTop result |> Expect.equal Nothing
                  ]
                    ()
        , test "escape cancels the deletion and cleans up the warning views" <|
            \_ ->
              let
                baselineViews = AutoDict.size linkedModel.graph_views
                result =
                  U.step [ PackageMsg (Utility.uuid_from_int 7) DeletePackage, Escape ] linkedModel
              in
                Expect.all
                  [ \_ -> globalTop result |> Expect.equal Nothing
                  , \_ -> AutoDict.size result.packages |> Expect.equal 2
                  , \_ -> AutoDict.size result.graph_views |> Expect.equal baselineViews
                  ]
                    ()
        ]
    ]
