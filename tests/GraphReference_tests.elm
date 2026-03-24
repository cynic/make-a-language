module GraphReference_tests exposing (suite)
{-| Batch 19: the connection editor's graph-reference flow.

    «`» enters `GraphReferenceSearch` mode, typing filters the reference
    list by description (packages without a description always remain),
    «`» returns to character input; choosing a reference toggles the
    `ViaGraphReference` transition in and out of the connection, and
    confirming commits it into the graph edge.
-}
import AutoDict
import AutoSet
import Data exposing
  ( AcceptVia(..)
  , ConnectionTarget(..)
  , GraphViewMsg(..)
  , InteractionState(..)
  , Model
  , Msg(..)
  , PackageMsg(..)
  , QuickInputMode(..)
  , Transition
  , transitionToString
  )
import Dict
import Expect
import Graph
import Graph exposing (NodeId)
import GraphEditor
import IntDict
import Queries as Q
import Test exposing (..)
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


refA : Uuid
refA =
  Utility.uuid_from_int 7


refB : Uuid
refB =
  Utility.uuid_from_int 9


{-| Two packages with descriptions ("Alpha graph", "Beta graph"); the
    newer one (B) is selected, so the main view shows `0-b-1`.
-}
refModel : Model
refModel =
  U.initWithPackages
    [ Utility.packageWithGraph refA 1000 (Utility.ag "0-a-1")
    , Utility.packageWithGraph refB 2000 (Utility.ag "0-b-1")
    ]
  |> U.step
      [ PackageMsg refA (UpdateComputationDescription "Alpha graph")
      , PackageMsg refB (UpdateComputationDescription "Beta graph")
      ]


{-| Same, but package B has no description.
-}
partiallyDescribedModel : Model
partiallyDescribedModel =
  U.initWithPackages
    [ Utility.packageWithGraph refA 1000 (Utility.ag "0-a-1")
    , Utility.packageWithGraph refB 2000 (Utility.ag "0-b-1")
    ]
  |> U.step1 (PackageMsg refA (UpdateComputationDescription "Alpha graph"))


openEditor : Msg
openEditor =
  EditConnection { x = 50, y = 50 } refModel.mainGraphView 0 1 (Utility.mkConn "b")


topOf : Model -> Maybe InteractionState
topOf m =
  Q.peekInteraction (Just m.mainGraphView) m.interactionsDict


propsOf : Model -> Maybe { shownList : List Uuid, referenceList : List Uuid, editingMode : QuickInputMode }
propsOf m =
  topOf m
  |> Maybe.map
      (\i ->
        case i of
          EditingConnection _ props ->
            { shownList = props.shownList, referenceList = props.referenceList, editingMode = props.editingMode }

          _ ->
            { shownList = [], referenceList = [], editingMode = CharacterInput }
      )


editedConnectionHas : Model -> Transition -> Bool
editedConnectionHas m transition =
  topOf m
  |> Maybe.map
      (\i ->
        case i of
          EditingConnection alteration _ ->
            AutoSet.member transition alteration.connection

          _ ->
            False
      )
  |> Maybe.withDefault False


{-| The package-uuid linked to a view, for the given view uuid.
-}
packageOfView : Model -> Uuid -> Maybe Uuid
packageOfView m view_uuid =
  AutoDict.get view_uuid m.graph_views
  |> Maybe.andThen .graphPackage


edgeConnectionHas : Model -> NodeId -> NodeId -> Transition -> Bool
edgeConnectionHas m src dest transition =
  U.mainView m
  |> Maybe.andThen (.computation >> .graph >> Graph.get dest)
  |> Maybe.map
      (\ctx ->
        ctx.incoming
        |> IntDict.get src
        |> Maybe.withDefault (AutoSet.empty transitionToString)
        |> AutoSet.member transition
      )
  |> Maybe.withDefault False


suite : Test
suite =
  describe "Graph references"
    [ describe "search mode"
        [ test "backtick enters reference search; backtick returns to character input" <|
            \_ ->
              let
                searching =
                  U.step [ openEditor, QuickInput "`" ] refModel
                back =
                  U.step [ openEditor, QuickInput "`", QuickInput "`" ] refModel
              in
                Expect.all
                  [ \_ -> propsOf searching |> Maybe.map .editingMode |> Expect.equal (Just (GraphReferenceSearch ""))
                  , \_ -> propsOf back |> Maybe.map .editingMode |> Expect.equal (Just CharacterInput)
                  ]
                    ()
        , test "typing filters the reference list by description" <|
            \_ ->
              let
                result =
                  U.step [ openEditor, QuickInput "`", QuickInput "alpha" ] refModel
              in
                Expect.all
                  [ \_ ->
                      propsOf result
                      |> Maybe.map (.referenceList >> List.length)
                      |> Expect.equal (Just 2)
                  , \_ ->
                      propsOf result
                      |> Maybe.andThen
                          (\props ->
                            case props.shownList of
                              [ only ] ->
                                packageOfView result only

                              _ ->
                                    Nothing
                          )
                      |> Expect.equal (Just refA)
                  ]
                    ()
        , test "packages without a description always remain listed" <|
            \_ ->
              -- "zzz" matches no description; the undescribed package B
              -- must survive the filter (Nothing → included)
              let
                result =
                  U.step
                    [ EditConnection { x = 50, y = 50 } partiallyDescribedModel.mainGraphView 0 1 (Utility.mkConn "b")
                    , QuickInput "`"
                    , QuickInput "zzz"
                    ]
                    partiallyDescribedModel
              in
                propsOf result
                |> Maybe.andThen
                    (\props ->
                      case props.shownList of
                        [ only ] ->
                          packageOfView result only

                        _ ->
                            Nothing
                    )
                |> Expect.equal (Just refB)
        ]
    , describe "choosing a reference"
        [ test "toggles the reference transition into, and out of, the connection" <|
            \_ ->
              let
                refTransition =
                  Transition False (ViaGraphReference refA)
                added =
                  U.step [ openEditor, ToggleConnectionTransition (ViaGraphReference refA) ] refModel
                removed =
                  U.step [ openEditor, ToggleConnectionTransition (ViaGraphReference refA), ToggleConnectionTransition (ViaGraphReference refA) ] refModel
              in
                Expect.all
                  [ \_ -> editedConnectionHas added refTransition |> Expect.equal True
                  , \_ -> editedConnectionHas removed refTransition |> Expect.equal False
                  ]
                    ()
        , test "confirming commits the reference into the graph edge" <|
            \_ ->
              U.step
                [ openEditor
                , ToggleConnectionTransition (ViaGraphReference refA)
                , Confirm
                ]
                refModel
              |> \m -> edgeConnectionHas m 0 1 (Transition False (ViaGraphReference refA))
              |> Expect.equal True
        ]
    , describe "reference labels"
        [ test "carry the graph-reference classes (non-final and final)" <|
            \_ ->
              Expect.equal
                ( GraphEditor.characterTransitionClass False -- control: character labels
                , GraphEditor.referenceTransitionClass False
                , GraphEditor.referenceTransitionClass True
                )
                ( [ "transition", "text", "nonfinal" ]
                , [ "transition", "graph-reference", "nonfinal" ]
                , [ "transition", "graph-reference", "final" ]
                )
        ]
    ]
