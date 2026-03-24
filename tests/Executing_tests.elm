module Executing_tests exposing (suite, execModel)
{-| Batch 7: executing a computation, at the update level.

    Execution is driven by the selected test's input.  `Step`/`Run`/`StepBack`
    push or replace the global `Executing` interaction, create a fresh
    execution view, highlight the executed links, centre the view on them,
    and lowlight the rest (links out of the *current* node — the "next
    links" — are deliberately neither).

    Note: `DFA.load` renumbers node ids, so these tests assert structural
    invariants (taken path from the `ExecutionData`, out-edges of the
    current node, …) rather than hardcoded ids.
-}
import AutoDict
import Data exposing
  ( AutomatonGraph
  , ExecutionResult(..)
  , GraphPackage
  , InteractionState(..)
  , Model
  , Msg(..)
  , NodeEffect(..)
  , TestExpectation(..)
  , entity
  )
import Dict
import Expect
import Graph
import Graph exposing (NodeId)
import Main
import Queries as Q
import Set exposing (Set)
import Test exposing (..)
import Time
import UILibrary as U
import Uuid exposing (Uuid)
import Utility


isExecuting : InteractionState -> Bool
isExecuting i =
  case i of
    Executing _ _ ->
      True

    _ ->
      False


{-| 0 -a→ 1 -!b→ 2 and 1 -c→ 3: stepping "ab" highlights 0→1 then 1→2,
    lowlights the alternative 1→3 on the second step.
-}
execGraph : AutomatonGraph
execGraph =
  let
    at id x y =
      (entity id NoEffect) |> (\e -> { e | x = x, y = y })
  in
    { graph =
        Graph.fromNodesAndEdges
          [ Graph.Node 0 (at 0 0 0)
          , Graph.Node 1 (at 1 300 0)
          , Graph.Node 2 (at 2 600 0)
          , Graph.Node 3 (at 3 600 400)
          ]
          [ Graph.Edge 0 1 (Utility.mkConn "a")
          , Graph.Edge 1 2 (Utility.mkConn "!b")
          , Graph.Edge 1 3 (Utility.mkConn "c")
            -- without the outgoing d, node 3 would be equivalent to node 2
            -- and DFA minimisation would merge them, leaving no alternative
            -- branch to lowlight.
          , Graph.Edge 3 2 (Utility.mkConn "d")
          ]
    , description = Nothing
    , root = 0
    }


testKey : Uuid
testKey =
  Utility.uuid_from_int 8


execPackage : GraphPackage
execPackage =
  { computation = execGraph
  , packageIdentifier = Utility.uuid_from_int 7
  , created = Time.millisToPosix 1000
  , currentTestKey = testKey
  , tests =
      AutoDict.singleton Uuid.toString testKey
        { input = "ab"
        , expectation = ExpectAccepted
        , result = InternalError "recomputed on decode"
        }
  }


execModel : Model
execModel =
  U.initWithPackages [ execPackage ]
  |> U.withGraph execGraph


globalTop : Model -> Maybe InteractionState
globalTop m =
  Q.peekInteraction Nothing m.interactionsDict


headExecution : Model -> Maybe Data.ExecutionData
headExecution m =
  globalTop m
  |> Maybe.map
      (\i ->
        case i of
          Executing (h :: _) _ ->
            Just h

          _ ->
            Nothing
      )
  |> Maybe.withDefault Nothing


highlighted : Model -> Set ( NodeId, NodeId )
highlighted m =
  U.mainViewOrFail m |> .drawingData |> .highlighted_links


lowlighted : Model -> Set ( NodeId, NodeId )
lowlighted m =
  U.mainViewOrFail m |> .drawingData |> .lowlighted_links


drawnLinks : Model -> List ( NodeId, NodeId )
drawnLinks m =
  U.mainViewOrFail m |> .drawingData |> .link_drawing |> Dict.keys


{-| Edges out of the given node, as drawn.
-}
edgesFrom : Model -> NodeId -> List ( NodeId, NodeId )
edgesFrom m node =
  drawnLinks m
  |> List.filter (\( s, _ ) -> s == node)


nodeCoords : Model -> NodeId -> Maybe ( Float, Float )
nodeCoords m id =
  U.mainView m
  |> Maybe.andThen (.computation >> .graph >> Graph.get id)
  |> Maybe.map (\ctx -> ( ctx.node.label.x, ctx.node.label.y ))


suite : Test
suite =
  describe "Executing a computation"
    [ describe "Step"
        [ test "starts execution on a fresh view with the global Executing interaction" <|
            \_ ->
              let
                result = U.step1 Step execModel
              in
                Expect.all
                  [ \_ -> result.mainGraphView /= execModel.mainGraphView |> Expect.equal True
                  , \_ ->
                      globalTop result
                      |> Maybe.map isExecuting
                      |> Expect.equal (Just True)
                  , \_ ->
                      result.can
                      |> \can -> ( can.escape, can.loadTestInput, can.createTestInput )
                      |> Expect.equal ( True, True, True )
                  ]
                    ()
        , test "highlights exactly the link that was taken" <|
            \_ ->
              let
                result = U.step1 Step execModel
              in
                case headExecution result of
                  Just h ->
                    Expect.all
                      [ \_ -> Set.size (highlighted result) |> Expect.equal 1
                        -- the highlight is exactly what the app traces from
                        -- the ExecutionData…
                      , \_ -> highlighted result |> Expect.equal (Set.fromList (Main.executing_edges h))
                        -- …and it is a real link that ends at the current node
                      , \_ ->
                          highlighted result
                          |> Set.toList
                          |> List.map (\e -> List.member e (drawnLinks result))
                          |> Expect.equal [ True ]
                      , \_ ->
                          highlighted result
                          |> Set.toList
                          |> List.map Tuple.second
                          |> Expect.equal [ h.currentNode ]
                      ]
                      ()

                  _ ->
                    Expect.fail "expected execution to have started"
        , test "next-links (out of the current node) are neither highlighted nor lowlighted" <|
            \_ ->
              let
                result = U.step1 Step execModel
                covered = Set.union (highlighted result) (lowlighted result)
              in
                case headExecution result of
                  Just h ->
                    Expect.all
                      [ \_ ->
                          edgesFrom result h.currentNode
                          |> List.map (\e -> Set.member e covered)
                          |> List.all (\b -> b == False)
                          |> Expect.equal True
                      , \_ ->
                          -- …while the rest of the graph *is* covered
                          drawnLinks result
                          |> List.filter (\e -> not (Set.member e covered))
                          |> List.length
                          |> Expect.equal (List.length (edgesFrom result h.currentNode))
                      ]
                        ()

                  _ ->
                    Expect.fail "expected execution to have started"
        , test "lowlight covers everything that is neither on-path nor a next-link" <|
            \_ ->
              let
                result = U.step1 Step execModel
                hl = highlighted result
                ll = lowlighted result
                covered = Set.union hl ll
              in
                case headExecution result of
                  Just h ->
                    -- every drawn link is in exactly one of: highlighted,
                    -- lowlighted, or an outgoing link of the current node
                    -- (the "next links")
                    Expect.all
                      [ \_ ->
                          drawnLinks result
                          |> List.filter
                              (\e ->
                                not (Set.member e covered)
                                  && Tuple.first e /= h.currentNode
                              )
                          |> Expect.equal []
                      , \_ ->
                          ll
                          |> Set.toList
                          |> List.map (\( s, _ ) -> s /= h.currentNode)
                          |> List.all identity
                          |> Expect.equal True
                      , \_ ->
                          Set.size ll
                          |> Expect.equal
                              (List.length (drawnLinks result)
                                - Set.size hl
                                - List.length (edgesFrom result h.currentNode)
                              )
                      ]
                        ()

                  _ ->
                    Expect.fail "expected execution to have started"
        , test "stepping again highlights the whole path; the final view shows only the executed path" <|
            \_ ->
              let
                result = U.step [ Step, Step ] execModel
                hl = highlighted result
                flattened =
                  Set.toList hl |> List.concatMap (\( a, b ) -> [ a, b ])
                intermediates =
                  Set.fromList flattened
                  |> Set.toList
                  |> List.filter (\n -> List.length (List.filter ((==) n) flattened) == 2)
              in
                case ( Set.size hl, intermediates ) of
                  ( 2, [ _ ] ) ->
                    Expect.all
                      [ \_ -> Set.size (lowlighted result) |> Expect.equal 0
                        -- the final-step view contains exactly the executed path
                      , \_ ->
                          Set.fromList (drawnLinks result)
                          |> Expect.equal hl
                      , \_ ->
                          -- the path ends at the current node: the unique
                          -- node that is a destination but never a source
                          let
                            edgeList = Set.toList hl
                            endNode =
                              flattened
                              |> List.filter
                                  (\n ->
                                    List.member n (List.map Tuple.second edgeList)
                                      && not (List.member n (List.map Tuple.first edgeList))
                                  )
                              |> List.head
                          in
                            headExecution result
                            |> Maybe.map .currentNode
                            |> Expect.equal endNode
                      ]
                        ()

                  other ->
                    Expect.fail ("expected a two-edge path with one intermediate node, got " ++ Debug.toString other)
        , test "centres the view on the executed links" <|
            \_ ->
              let
                result = U.step1 Step execModel
                guest = U.mainViewOrFail result |> .guest
                executedIds =
                  highlighted result
                  |> Set.toList
                  |> List.concatMap (\( a, b ) -> [ a, b ])
              in
                case List.filterMap (nodeCoords result) executedIds of
                  [] ->
                    Expect.fail "expected the executed nodes to be present in the new view"

                  coords ->
                    let
                      xs = List.map Tuple.first coords
                      xmin = List.minimum xs |> Maybe.withDefault 0
                      xmax = List.maximum xs |> Maybe.withDefault 0
                      expectedCentre = (xmin + xmax) / 2
                    in
                      Expect.all
                        [ \_ ->
                            Expect.within (Expect.Absolute 0.000001)
                              (guest.x + guest.w / 2)
                              expectedCentre
                        , \_ ->
                            -- both executed nodes are inside the viewport
                            ( guest.x <= xmin + 0.000001 && guest.x + guest.w >= xmax - 0.000001 )
                              |> Expect.equal True
                        ]
                          ()
        , test "StepBack returns to the previous step" <|
            \_ ->
              -- Note: node ids are renumbered per execution, so this asserts
              -- the structural shape of "one step back" rather than comparing
              -- against the highlight of a separately-run stepping sequence.
              let
                result = U.step [ Step, Step, StepBack ] execModel
              in
                case headExecution result of
                  Just h ->
                    Expect.all
                      [ \_ ->
                          headExecution result
                          |> Maybe.map (.transitions >> List.length)
                          |> Expect.equal (Just 1)
                      , \_ -> Set.size (highlighted result) |> Expect.equal 1
                      , \_ ->
                          -- the remaining highlight is a drawn link starting
                          -- at the execution root and ending at the current node
                          highlighted result
                          |> Set.toList
                          |> Expect.equal
                              (drawnLinks result
                                |> List.filter
                                    (\e ->
                                      Tuple.first e == h.computation.root
                                        && Tuple.second e == h.currentNode
                                    )
                              )
                      ]
                        ()

                  _ ->
                    Expect.fail "expected execution to continue after StepBack"
        ]
    , describe "Run"
        [ test "executes to the end and accepts" <|
            \_ ->
              U.step1 Run execModel
              |> headExecution
              |> \h -> ( Maybe.map (.remainingData >> List.isEmpty) h, Maybe.andThen .finalResult h )
              |> Expect.equal ( Just True, Just Accepted )
        ]
    , describe "ResetComputation"
        [ test "ends execution and restores idle capabilities" <|
            \_ ->
              U.step [ Step, ResetComputation ] execModel
              |> \m ->
                  Expect.all
                    [ \_ -> globalTop m |> Expect.equal Nothing
                    , \_ -> m.can.escape |> Expect.equal False
                    ]
                      ()
        ]
    ]
