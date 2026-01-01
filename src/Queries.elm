module Queries exposing
    ( bounds_for
    , descriptionsForConnection
    , descriptionsForPackages
    , linkExistsInGraph
    , mostRecentInteraction
    , packagesAffectedBy
    , packagesAndRefs
    , peekInteraction
    , resolutionDict
    , unusedNodeId
    , viewsContainingPackage
    )
import Automata.Data exposing (..)
import AutoDict
import AutoSet
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Graph exposing (NodeContext, NodeId, Graph)
import IntDict
import Set
import List.Extra as List

descriptionsForPackages : PackageDict -> GraphReferenceDescriptions
descriptionsForPackages packages =
  AutoDict.toList packages
  |> List.filterMap (\(k, v) -> Maybe.combineSecond (k, v.computation.description))
  |> AutoDict.fromList Uuid.toString

linkExistsInGraph : NodeContext Entity Connection -> NodeId -> Bool
linkExistsInGraph from to =
  -- does a link exist from `from` to `to`?
  IntDict.member to from.outgoing
  -- |> Debug.log ("Checking for #" ++ String.fromInt to ++ " in #" ++ String.fromInt from.node.id ++ "'s outgoing list " ++ Debug.toString (IntDict.keys from.outgoing))

descriptionsForConnection : Connection -> PackageDict -> AutoDict.Dict String Uuid String
descriptionsForConnection connection packages =
  AutoSet.toList connection
  |> List.filterMap
    (\{via} ->
        case via of
          ViaCharacter _ ->
            Nothing
          ViaGraphReference uuid ->
            AutoDict.get uuid packages
            |> Maybe.andThen (.computation >> .description)
            |> Maybe.map (\s -> ( uuid, s ))
    )
  |> AutoDict.fromList Uuid.toString

bounds_for : List NodeId -> Graph.Graph Entity Connection -> Bounds
bounds_for nodeIds graph =
  let
    contexts =
      (Set.fromList >> Set.toList) nodeIds
      |> List.filterMap (\id -> Graph.get id graph)
      -- |> debugLog_ "[bounds_for] contexts to check" (List.map (.node >> .label))
  in
  -- Now find out: where is the bounding box for the nodes?
    case contexts of
      [] ->
        { min = { x = 0, y = 0 }, max = { x = 0, y = 0 } }
        -- |> Debug.log "Default Bounds"
      h::t ->
        List.foldl
          (\ctx best ->
            { min =
                { x = min ctx.node.label.x best.min.x
                , y = min ctx.node.label.y best.min.y
                }
            , max =
                { x = max ctx.node.label.x best.max.x
                , y = max ctx.node.label.y best.max.y
                }
            }
          )
          { min = { x = h.node.label.x, y = h.node.label.y }
          , max = { x = h.node.label.x, y = h.node.label.y }
          }
          t
          -- |> Debug.log "[bounds_for] Raw Bounds"

peekInteraction : Maybe Uuid -> InteractionsDict -> Maybe InteractionState
peekInteraction uuid interactions =
  AutoDict.get uuid interactions
  |> Maybe.map Tuple.second
  |> Maybe.andThen List.head

mostRecentInteraction : Model -> Maybe (Maybe Uuid, InteractionState)
mostRecentInteraction model =
  AutoDict.toList model.interactionsDict
  |> List.maximumBy (Tuple.second >> Tuple.first)
  |> Maybe.andThen
    (\(uuid, (_, interaction)) ->
      case interaction of
        [] -> Nothing
        h::_ -> Just (uuid, h)
    )

resolutionDict : PackageDict -> ResolutionDict
resolutionDict packages =
  AutoDict.map (\_ -> .computation) packages

viewsContainingPackage : Uuid -> Model -> List GraphView
viewsContainingPackage package_uuid m =
  AutoDict.values m.graph_views
  |> List.filter (\gv -> gv.graphPackage == Just package_uuid)

packagesAndRefs : Model -> List (Uuid, AutoSet.Set String Uuid)
packagesAndRefs {packages} =
  AutoDict.values packages
  |> List.foldl
    (\pkg acc ->
      ( pkg.packageIdentifier
      , Graph.edges pkg.computation.graph
        |> List.foldl
            (\{label} set ->
              AutoSet.foldl
                (\{via} set_ ->
                  case via of
                    ViaGraphReference ref ->
                      AutoSet.insert ref set_
                    _ ->
                      set_
                )
                set
                label
            )
            (AutoSet.empty Uuid.toString)
      ) :: acc
    )
    []

packagesAffectedBy : Uuid -> Model -> List Uuid
packagesAffectedBy uuid model =
  packagesAndRefs model
  |> List.filterMap
    (\(package_uuid, refs) ->
      if AutoSet.member uuid refs then
        Just package_uuid
      else
        Nothing
    )

unusedNodeId : AutomatonGraph -> NodeId
unusedNodeId {graph} =
  Graph.nodeIdRange graph
  |> Maybe.map (Tuple.second >> (+) 1)
  |> Maybe.withDefault 0
