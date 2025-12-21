module Queries exposing
    ( descriptionsForPackages
    , linkExistsInGraph
    , descriptionsForConnection
    )
import Automata.Data exposing (..)
import AutoDict
import AutoSet
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Graph exposing (NodeContext, NodeId)
import IntDict

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
