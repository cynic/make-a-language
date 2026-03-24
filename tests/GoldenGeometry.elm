module GoldenGeometry exposing
  ( straightGraph, curvedGraph, recursiveGraph
  , computed, golden, mkGraph
  )
{-| Golden `pathString` fixtures for Tier-2 geometry pinning: one straight,
    one curved, one recursive link (see `UI-Tests.md`, "Geometry
    assertions — Tier 2").

    ## Regeneration procedure

    Only when a geometry change is *intentional*:

    1. temporarily set the three values in `golden` to `""`;
    2. run `npm test` — the golden test fails and prints the actual strings;
    3. copy the three actual strings from the failure diff back into
       `golden`.

    Everything else in the suite then re-verifies against the new geometry.
-}
import AutoDict
import Data exposing (AutomatonGraph, NodeEffect(..), entity)
import Dict
import Graph
import Graph exposing (NodeId)
import GraphEditor
import Uuid exposing (Uuid)
import Utility


at : NodeId -> Float -> Float -> Data.Entity
at id x y =
  (entity id NoEffect) |> (\e -> { e | x = x, y = y })


mkGraph : List ( NodeId, Float, Float ) -> List ( NodeId, NodeId, String ) -> AutomatonGraph
mkGraph nodes edges =
  { graph =
      Graph.fromNodesAndEdges
        (List.map (\( id, x, y ) -> Graph.Node id (at id x y)) nodes)
        (List.map (\( a, b, c ) -> Graph.Edge a b (Utility.mkConn c)) edges)
  , description = Nothing
  , root = 0
  }


{-| 0 -a→ 1 : a straight, unidirectional link.
-}
straightGraph : AutomatonGraph
straightGraph =
  mkGraph [ ( 0, 0, 0 ), ( 1, 300, 0 ) ] [ ( 0, 1, "a" ) ]


{-| 0 ⇄ 1 : the (0,1) link is curved (bidirectional).
-}
curvedGraph : AutomatonGraph
curvedGraph =
  mkGraph [ ( 0, 0, 0 ), ( 1, 300, 0 ) ] [ ( 0, 1, "a" ), ( 1, 0, "b" ) ]


{-| A self-loop on the node at (100,100).
-}
recursiveGraph : AutomatonGraph
recursiveGraph =
  mkGraph [ ( 0, 100, 100 ) ] [ ( 0, 0, "a" ) ]


pathOf : AutomatonGraph -> ( NodeId, NodeId ) -> String
pathOf ag key =
  GraphEditor.linkDrawingForPackage ag (AutoDict.empty Uuid.toString)
  |> Dict.get key
  |> Maybe.map (.pathBetween >> .pathString)
  |> Maybe.withDefault "MISSING"


{-| The pathStrings as the code computes them right now.
-}
computed : { straight : String, curved : String, recursive : String }
computed =
  { straight = pathOf straightGraph ( 0, 1 )
  , curved = pathOf curvedGraph ( 0, 1 )
  , recursive = pathOf recursiveGraph ( 0, 0 )
  }


{-| The pinned golden strings — see the regeneration procedure above.
-}
golden : { straight : String, curved : String, recursive : String }
golden =
  { straight = "M 7 0 Q 150 0 282 0"
  , curved = "M 5.63830617915638 -4.148433852683079 Q 150 -110.3638323514327 285.501498396455 -10.667401335470775"
  , recursive = "M 94.26633378771314 93.1154957521913 c -14,-14 28,-14 14,0"
  }
