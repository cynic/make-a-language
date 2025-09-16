module Encoding_tests exposing (..)
import Test exposing (..)
import Utility exposing (..)
import Expect
import Automata.Data exposing (NodeEffect(..), AcceptVia(..), Entity)
import Automata.DFA exposing (encodeAutomatonGraph, decodeAutomatonGraph)
import Graph exposing (Node, Edge)
import Json.Decode as D
import Automata.Debugging exposing (debugAutomatonGraph)

-- mkNode : Int -> Graph.Node Entity
mkNode id =
  Node id <| Entity 0 0 0 0 id NoEffect

mkEdge from to chars =
  Edge from to (mkConn chars)

mkAutomatonGraph nodeIds edges root =
  { graph =
      Graph.fromNodesAndEdges
        ( nodeIds |> List.map mkNode )
        ( List.map (\(from, to, chars) -> Edge from to (mkConn dummy_uuid chars)) edges )
  , graphIdentifier = dummy_uuid
  , root = root
  }

czech root nodes edges =
  let
    v =
      mkAutomatonGraph nodes edges root
      |> debugAutomatonGraph "[czech] AutomatonGraph test"
  in
    case encodeAutomatonGraph v |> D.decodeValue decodeAutomatonGraph of
      Ok v_ -> Expect.equal v v_
      Err e -> Expect.fail (Debug.toString e)


suite : Test
suite =
  describe "Encoding & Decoding serialized automata"
    [ describe "Round-tripping"
      [ test "handles simple transitions correctly" <|
        \_ ->
          czech 0
            [0, 1, 2]
            [ (0, 1, "!av"), (0, 2, "b!vk!z"), (2, 0, "p") ]
      , test "handles single transition" <|
        \_ ->
          czech 1 [1, 2] [(1, 2, "abc")]
      , test "handles whitespace transitions" <|
        \_ ->
          czech 0
            [0, 1, 2, 3]
            [ (0, 1, "! "), (1, 2, " "), (2, 3, "p ") ]
      ]
      , test "handles graph references" <|
        \_ ->
          czech 0
            [0, 1, 2, 3]
            [ (0, 1, "!+ab9d1082-73ef-4541-958e-ec84498ee37a") -- 1 uuid
              -- 2 uuids, one final and the other nonfinal
            , (1, 2, "+13c922a2-91e9-43e5-9f6b-6c05967c846d!+ab9d1082-73ef-4541-958e-ec84498ee37a")
              -- 1 uuid and 2 characters
            , (2, 3, "+5b83a297-30f7-4708-bc78-2868e830250aab")
            ]
    ]