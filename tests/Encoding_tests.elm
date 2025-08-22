module Encoding_tests exposing (..)
import Test exposing (..)
import Utility exposing (..)
import Expect
import Automata.Data exposing (mkAG_input, NodeEffect(..), AcceptVia(..), Entity)
import Automata.DFA exposing (serializeAutomatonGraph, mkConn, deserializeAutomatonGraph)
import Graph exposing (Node, Edge)
import Json.Decode as D

-- mkNode : Int -> Graph.Node Entity
mkNode id =
  Node id <| Entity 0 0 0 0 id NoEffect

mkEdge from to chars =
  Edge from to (mkConn chars)

mkAutomatonGraph nodeIds edges root =
  { graph =
      Graph.fromNodesAndEdges
        ( nodeIds |> List.map mkNode )
        ( List.map (\(from, to, chars) -> Edge from to (mkConn chars)) edges )
  , maxId = List.maximum nodeIds |> Maybe.withDefault 0
  , root = root
  }

czech root nodes edges =
  let
    v = mkAutomatonGraph nodes edges root
  in
    case serializeAutomatonGraph v |> D.decodeValue deserializeAutomatonGraph of
      Ok v_ -> Expect.equal v v_
      Err e -> Expect.fail (Debug.toString e)


suite : Test
suite =
  describe "Encoding & Decoding serialized automata"
    [ describe "Decoding: serialized string to AutomatonGraph"
      [ test "handles a + character" <|
        \_ ->
          Expect.equal [(1, "+", 2)] (mkAG_input "1-++-2")
      ]
    , describe "Round-tripping"
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
    -- , test "handles references" <|
    --   \_ ->
    --     Expect.equal

    ]